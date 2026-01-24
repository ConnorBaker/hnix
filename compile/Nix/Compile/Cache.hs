{-# LANGUAGE RecordWildCards #-}

-- | Persistent cache for compiled Nix expressions.
--
-- This module provides caching of:
-- 1. Parsed Nix ASTs (to avoid re-parsing unchanged files)
-- 2. Compiled GHC Core expressions (to avoid re-compilation)
--
-- The cache is keyed by:
--
-- 1. A hash of the hnix executable path, which implicitly captures:
--    - GHC version
--    - HNix source code version
--    - All transitive dependencies (via Nix closure)
--
-- 2. A SHA256 hash of the source file content
--
-- Cache structure:
--
-- @
-- $XDG_CACHE_HOME/hnix/compile/<hnix-path-hash>/ast/<source-hash>.cbor
-- $XDG_CACHE_HOME/hnix/compile/<hnix-path-hash>/core/<source-hash>.core
-- @
--
-- The AST cache uses CBOR serialization (matching src/Nix/Cache.hs).
-- The Core cache uses GHC's interface file serialization infrastructure.
-- Both use atomic writes for safety.
module Nix.Compile.Cache
  ( -- * Types
    CacheConfig(..)
    -- * Initialization
  , initCacheConfig
    -- * File hashing
  , hashSourceFile
  , hashSourceFileWithPath
    -- * AST cache operations
  , readAstCache
  , writeAstCache
    -- * Core cache operations
  , readCoreCache
  , writeCoreCache
    -- * Utilities
  , getCachePath
  , getCoreCachePath
  ) where

import Relude hiding (readFile)
import Control.Exception (catch)
import qualified Codec.Serialise as S
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteArray (convert)
import System.IO (hClose, hPutStrLn)
import System.Directory
  ( XdgDirectory(XdgCache)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  , renameFile
  )
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import qualified System.IO.Temp as Temp

-- GHC imports for Core serialization
import GHC.Core (CoreExpr)
import GHC.CoreToIface (toIfaceExpr)
import GHC.Driver.Env (HscEnv(..))
import GHC.Iface.Syntax (IfaceExpr)
import GHC.Iface.Binary (putWithUserData, getWithUserData, TraceBinIFace(..), CompressionIFace(..))
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Tc.Utils.Monad (initIfaceLoad, initIfaceLcl)
import GHC.Unit.Module (mkModule, mkModuleName)
import GHC.Unit.Types (mainUnit)
import GHC.Utils.Binary
  ( openBinMem
  , writeBinMem
  , readBinMem
  )
import GHC.Utils.Outputable (text)
import Language.Haskell.Syntax.ImpExp (IsBootInterface(..))

import Nix.Expr.Types.Annotated (NExprLoc)
import Nix.Types.Path (Path(..))

-- * Types

-- | Configuration for the compile cache.
data CacheConfig = CacheConfig
  { ccBaseDir :: !FilePath
    -- ^ Base directory for cache storage (XDG cache dir + "hnix/compile")
  , ccHnixPathHash :: !ByteString
    -- ^ Hash of the hnix executable path (captures version/deps)
  }
  deriving (Eq, Show)

-- * Initialization

-- | Initialize cache configuration.
--
-- This function:
-- 1. Gets the hnix executable path via 'getExecutablePath'
-- 2. Hashes the path with SHA256 (the path itself, not contents, as
--    the Nix store path encodes all build inputs)
-- 3. Uses XDG cache directory for the base path
initCacheConfig :: IO CacheConfig
initCacheConfig = do
  -- Get the hnix executable path
  exePath <- getExecutablePath

  -- Hash the executable path (the Nix store path encodes all inputs)
  -- Using SHA256 and taking first 32 chars of hex representation
  let pathBytes = encodeUtf8 (toText exePath) :: ByteString
      digest = Hash.hash pathBytes :: Hash.Digest Hash.SHA256
      hashHex = show digest
      -- Take first 32 characters of the hex hash for reasonable path length
      hnixHash = encodeUtf8 (toText (take 32 hashHex))

  -- Get XDG cache directory
  baseDir <- getXdgDirectory XdgCache "hnix/compile"

  pure CacheConfig
    { ccBaseDir = baseDir
    , ccHnixPathHash = hnixHash
    }

-- * File hashing

-- | Hash the contents of a source file using SHA256.
--
-- Returns the raw digest bytes (32 bytes for SHA256).
-- Note: This only hashes the content. For Core caching where the compiled
-- expression includes the file path (for import resolution), use
-- 'hashSourceFileWithPath' instead.
hashSourceFile :: Path -> IO ByteString
hashSourceFile (Path filePath) = do
  contents <- BS.readFile filePath
  let digest = Hash.hash contents :: Hash.Digest Hash.SHA256
  -- Convert the digest to raw bytes
  pure (convert digest)

-- | Hash the contents of a source file along with its path using SHA256.
--
-- This is used for Core caching because the compiled Core expression
-- includes the source file path (for relative import resolution via
-- envCurrentFile). Two files with identical content but different paths
-- will produce different hashes, ensuring they get separate cache entries.
--
-- Returns the raw digest bytes (32 bytes for SHA256).
hashSourceFileWithPath :: Path -> IO ByteString
hashSourceFileWithPath (Path filePath) = do
  contents <- BS.readFile filePath
  -- Hash both the path and content together
  let pathBytes = encodeUtf8 (toText filePath) :: ByteString
      combined = pathBytes <> "\0" <> contents  -- Use null byte as separator
      digest = Hash.hash combined :: Hash.Digest Hash.SHA256
  pure (convert digest)

-- * AST Cache Operations

-- | Get the AST cache file path for a given source hash.
--
-- Path format: @<baseDir>/<hnixHash>/ast/<sourceHash>.cbor@
getCachePath :: CacheConfig -> ByteString -> FilePath
getCachePath CacheConfig{..} sourceHash =
  ccBaseDir
    </> toString (decodeUtf8 ccHnixPathHash :: Text)
    </> "ast"
    </> hashToHex sourceHash <> ".cbor"

-- | Read a cached AST if it exists and is valid.
--
-- Returns 'Nothing' if:
-- - The cache file doesn't exist
-- - The cache file is corrupted or incompatible
--
-- IO errors are caught and treated as cache misses.
readAstCache :: CacheConfig -> ByteString -> IO (Maybe NExprLoc)
readAstCache cfg sourceHash = do
  let cachePath = getCachePath cfg sourceHash
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else readAstCacheFile cachePath

-- | Attempt to read and deserialize an AST cache file.
-- Returns Nothing on any error (treated as cache miss).
-- Logs errors to stderr for debugging.
readAstCacheFile :: FilePath -> IO (Maybe NExprLoc)
readAstCacheFile cachePath =
  catch readAndDeserialize handleError
  where
    readAndDeserialize :: IO (Maybe NExprLoc)
    readAndDeserialize = do
      contents <- BSL.readFile cachePath
      case S.deserialiseOrFail contents of
        Left _err -> pure Nothing
        Right ast -> pure (Just ast)

    handleError :: SomeException -> IO (Maybe NExprLoc)
    handleError e = do
      hPutStrLn stderr $ "AST cache read error (treating as miss): " <> displayException e
      pure Nothing

-- | Write an AST to the cache.
--
-- Uses atomic write (write to temp file, then rename) for safety.
-- Creates parent directories as needed.
--
-- IO errors are logged to stderr but treated as non-fatal (cache writes are best-effort).
writeAstCache :: CacheConfig -> ByteString -> NExprLoc -> IO ()
writeAstCache cfg sourceHash ast =
  catch writeAtomically handleError
  where
    cachePath = getCachePath cfg sourceHash
    cacheDir = takeDirectory cachePath

    writeAtomically :: IO ()
    writeAtomically = do
      -- Create cache directory if needed
      createDirectoryIfMissing True cacheDir

      -- Write to a temp file in the same directory (for atomic rename)
      let serialized = S.serialise ast
      Temp.withTempFile cacheDir "ast.cbor.tmp" $ \tempPath tempHandle -> do
        BSL.hPut tempHandle serialized
        hClose tempHandle
        -- Atomic rename
        renameFile tempPath cachePath

    handleError :: SomeException -> IO ()
    handleError e = hPutStrLn stderr $ "AST cache write error (ignored): " <> displayException e

-- ============================================================================
-- Core Expression Cache
-- ============================================================================
--
-- Core expressions are serialized using GHC's interface file infrastructure.
-- This involves:
--
-- 1. Converting CoreExpr to IfaceExpr using toIfaceExpr
-- 2. Serializing IfaceExpr using GHC's Binary class via putWithUserData
-- 3. Deserializing using getWithUserData
-- 4. Converting IfaceExpr back to CoreExpr using tcIfaceExpr
--
-- The HscEnv is required for both serialization (for the NameCache and
-- symbol table setup) and deserialization (for type checking the interface
-- expressions back to Core).
--
-- Cache structure:
--
-- @
-- $XDG_CACHE_HOME/hnix/compile/<hnix-path-hash>/core/<source-hash>.core
-- @

-- | Get the Core cache file path for a given source hash.
--
-- Path format: @<baseDir>/<hnixHash>/core/<sourceHash>.core@
getCoreCachePath :: CacheConfig -> ByteString -> FilePath
getCoreCachePath CacheConfig{..} sourceHash =
  ccBaseDir
    </> toString (decodeUtf8 ccHnixPathHash :: Text)
    </> "core"
    </> hashToHex sourceHash <> ".core"

-- | Read a cached Core expression if it exists and is valid.
--
-- This function:
-- 1. Checks if the cache file exists
-- 2. Reads the binary data using GHC's Binary infrastructure
-- 3. Deserializes to IfaceExpr using getWithUserData
-- 4. Type-checks the IfaceExpr back to CoreExpr using tcIfaceExpr
--
-- Returns 'Nothing' if:
-- - The cache file doesn't exist
-- - The cache file is corrupted or incompatible
-- - Type checking the interface expression fails
--
-- IO errors are caught and treated as cache misses.
--
-- IMPORTANT: The HscEnv must have the same runtime modules loaded as when
-- the Core was originally compiled. If the runtime library has changed,
-- type checking will fail (which is the desired behavior for cache
-- invalidation).
readCoreCache :: CacheConfig -> ByteString -> HscEnv -> IO (Maybe CoreExpr)
readCoreCache cfg sourceHash hscEnv = do
  let cachePath = getCoreCachePath cfg sourceHash
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else readCoreCacheFile cachePath hscEnv

-- | Attempt to read and deserialize a Core cache file.
-- Returns Nothing on any error (treated as cache miss).
-- Logs errors to stderr for debugging.
readCoreCacheFile :: FilePath -> HscEnv -> IO (Maybe CoreExpr)
readCoreCacheFile cachePath hscEnv =
  catch readAndDeserialize handleError
  where
    readAndDeserialize :: IO (Maybe CoreExpr)
    readAndDeserialize = do
      -- Read the binary file using GHC's Binary infrastructure
      bh <- readBinMem cachePath

      -- Deserialize IfaceExpr using GHC's getWithUserData
      -- This handles the symbol table and dictionary for Names/FastStrings
      ifaceExpr <- getWithUserData (hsc_NC hscEnv) bh :: IO IfaceExpr

      -- Type-check the IfaceExpr back to CoreExpr
      -- This requires the interface type checker context
      runIfaceTc hscEnv ifaceExpr

    handleError :: SomeException -> IO (Maybe CoreExpr)
    handleError e = do
      hPutStrLn stderr $ "Core cache read error (treating as miss): " <> displayException e
      pure Nothing

-- | Run the interface typechecker to convert IfaceExpr to CoreExpr.
--
-- Uses initIfaceLoad and initIfaceLcl to set up the proper context for tcIfaceExpr.
-- Returns Nothing if type checking fails. Logs errors to stderr for debugging.
runIfaceTc :: HscEnv -> IfaceExpr -> IO (Maybe CoreExpr)
runIfaceTc hscEnv ifaceExpr =
  -- Use initIfaceLoad to initialize the interface monad context.
  -- This sets up the necessary environment for tcIfaceExpr to work,
  -- including access to the External Package State for looking up
  -- Names and types referenced in the IfaceExpr.
  catch doConversion handleError
  where
    -- Create a dummy module for the local environment
    dummyModule = mkModule mainUnit (mkModuleName "NixCache")

    doConversion :: IO (Maybe CoreExpr)
    doConversion = do
      -- initIfaceLcl bridges IfL (local) to IfG (global) context
      -- tcIfaceExpr returns IfL CoreExpr, so we need initIfaceLcl
      coreExpr <- initIfaceLoad hscEnv $
        initIfaceLcl dummyModule (text "nix cache") NotBoot $
          tcIfaceExpr ifaceExpr
      pure (Just coreExpr)

    handleError :: SomeException -> IO (Maybe CoreExpr)
    handleError e = do
      hPutStrLn stderr $ "Core cache type-check error (treating as miss): " <> displayException e
      pure Nothing

-- | Write a Core expression to the cache.
--
-- This function:
-- 1. Converts CoreExpr to IfaceExpr using toIfaceExpr
-- 2. Serializes using GHC's Binary infrastructure with putWithUserData
-- 3. Writes atomically (temp file + rename) for safety
--
-- Uses atomic write for safety. Creates parent directories as needed.
-- IO errors are logged to stderr but treated as non-fatal (cache writes are best-effort).
--
-- Note: Unlike readCoreCache, writing does NOT require HscEnv because
-- putWithUserData handles symbol table construction internally.
-- Only deserialization (getWithUserData) needs the NameCache.
writeCoreCache :: CacheConfig -> ByteString -> CoreExpr -> IO ()
writeCoreCache cfg sourceHash coreExpr =
  catch writeAtomically handleError
  where
    cachePath = getCoreCachePath cfg sourceHash
    cacheDir = takeDirectory cachePath

    writeAtomically :: IO ()
    writeAtomically = do
      -- Create cache directory if needed
      createDirectoryIfMissing True cacheDir

      -- Convert CoreExpr to IfaceExpr for serialization
      let ifaceExpr = toIfaceExpr coreExpr

      -- Serialize to binary using GHC's infrastructure
      -- We serialize to a temp file first, then rename for atomicity
      Temp.withTempFile cacheDir "core.tmp" $ \tempPath tempHandle -> do
        hClose tempHandle  -- Close the handle, we'll use writeBinMem

        -- Create BinHandle and serialize
        bh <- openBinMem (64 * 1024)  -- 64KB initial buffer

        -- putWithUserData handles Name/FastString symbol tables
        -- Use QuietBinIFace for silent serialization (no debug output)
        -- Use NormalCompression for reasonable size/speed tradeoff
        putWithUserData QuietBinIFace NormalCompression bh ifaceExpr

        -- Write the binary buffer to the temp file
        writeBinMem bh tempPath

        -- Atomic rename to final path
        renameFile tempPath cachePath

    handleError :: SomeException -> IO ()
    handleError e = hPutStrLn stderr $ "Core cache write error (ignored): " <> displayException e

-- ============================================================================
-- Internal Helpers
-- ============================================================================

-- | Convert a ByteString hash to a hex string.
hashToHex :: ByteString -> String
hashToHex bs = concatMap byteToHex (BS.unpack bs)

-- | Format a byte as two hex characters.
byteToHex :: Word8 -> String
byteToHex b =
  let (hi, lo) = b `divMod` 16
      hexChar n
        | n < 10 = chr (ord '0' + fromIntegral n)
        | otherwise = chr (ord 'a' + fromIntegral n - 10)
  in [hexChar hi, hexChar lo]
