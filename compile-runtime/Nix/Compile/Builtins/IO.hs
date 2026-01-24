{-# LANGUAGE NoStrict #-}

-- | IO builtin implementations for the compiled Nix runtime.
--
-- These wrap the IO primops as VBuiltin values that can be included
-- in the builtins attribute set.
--
-- Note: Many of these are stubs awaiting proper IO primop implementation.
-- The import builtin is special because it needs NixEnv to resolve paths.
module Nix.Compile.Builtins.IO
  ( -- * File builtins
    builtinImport
  , builtinImportGlobal
  , builtinReadFile
  , builtinReadDir
  , builtinPathExists
    -- * Environment builtins
  , builtinGetEnv
    -- * Store builtins (stubs)
  , builtinDerivation
  , builtinToFile
  , builtinFilterSource
  , builtinPath
    -- * Fetch builtins (stubs)
  , builtinFetchurl
  , builtinFetchTarball
  , builtinFetchGit
  ) where

import Relude hiding (lookupEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesPathExist, doesFileExist, doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Nix.Types.Path (Path(..))
import Nix.Types.VarName (mkVarName)
import Nix.Compile.Value
import qualified Nix.Compile.Primops.IO as PrimopsIO

-- * IO Primops (inline stubs)
--
-- These will eventually be moved to a dedicated Primops/IO.hs module.
-- For now, they are implemented inline.

-- | Read a file's contents as a string.
-- Uses unsafePerformIO to match the lazy evaluation model.
nixReadFile :: Path -> NixValue
nixReadFile (Path fp) = unsafePerformIO $ do
  exists <- doesFileExist fp
  if exists
    then do
      contents <- TIO.readFile fp
      -- Strip final newline if present (Nix semantics)
      let stripped = if T.isSuffixOf "\n" contents
                     then T.dropEnd 1 contents
                     else contents
      pure $ VString stripped emptyContext
    else throwNixError $ ThrownError $ "file '" <> toText fp <> "' does not exist"
{-# NOINLINE nixReadFile #-}

-- | Read a directory listing.
-- Returns an attrset where keys are filenames and values are file types.
nixReadDir :: Path -> NixValue
nixReadDir (Path fp) = unsafePerformIO $ do
  exists <- doesDirectoryExist fp
  if exists
    then do
      entries <- listDirectory fp
      pairs <- forM entries $ \name -> do
        let fullPath = fp <> "/" <> name
        isDir <- doesDirectoryExist fullPath
        isFile <- doesFileExist fullPath
        let typeStr = if isDir then "directory"
                      else if isFile then "regular"
                      else "unknown"
        pure (mkVarName (toText name), VString typeStr emptyContext)
      pure $ VAttrs $ attrsFromList pairs
    else throwNixError $ ThrownError $ "directory '" <> toText fp <> "' does not exist"
{-# NOINLINE nixReadDir #-}

-- | Check if a path exists.
nixPathExists :: Path -> NixValue
nixPathExists (Path fp) = unsafePerformIO $ do
  exists <- doesPathExist fp
  pure $ VBool exists
{-# NOINLINE nixPathExists #-}

-- | Get an environment variable.
-- Returns empty string if not set (Nix semantics).
nixGetEnv :: Text -> NixValue
nixGetEnv name = unsafePerformIO $ do
  result <- lookupEnv (toString name)
  pure $ VString (maybe "" toText result) emptyContext
{-# NOINLINE nixGetEnv #-}

-- | Stub: Create a derivation.
-- Derivations require interaction with the Nix daemon and store.
nixDerivation :: NixValue -> NixValue
nixDerivation _args =
  throwNixError $ ThrownError
    "builtins.derivation: store operations not yet implemented"
{-# NOINLINE nixDerivation #-}

-- | Stub: Create a file in the store.
nixToFile :: NixValue -> NixValue -> NixValue
nixToFile _nameVal _contentsVal =
  throwNixError $ ThrownError
    "builtins.toFile: store operations not yet implemented"
{-# NOINLINE nixToFile #-}

-- | Stub: Filter source files.
nixFilterSource :: NixValue -> NixValue -> NixValue
nixFilterSource _filterFn _pathVal =
  throwNixError $ ThrownError
    "builtins.filterSource: store operations not yet implemented"
{-# NOINLINE nixFilterSource #-}

-- | Stub: Add a path to the store.
nixAddPath :: NixValue -> NixValue
nixAddPath _pathVal =
  throwNixError $ ThrownError
    "builtins.path: store operations not yet implemented"
{-# NOINLINE nixAddPath #-}

-- | Stub: Fetch a URL.
nixFetchUrl :: NixValue -> NixValue
nixFetchUrl _args =
  throwNixError $ ThrownError
    "builtins.fetchurl: network operations not yet implemented"
{-# NOINLINE nixFetchUrl #-}

-- | Stub: Fetch and extract a tarball.
nixFetchTarball :: NixValue -> NixValue
nixFetchTarball _args =
  throwNixError $ ThrownError
    "builtins.fetchTarball: network operations not yet implemented"
{-# NOINLINE nixFetchTarball #-}

-- | Stub: Fetch a git repository.
nixFetchGit :: NixValue -> NixValue
nixFetchGit _args =
  throwNixError $ ThrownError
    "builtins.fetchGit: network operations not yet implemented"
{-# NOINLINE nixFetchGit #-}

-- * Builtin wrappers

-- | Helper to extract a path from a NixValue.
extractPath :: NixValue -> Path
extractPath (VPath p) = p
extractPath (VString s _) = fromString (toString s)
extractPath v = throwNixError $ TypeError "a path" (valueTypeName v)
{-# INLINE extractPath #-}

-- | builtins.import - Import a Nix file.
-- This is a two-argument builtin: import takes NixEnv implicitly and path explicitly.
-- The NixEnv is captured from the evaluation context.
builtinImport :: NixEnv -> NixValue
builtinImport env = VBuiltin (mkVarName "import") $ \pathVal ->
  PrimopsIO.nixImport env (extractPath pathVal)
{-# NOINLINE builtinImport #-}

-- | Stub: import needs special handling because it requires NixEnv.
-- The compiler needs to generate code that passes the current env to this.
builtinImportGlobal :: NixValue
builtinImportGlobal = VBuiltin (mkVarName "import") $ \_ ->
  throwNixError $ ThrownError
    "import: not yet fully implemented. Use 'import ./path.nix' syntax which is compiled directly."
{-# NOINLINE builtinImportGlobal #-}

-- | builtins.readFile - Read a file as a string.
builtinReadFile :: NixValue
builtinReadFile = VBuiltin (mkVarName "readFile") $ \pathVal ->
  nixReadFile (extractPath pathVal)
{-# NOINLINE builtinReadFile #-}

-- | builtins.readDir - Read a directory listing.
builtinReadDir :: NixValue
builtinReadDir = VBuiltin (mkVarName "readDir") $ \pathVal ->
  nixReadDir (extractPath pathVal)
{-# NOINLINE builtinReadDir #-}

-- | builtins.pathExists - Check if a path exists.
builtinPathExists :: NixValue
builtinPathExists = VBuiltin (mkVarName "pathExists") $ \pathVal ->
  nixPathExists (extractPath pathVal)
{-# NOINLINE builtinPathExists #-}

-- | builtins.getEnv - Get an environment variable.
builtinGetEnv :: NixValue
builtinGetEnv = VBuiltin (mkVarName "getEnv") $ \nameVal ->
  let (name, _) = case nameVal of
        VString t ctx -> (t, ctx)
        v -> throwNixError $ TypeError "a string" (valueTypeName v)
  in nixGetEnv name
{-# NOINLINE builtinGetEnv #-}

-- | builtins.derivation - Create a derivation.
builtinDerivation :: NixValue
builtinDerivation = VBuiltin (mkVarName "derivation") nixDerivation
{-# NOINLINE builtinDerivation #-}

-- | builtins.toFile - Create a file in the store.
builtinToFile :: NixValue
builtinToFile = VBuiltin (mkVarName "toFile") $ \nameVal ->
  VBuiltin (mkVarName "toFile content") $ \contentsVal ->
    nixToFile nameVal contentsVal
{-# NOINLINE builtinToFile #-}

-- | builtins.filterSource - Filter source files.
builtinFilterSource :: NixValue
builtinFilterSource = VBuiltin (mkVarName "filterSource") $ \filterFn ->
  VBuiltin (mkVarName "filterSource path") $ \pathVal ->
    nixFilterSource filterFn pathVal
{-# NOINLINE builtinFilterSource #-}

-- | builtins.path - Add a path to the store with options.
builtinPath :: NixValue
builtinPath = VBuiltin (mkVarName "path") $ \args ->
  case args of
    VAttrs attrs ->
      -- { path, name?, filter?, recursive?, sha256? }
      case lookupAttr (mkVarName "path") attrs of
        Just p -> nixAddPath p
        Nothing -> throwNixError $ AttrMissing (mkVarName "path")
    v -> nixAddPath v  -- Simple form: just add the path
{-# NOINLINE builtinPath #-}

-- | builtins.fetchurl - Fetch a URL.
builtinFetchurl :: NixValue
builtinFetchurl = VBuiltin (mkVarName "fetchurl") nixFetchUrl
{-# NOINLINE builtinFetchurl #-}

-- | builtins.fetchTarball - Fetch and extract a tarball.
builtinFetchTarball :: NixValue
builtinFetchTarball = VBuiltin (mkVarName "fetchTarball") nixFetchTarball
{-# NOINLINE builtinFetchTarball #-}

-- | builtins.fetchGit - Fetch a git repository.
builtinFetchGit :: NixValue
builtinFetchGit = VBuiltin (mkVarName "fetchGit") nixFetchGit
{-# NOINLINE builtinFetchGit #-}
