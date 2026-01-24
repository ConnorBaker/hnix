{-# LANGUAGE NoStrict #-}

-- | IO primitive operations for the compiled Nix runtime.
--
-- These primops use unsafePerformIO to perform IO operations from pure code.
-- The import primop uses unsafeInterleaveIO for lazy thunk creation.
--
-- Design notes:
-- - All IO primops use NOINLINE to prevent GHC from floating out the
--   unsafePerformIO calls or sharing them inappropriately.
-- - Import uses unsafeInterleaveIO so the actual file IO only happens
--   when the result is forced, matching Nix's lazy import semantics.
-- - The import state uses atomic check-and-mark operations to prevent
--   TOCTOU races in cycle detection. See Runtime.hs for details.
-- - Path resolution handles both absolute and relative paths, using the
--   current file's directory as the base for relative imports.
module Nix.Compile.Primops.IO
  ( -- * Import
    nixImport
    -- * File operations
  , nixReadFile
  , nixReadDir
  , nixPathExists
    -- * Environment
  , nixGetEnv
  ) where

import Relude
import Control.Exception (try, throwIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (makeAbsolute, getCurrentDirectory)

import Nix.Types.Path (Path(..))
import qualified Nix.Types.Path as Path
import Nix.Types.VarName (mkVarName)
import Nix.Compile.Value
import Nix.Compile.Value.IO (FileType(..))
import Nix.Compile.Runtime
  ( ImportCheck(..)
  , checkAndMarkPending
  , cacheAndUnmarkPending
  , unmarkPending
  , getHandlers
  )
import qualified Nix.Compile.Value.IO as IO

-- | Import a Nix file, returning its evaluated value.
--
-- Import semantics with atomic cycle detection:
-- 1. Resolve the path relative to the current file (or CWD if no current file)
-- 2. Atomically check cache, check pending, and mark pending if needed
-- 3. On cache hit: return cached value immediately
-- 4. On already pending: throw CyclicImport error (cycle detected)
-- 5. On not started: evaluate, cache result, unmark pending
--
-- The atomic check-and-mark prevents TOCTOU races where two threads could
-- both see a cache miss and both attempt to import, with one falsely
-- detecting a cycle.
--
-- Exception safety: On error, the pending status is cleaned up before
-- rethrowing, preventing false cycle detection on retry.
nixImport :: NixEnv -> Path -> NixValue
nixImport env importPath = unsafePerformIO $ do
  let handlers = getHandlers

  -- Resolve relative path against current file
  absPath <- resolveRelativePath (envCurrentFile env) importPath

  -- Atomically check cache, check pending, and mark pending if needed
  checkResult <- checkAndMarkPending absPath

  case checkResult of
    CacheHit cached -> pure cached
    AlreadyPending  -> throwNixError $ CyclicImport absPath
    NotStarted      -> do
      -- Evaluate with explicit error handling for pending cleanup
      evalResult <- try $ IO.handleImport handlers absPath

      case evalResult of
        Left (err :: SomeException) -> do
          -- Clean up pending status on error, then rethrow
          unmarkPending absPath
          throwIO err
        Right result -> do
          -- Cache the successful result and unmark pending (atomic)
          cacheAndUnmarkPending absPath result
          pure result
{-# NOINLINE nixImport #-}

-- | Resolve a relative path against the current file's directory.
--
-- If the path is absolute, return it unchanged.
-- If relative and we have a current file, resolve relative to that file's directory.
-- If relative and no current file, resolve relative to the current working directory.
resolveRelativePath :: Maybe Path -> Path -> IO Path
resolveRelativePath mCurrentFile relPath@(Path pathStr) = do
  if isAbsolute pathStr
    then pure relPath
    else do
      baseDir <- case mCurrentFile of
        Just currentFile -> pure $ Path.takeDirectory currentFile
        Nothing -> Path <$> getCurrentDirectory
      let Path fullPath = baseDir Path.</> relPath
      Path <$> makeAbsolute fullPath
  where
    isAbsolute ('/':_) = True
    isAbsolute _ = False

-- | Read a file and return its contents as a string.
--
-- Uses unsafePerformIO to perform the IO from pure code.
-- Throws an exception if the file doesn't exist or can't be read.
--
-- The result is a Nix string with no context (readFile doesn't add
-- store path context since it just reads file contents).
nixReadFile :: Path -> NixValue
nixReadFile path = unsafePerformIO $ do
  let handlers = getHandlers
  contents <- IO.handleReadFile handlers path
  pure $ VString contents emptyContext
{-# NOINLINE nixReadFile #-}

-- | Read a directory and return its contents as an attrset.
--
-- Returns an attribute set mapping filenames to their file types:
-- - "regular" for regular files
-- - "directory" for directories
-- - "symlink" for symbolic links
-- - "unknown" for other file types
--
-- Example: readDir /etc might return
-- { "passwd" = "regular"; "hosts" = "regular"; "ssl" = "directory"; }
nixReadDir :: Path -> NixValue
nixReadDir path = unsafePerformIO $ do
  let handlers = getHandlers
  entries <- IO.handleReadDir handlers path
  let pairs = [(mkVarName name, VString (fileTypeToString ft) emptyContext)
              | (name, ft) <- entries]
  pure $ VAttrs $ attrsFromList pairs
{-# NOINLINE nixReadDir #-}

-- | Convert FileType to the string Nix uses.
fileTypeToString :: FileType -> Text
fileTypeToString Regular   = "regular"
fileTypeToString Directory = "directory"
fileTypeToString Symlink   = "symlink"
fileTypeToString Unknown   = "unknown"

-- | Check if a path exists.
--
-- Returns true if the path exists (regardless of whether it's a file,
-- directory, or symlink).
nixPathExists :: Path -> NixValue
nixPathExists path = unsafePerformIO $ do
  let handlers = getHandlers
  exists <- IO.handlePathExists handlers path
  pure $ VBool exists
{-# NOINLINE nixPathExists #-}

-- | Get an environment variable, returning empty string if not set.
--
-- This matches Nix's builtins.getEnv semantics where an unset variable
-- returns an empty string rather than throwing or returning null.
nixGetEnv :: Text -> NixValue
nixGetEnv varName = unsafePerformIO $ do
  let handlers = getHandlers
  mVal <- IO.handleGetEnv handlers varName
  pure $ VString (fromMaybe "" mVal) emptyContext
{-# NOINLINE nixGetEnv #-}
