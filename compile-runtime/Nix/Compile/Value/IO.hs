{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoStrict #-}

-- | IO handler types for the compiled Nix runtime.
--
-- This module defines the interface for IO operations that the runtime needs
-- to perform during evaluation. The actual implementations are provided by
-- the driver (CLI or library) before evaluation begins.
--
-- The design separates the interface (this module) from the global state
-- management (Nix.Compile.Runtime) to keep concerns clean and allow different
-- implementations for testing, sandboxing, etc.
--
-- IO operations covered:
-- - File reading and directory listing
-- - Path existence checking
-- - Environment variable access
-- - Import handling (parsing and evaluating Nix files)
--
-- Future extensions (Phase 4) will add:
-- - Derivation building
-- - Store path management
-- - URL fetching
module Nix.Compile.Value.IO
  ( -- * File type enumeration
    FileType(..)
    -- * IO handlers record
  , IOHandlers(..)
  ) where

import Relude
import Nix.Types.Path (Path)

-- Import NixValue qualified to avoid circular dependency issues.
-- The actual type is defined in Nix.Compile.Value.
import Nix.Compile.Value (NixValue)

-- | File type enumeration for readDir results.
--
-- Matches Nix's file type representation in the builtins.readDir output.
-- Each entry in the returned attrset maps filename to file type string.
data FileType
  = Regular
    -- ^ Regular file ("regular" in Nix)
  | Directory
    -- ^ Directory ("directory" in Nix)
  | Symlink
    -- ^ Symbolic link ("symlink" in Nix)
  | Unknown
    -- ^ Unknown file type ("unknown" in Nix)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | IO operation handlers that the runtime uses to perform effects.
--
-- These handlers are set by the driver before evaluation begins. The runtime
-- calls these through the global state managed by Nix.Compile.Runtime.
--
-- Design rationale:
-- - Using a record of functions allows different implementations for different
--   contexts (CLI vs library, sandboxed vs unrestricted, testing vs production)
-- - Handlers return IO to allow real filesystem and network operations
-- - The import handler takes a Path and returns NixValue because import needs
--   to parse and evaluate the file, which the driver coordinates
--
-- Note: Store operations (derivation building, path adding, URL fetching) will
-- be added in Phase 4 when store integration is implemented.
data IOHandlers = IOHandlers
  { handleImport :: Path -> IO NixValue
    -- ^ Import a Nix file, parsing and evaluating it.
    -- The driver is responsible for:
    -- 1. Reading the file
    -- 2. Parsing it
    -- 3. Compiling/evaluating it
    -- 4. Caching the result
    -- The import cache is managed separately in Nix.Compile.Runtime.

  , handleReadFile :: Path -> IO Text
    -- ^ Read the contents of a file as text (builtins.readFile).
    -- Should throw an IO exception if the file doesn't exist or can't be read.

  , handleReadDir :: Path -> IO [(Text, FileType)]
    -- ^ List directory contents with file types (builtins.readDir).
    -- Returns a list of (filename, filetype) pairs.
    -- Should throw an IO exception if the path is not a directory.

  , handlePathExists :: Path -> IO Bool
    -- ^ Check if a path exists (builtins.pathExists).
    -- Returns True if the path exists (file, directory, or symlink).

  , handleGetEnv :: Text -> IO (Maybe Text)
    -- ^ Get an environment variable (builtins.getEnv).
    -- Returns Nothing if the variable is not set.
    -- Note: builtins.getEnv returns "" for unset variables, but the handler
    -- returns Maybe so the caller can distinguish unset from empty.

  -- Store operations (to be extended in Phase 4)
  -- , handleDerivation  :: Derivation -> IO NixValue
  -- , handleAddToStore  :: Text -> ByteString -> IO StorePath
  -- , handleFetchUrl    :: Text -> Maybe Text -> IO StorePath
  }
