{-# LANGUAGE NoStrict #-}

-- | Control flow operations for the compiled Nix runtime.
--
-- These functions implement function application, pattern validation,
-- control flow (assert, throw, abort), and path resolution.
-- They are called by generated GHC Core code and use Haskell exceptions
-- (NixError) for error propagation.
--
-- Design principles:
-- * Match Nix semantics exactly
-- * Throw NixError on type mismatches
-- * INLINE aggressively - these are hot paths
-- * No thunk management - GHC handles that
module Nix.Compile.Primops.Control
  ( -- * Function application
    nixApply
    -- * Pattern set validation
  , nixCheckClosedPattern
    -- * Control flow
  , nixAssert
  , nixThrow
  , nixAbort
    -- * Path resolution
  , resolveEnvPath
  , nixResolveEnvPath
  ) where

import Relude hiding (empty)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import System.Directory (doesPathExist)
import System.IO.Unsafe (unsafePerformIO)
import Nix.Types.VarName (VarName, varNameText)
import Nix.Types.Path (Path(..))
import qualified Nix.Types.Path as Path
import Nix.Compile.Value
import Nix.Compile.Primops.Coerce (expectBool, expectFunction, expectString)

-- * Function application

-- | Apply a function to an argument.
nixApply :: NixValue -> NixValue -> NixValue
nixApply fn arg = expectFunction fn arg
{-# INLINE nixApply #-}

-- * Pattern set validation

-- | Check that a closed pattern set (without ...) receives only expected arguments.
-- Takes the argument attribute set and a list of expected parameter names.
-- Throws ThrownError if any unexpected arguments are present.
nixCheckClosedPattern :: NixAttrs -> [VarName] -> ()
nixCheckClosedPattern argAttrs expectedNames =
  let expectedSet = HS.fromList expectedNames
      actualKeys = attrKeys argAttrs
      extraKeys = filter (\k -> not (HS.member k expectedSet)) actualKeys
  in case extraKeys of
    [] -> ()
    (first : _) ->
      let nameText = varNameText first
      in throwNixError $ ThrownError $
           "'" <> nameText <> "' is an unexpected argument"
{-# INLINE nixCheckClosedPattern #-}

-- * Control flow

-- | Assert. Evaluates body if condition is true, throws otherwise.
nixAssert :: NixValue -> NixValue -> NixValue
nixAssert cond body
  | expectBool cond = body
  | otherwise = throwNixError AssertionFailed
{-# INLINE nixAssert #-}

-- | Throw an error with a message.
nixThrow :: NixValue -> NixValue
nixThrow v =
  let (msg, _) = expectString v
  in throwNixError $ ThrownError msg

-- | Abort evaluation with a message. Like throw but for fatal errors.
nixAbort :: NixValue -> NixValue
nixAbort v =
  let (msg, _) = expectString v
  in throwNixError $ AbortError msg

-- * Path resolution

-- | Resolve an environment path using NIX_PATH.
-- Searches NIX_PATH entries in order, looking for a matching prefix or
-- a directory containing the requested name.
resolveEnvPath :: NixPath -> Path -> IO (Either NixError Path)
resolveEnvPath (NixPath entries) searchName = go entries
  where
    searchText = toText searchName

    go [] = pure $ Left $ EnvPathNotFound searchName
    go (NixPathPrefix prefix (Path fp) : rest)
      | searchText == prefix = do
          -- Exact match: <nixpkgs> matches nixpkgs=/some/path
          exists <- doesPathExist fp
          if exists then pure (Right (Path fp)) else go rest
      | T.isPrefixOf (prefix <> "/") searchText = do
          -- Prefix match: <nixpkgs/lib> matches nixpkgs=/some/path
          let subPath = T.drop (T.length prefix + 1) searchText
              fullPath@(Path fullFp) = Path fp Path.</> fromString (toString subPath)
          exists <- doesPathExist fullFp
          if exists then pure (Right fullPath) else go rest
      | otherwise = go rest
    go (NixPathPlain (Path dirFp) : rest) = do
      -- Plain path: look for a subdirectory matching the search name
      let fullPath@(Path fullFp) = Path dirFp Path.</> fromString (toString searchText)
      exists <- doesPathExist fullFp
      if exists then pure (Right fullPath) else go rest

-- | Resolve an environment path, returning a NixValue.
-- Uses unsafePerformIO to match the pattern of other builtins like tryEval.
-- Takes NixEnv to extract the NIX_PATH from the environment.
nixResolveEnvPath :: NixEnv -> VarName -> NixValue
nixResolveEnvPath env name = unsafePerformIO $ do
  let searchPath = fromString $ toString $ varNameText name
  result <- resolveEnvPath (envNixPath env) searchPath
  pure $ case result of
    Right path -> VPath path
    Left err -> throwNixError err
{-# NOINLINE nixResolveEnvPath #-}
