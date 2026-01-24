{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoStrict #-}

-- | Runtime errors during Nix evaluation.
--
-- This module defines the error types thrown by the compiled Nix evaluator.
-- Errors are represented as Haskell exceptions so they can be caught by
-- the top-level exception handler and displayed to the user.
--
-- The error types cover:
-- - Type errors (type mismatches during operations)
-- - Attribute access errors (missing or invalid attributes)
-- - Variable binding errors (undefined variables)
-- - Arithmetic errors (division by zero, overflow)
-- - Control flow errors (assertion failures, user-thrown errors)
-- - Path resolution errors (environment paths not found)
--
-- Note: This module is intentionally standalone with no dependency on NixAttrs
-- or NixValue to avoid circular imports. Error context uses Text instead.
module Nix.Compile.Value.Error
  ( NixError(..)
  , throwNixError
  ) where

import Relude
import Control.Exception (throw)
import Nix.Types.VarName (VarName)
import Nix.Types.Path (Path)

-- | Runtime errors during Nix evaluation.
data NixError
  = TypeError !Text !Text
    -- ^ Type mismatch: expected type, got type
  | AttrMissing !VarName
    -- ^ Attribute not found in set
  | UndefinedVariable !VarName
    -- ^ Reference to undefined variable
  | AssertionFailed
    -- ^ Assert expression evaluated to false
  | ThrownError !Text
    -- ^ User-thrown error via builtins.throw
  | DivisionByZero
    -- ^ Division or modulo by zero
  | IntegerOverflow !Text
    -- ^ Integer arithmetic overflow
  | CoercionError !Text !Text
    -- ^ Failed coercion: from type, to type
  | InfiniteRecursion
    -- ^ Detected infinite recursion
  | AbortError !Text
    -- ^ builtins.abort called
  | EnvPathNotFound !Path
    -- ^ Environment path (like <nixpkgs>) not found in NIX_PATH
  | CyclicImport !Path
    -- ^ Cyclic import detected at this path. This means the file tried to
    -- import itself (directly or transitively) before its evaluation completed.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Exception)

-- | Throw a NixError as a Haskell exception.
-- This is used by primops when evaluation fails.
throwNixError :: NixError -> a
throwNixError = throw
{-# INLINE throwNixError #-}
