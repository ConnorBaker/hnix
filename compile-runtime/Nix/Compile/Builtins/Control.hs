{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStrict #-}

-- | Control flow built-in functions for the compiled Nix runtime.
--
-- This module implements control flow builtins that affect evaluation order and error handling:
-- - builtinThrow: Throw an error with a message
-- - builtinAbort: Abort evaluation with a message
-- - builtinTryEval: Try to evaluate, catching errors
-- - builtinSeq: Evaluate first argument then return second
-- - builtinDeepSeq: Deeply evaluate first argument then return second
-- - builtinTrace: Print a message and return the value
module Nix.Compile.Builtins.Control
  ( builtinThrow
  , builtinAbort
  , builtinTryEval
  , builtinSeq
  , builtinDeepSeq
  , builtinTrace
  ) where

import Relude
import Control.Exception (try, evaluate, throw, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Nix.Types.VarName (mkVarName, varNameText)
import Nix.Compile.Value
import Nix.Compile.Primops

-- * Control flow

builtinThrow :: NixValue -> NixValue
builtinThrow = nixThrow
{-# INLINE builtinThrow #-}

builtinAbort :: NixValue -> NixValue
builtinAbort = nixAbort
{-# INLINE builtinAbort #-}

-- | Try to evaluate, returning {success, value} or {success = false}.
--
-- IMPORTANT: In Nix, tryEval only catches errors from `throw`, NOT from `abort`.
-- - builtins.throw: Catchable by tryEval (returns {success=false; value=false})
-- - builtins.abort: NOT catchable by tryEval (propagates the error)
--
-- This matches Nix semantics where abort is for unrecoverable errors.
builtinTryEval :: NixValue -> NixValue
builtinTryEval v =
  -- Use unsafePerformIO to catch exceptions
  unsafePerformIO $ do
    -- Force evaluation to WHNF to catch immediate errors
    -- We catch NixError specifically and check if it's catchable
    result <- try @NixError $ evaluate $! v
    pure $ case result of
      Right val -> VAttrs $ attrsFromList
        [ (mkVarName "success", VBool True)
        , (mkVarName "value", val)
        ]
      Left err
        | isCatchableError err -> VAttrs $ attrsFromList
            [ (mkVarName "success", VBool False)
            , (mkVarName "value", VBool False)  -- Nix returns false for failed value
            ]
        | otherwise ->
            -- Re-throw non-catchable errors (like AbortError)
            throw err
{-# NOINLINE builtinTryEval #-}

-- | Check if a NixError is catchable by tryEval.
-- In Nix, tryEval catches most errors EXCEPT abort (which is for unrecoverable errors).
-- Catchable: throw, assert false, missing attrs, division by zero, type errors, etc.
-- Not catchable: abort (AbortError)
isCatchableError :: NixError -> Bool
isCatchableError (AbortError _) = False  -- Only abort is NOT catchable
isCatchableError _ = True                 -- Everything else is catchable
{-# INLINE isCatchableError #-}

-- | Evaluate first arg to WHNF then return second.
builtinSeq :: NixValue
builtinSeq = VBuiltin "seq" $ \v1 ->
  VBuiltin "seq y" $ \v2 ->
    v1 `seq` v2
{-# NOINLINE builtinSeq #-}

-- | Deeply evaluate first arg then return second.
builtinDeepSeq :: NixValue
builtinDeepSeq = VBuiltin "deepSeq" $ \v1 ->
  VBuiltin "deepSeq y" $ \v2 ->
    rnf v1 `seq` v2
{-# NOINLINE builtinDeepSeq #-}

-- | Print a message and return the value.
-- The message is coerced to string if not already a string.
-- For values that cannot be coerced (like attrsets without __toString/outPath),
-- a placeholder representation is shown.
builtinTrace :: NixValue
builtinTrace = VBuiltin "trace" $ \msgVal ->
  VBuiltin "trace value" $ \v ->
    let msg = traceValueToString msgVal
    in trace (toString msg) v
{-# NOINLINE builtinTrace #-}

-- | Convert a value to a string for trace output.
-- This is a best-effort conversion that handles all value types,
-- including those that cannot be coerced to strings via nixCoerceToString.
traceValueToString :: NixValue -> Text
traceValueToString = \case
  VString t _ -> t
  VPath p -> toText p
  VInt n -> show n
  VFloat n -> show n
  VBool True -> "true"
  VBool False -> "false"
  VNull -> "null"
  VList _ -> "«list»"
  -- For attrsets, try __toString/outPath via nixCoerceToString.
  -- If that throws (no __toString or outPath), use a placeholder.
  VAttrs as -> tryCoerceAttrset as
  VClosure _ _ -> "«lambda»"
  VBuiltin name _ -> "«primop " <> varNameText name <> "»"

-- | Try to coerce an attrset to string, returning a placeholder on failure.
-- Uses unsafePerformIO to catch the CoercionError that nixCoerceToString throws
-- when the attrset has no __toString or outPath.
tryCoerceAttrset :: NixAttrs -> Text
tryCoerceAttrset as = unsafePerformIO $ do
  result <- try @SomeException $ evaluate $ nixCoerceToString (VAttrs as)
  pure $ case result of
    Right (VString t _) -> t
    _ -> "{ ... }"  -- Placeholder for attrsets without __toString/outPath
{-# NOINLINE tryCoerceAttrset #-}
