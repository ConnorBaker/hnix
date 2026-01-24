{-# LANGUAGE NoStrict #-}

-- | Comparison operations for the compiled Nix runtime.
--
-- These functions implement Nix's comparison semantics for equality and ordering.
-- Error handling uses Haskell exceptions (NixError) which propagate through the generated code.
--
-- Design principles:
-- * Match Nix semantics exactly (type coercion rules, numeric promotion)
-- * Throw NixError on type mismatches rather than returning Maybe
-- * INLINE aggressively - these are hot paths
-- * No thunk management - GHC handles that
module Nix.Compile.Primops.Comparison
  ( nixEq
  , nixEqBool
  , nixNEq
  , nixLt
  , nixLte
  , nixGt
  , nixGte
  ) where

import Relude
import qualified Data.Vector as V
import Nix.Compile.Value
import Nix.Compile.Primops.Coerce (expectBool)

-- * Comparison operations

-- | Equality comparison. Works on all comparable types.
-- Functions are not comparable and throw.
nixEq :: NixValue -> NixValue -> NixValue
nixEq v1 v2 = VBool (nixEqBool v1 v2)
{-# INLINE nixEq #-}

-- | Internal equality as Bool for use in other comparisons.
nixEqBool :: NixValue -> NixValue -> Bool
nixEqBool (VInt a) (VInt b) = a == b
nixEqBool (VFloat a) (VFloat b) = a == b
nixEqBool (VInt a) (VFloat b) = fromIntegral a == b
nixEqBool (VFloat a) (VInt b) = a == fromIntegral b
nixEqBool (VBool a) (VBool b) = a == b
nixEqBool VNull VNull = True
nixEqBool (VString t1 _) (VString t2 _) = t1 == t2  -- Context is ignored for equality
nixEqBool (VPath p1) (VPath p2) = p1 == p2
nixEqBool (VList l1) (VList l2) =
  V.length l1 == V.length l2 && V.and (V.zipWith nixEqBool l1 l2)
nixEqBool (VAttrs a1) (VAttrs a2) =
  -- Use lookup-based comparison to avoid depending on HashMap iteration order.
  -- For each key in a1, look it up in a2 and compare values recursively.
  attrsSize a1 == attrsSize a2 &&
  all (\(k, v1) -> case lookupAttr k a2 of
         Just v2 -> nixEqBool v1 v2
         Nothing -> False)
      (attrToList a1)
nixEqBool (VClosure _ _) _ = throwNixError $ TypeError "a comparable value" "a function"
nixEqBool _ (VClosure _ _) = throwNixError $ TypeError "a comparable value" "a function"
nixEqBool (VBuiltin _ _) _ = throwNixError $ TypeError "a comparable value" "a function"
nixEqBool _ (VBuiltin _ _) = throwNixError $ TypeError "a comparable value" "a function"
nixEqBool _ _ = False  -- Different types are not equal

-- | Inequality comparison.
nixNEq :: NixValue -> NixValue -> NixValue
nixNEq v1 v2 = VBool (not $ nixEqBool v1 v2)
{-# INLINE nixNEq #-}

-- | Less than comparison. Only works on numbers and strings.
nixLt :: NixValue -> NixValue -> NixValue
nixLt v1 v2 = VBool (nixCompareBool (<) v1 v2)
{-# INLINE nixLt #-}

-- | Less than or equal.
nixLte :: NixValue -> NixValue -> NixValue
nixLte v1 v2 = VBool (nixCompareBool (<=) v1 v2)
{-# INLINE nixLte #-}

-- | Greater than.
nixGt :: NixValue -> NixValue -> NixValue
nixGt v1 v2 = VBool (nixCompareBool (>) v1 v2)
{-# INLINE nixGt #-}

-- | Greater than or equal.
nixGte :: NixValue -> NixValue -> NixValue
nixGte v1 v2 = VBool (nixCompareBool (>=) v1 v2)
{-# INLINE nixGte #-}

-- | Internal comparison helper.
nixCompareBool :: (forall a. Ord a => a -> a -> Bool) -> NixValue -> NixValue -> Bool
nixCompareBool cmp (VInt a) (VInt b) = cmp a b
nixCompareBool cmp (VFloat a) (VFloat b) = cmp a b
nixCompareBool cmp (VInt a) (VFloat b) = cmp (fromIntegral a) b
nixCompareBool cmp (VFloat a) (VInt b) = cmp a (fromIntegral b)
nixCompareBool cmp (VString t1 _) (VString t2 _) = cmp t1 t2
nixCompareBool _ v1 v2 = throwNixError $ TypeError "a comparable value" (valueTypeName v1 <> " and " <> valueTypeName v2)
