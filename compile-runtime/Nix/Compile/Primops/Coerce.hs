{-# LANGUAGE NoStrict #-}

-- | Type coercion functions for the compiled Nix runtime.
--
-- These functions extract values from NixValue with type checking,
-- throwing NixError on type mismatches. They are the core of Nix's
-- dynamic type system, ensuring runtime safety when accessing fields.
--
-- Design principles:
-- * Match Nix semantics for type checking
-- * Throw NixError on mismatch rather than returning Maybe
-- * INLINE aggressively - these are hot paths
module Nix.Compile.Primops.Coerce
  ( -- * Type coercion (throw on mismatch)
    expectInt
  , expectFloat
  , expectBool
  , expectString
  , expectPath
  , expectList
  , expectAttrs
  , expectFunction
    -- * Type predicates (for safe checking)
  , isAttrsPrimop
    -- * Numeric type promotion
  , Numeric(..)
  , toNumeric
  , promoteToFloat
  ) where

import Relude hiding (empty)
import Data.Vector (Vector)
import Nix.Types.VarName (VarName)
import Nix.Types.Path (Path)
import Nix.Compile.Value

-- * Type coercion functions

-- | Extract Int64 from a NixValue, throwing TypeError on mismatch.
expectInt :: NixValue -> Int64
expectInt (VInt n) = n
expectInt v = throwNixError $ TypeError "an integer" (valueTypeName v)
{-# INLINE expectInt #-}

-- | Extract Double from a NixValue, throwing TypeError on mismatch.
expectFloat :: NixValue -> Double
expectFloat (VFloat n) = n
expectFloat v = throwNixError $ TypeError "a float" (valueTypeName v)
{-# INLINE expectFloat #-}

-- | Extract Bool from a NixValue, throwing TypeError on mismatch.
expectBool :: NixValue -> Bool
expectBool (VBool b) = b
expectBool v = throwNixError $ TypeError "a boolean" (valueTypeName v)
{-# INLINE expectBool #-}

-- | Extract Text and context from a NixValue, throwing TypeError on mismatch.
expectString :: NixValue -> (Text, NixContext)
expectString (VString t ctx) = (t, ctx)
expectString v = throwNixError $ TypeError "a string" (valueTypeName v)
{-# INLINE expectString #-}

-- | Extract Path from a NixValue, throwing TypeError on mismatch.
expectPath :: NixValue -> Path
expectPath (VPath p) = p
expectPath v = throwNixError $ TypeError "a path" (valueTypeName v)
{-# INLINE expectPath #-}

-- | Extract Vector from a NixValue, throwing TypeError on mismatch.
expectList :: NixValue -> Vector NixValue
expectList (VList v) = v
expectList v = throwNixError $ TypeError "a list" (valueTypeName v)
{-# INLINE expectList #-}

-- | Extract NixAttrs from a NixValue, throwing TypeError on mismatch.
expectAttrs :: NixValue -> NixAttrs
expectAttrs (VAttrs as) = as
expectAttrs v = throwNixError $ TypeError "a set" (valueTypeName v)
{-# INLINE expectAttrs #-}

-- | Extract function from a NixValue, throwing TypeError on mismatch.
expectFunction :: NixValue -> (NixValue -> NixValue)
expectFunction (VClosure _ f) = f
expectFunction (VBuiltin _ f) = f
expectFunction v = throwNixError $ TypeError "a function" (valueTypeName v)
{-# INLINE expectFunction #-}

-- * Type predicates

-- | Check if a value is an attribute set.
-- Unlike expectAttrs, this returns Bool instead of throwing on non-attrsets.
-- Used by hasAttr path checking where non-attrset intermediates should return false.
isAttrsPrimop :: NixValue -> Bool
isAttrsPrimop (VAttrs _) = True
isAttrsPrimop _ = False
{-# INLINE isAttrsPrimop #-}

-- * Numeric type promotion

-- | Result of numeric coercion - either Int or Float
data Numeric = NumInt !Int64 | NumFloat !Double

-- | Coerce a value to a numeric type for arithmetic.
-- Integers and floats are accepted; everything else throws.
toNumeric :: NixValue -> Numeric
toNumeric (VInt n) = NumInt n
toNumeric (VFloat n) = NumFloat n
toNumeric v = throwNixError $ TypeError "a number" (valueTypeName v)
{-# INLINE toNumeric #-}

-- | Promote an integer to float if needed for mixed arithmetic.
promoteToFloat :: Numeric -> Double
promoteToFloat (NumInt n) = fromIntegral n
promoteToFloat (NumFloat n) = n
{-# INLINE promoteToFloat #-}
