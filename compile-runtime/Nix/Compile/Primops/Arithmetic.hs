{-# LANGUAGE NoStrict #-}

-- | Arithmetic primitive operations for the compiled Nix runtime.
--
-- These functions implement Nix's numeric operations with proper type coercion
-- and overflow checking. They are called by generated GHC Core code.
--
-- Design principles:
-- * Match Nix semantics exactly (checked arithmetic, type promotion rules)
-- * Support mixed int/float arithmetic with automatic promotion
-- * Throw NixError on type mismatches rather than returning Maybe
-- * INLINE aggressively - these are hot paths in numeric code
module Nix.Compile.Primops.Arithmetic
  ( nixAdd
  , nixSub
  , nixMul
  , nixDiv
  , nixNeg
  ) where

import Relude
import qualified Data.HashSet as HS
import Nix.Types.Path (Path)
import Nix.Types.Atom (checkedAdd, checkedSub, checkedMul, checkedDiv, checkedNeg)
import Nix.Compile.Value
import Nix.Compile.Primops.Coerce (Numeric(..), toNumeric, promoteToFloat)

-- | Addition. Handles int+int, float+float, int+float, string concatenation, and path concatenation.
nixAdd :: NixValue -> NixValue -> NixValue
nixAdd (VString t1 ctx1) (VString t2 ctx2) = VString (t1 <> t2) (unionContext ctx1 ctx2)
-- Path concatenation cases (Nix semantics: path + string appends string to path)
nixAdd (VPath p1) (VPath p2) = VPath (p1 <> p2)
nixAdd (VPath p) (VString s ctx)
  | HS.null ctx = VPath (p <> fromString (toString s))
  | otherwise = throwNixError $ CoercionError "a string with context" "path appendable"
nixAdd (VString s ctx) (VPath p) = VString (s <> toText p) (unionContext ctx (pathContext (toText p)))
nixAdd v1 v2 =
  case (toNumeric v1, toNumeric v2) of
    (NumInt a, NumInt b) ->
      case checkedAdd a b of
        Left err -> throwNixError $ IntegerOverflow (fromString err)
        Right r -> VInt r
    (n1, n2) -> VFloat (promoteToFloat n1 + promoteToFloat n2)
{-# INLINE nixAdd #-}

-- | Subtraction. Int-Int stays int; anything with float becomes float.
nixSub :: NixValue -> NixValue -> NixValue
nixSub v1 v2 =
  case (toNumeric v1, toNumeric v2) of
    (NumInt a, NumInt b) ->
      case checkedSub a b of
        Left err -> throwNixError $ IntegerOverflow (fromString err)
        Right r -> VInt r
    (n1, n2) -> VFloat (promoteToFloat n1 - promoteToFloat n2)
{-# INLINE nixSub #-}

-- | Multiplication. Int*Int stays int; anything with float becomes float.
nixMul :: NixValue -> NixValue -> NixValue
nixMul v1 v2 =
  case (toNumeric v1, toNumeric v2) of
    (NumInt a, NumInt b) ->
      case checkedMul a b of
        Left err -> throwNixError $ IntegerOverflow (fromString err)
        Right r -> VInt r
    (n1, n2) -> VFloat (promoteToFloat n1 * promoteToFloat n2)
{-# INLINE nixMul #-}

-- | Division. Int/Int uses integer division; anything with float uses float division.
nixDiv :: NixValue -> NixValue -> NixValue
nixDiv v1 v2 =
  case (toNumeric v1, toNumeric v2) of
    (NumInt a, NumInt b) ->
      case checkedDiv a b of
        Left err -> throwNixError $ DivisionByZero
        Right r -> VInt r
    (n1, n2) ->
      let d = promoteToFloat n2
      in if d == 0
         then throwNixError DivisionByZero
         else VFloat (promoteToFloat n1 / d)
{-# INLINE nixDiv #-}

-- | Unary negation.
nixNeg :: NixValue -> NixValue
nixNeg v =
  case toNumeric v of
    NumInt n ->
      case checkedNeg n of
        Left err -> throwNixError $ IntegerOverflow (fromString err)
        Right r -> VInt r
    NumFloat n -> VFloat (negate n)
{-# INLINE nixNeg #-}
