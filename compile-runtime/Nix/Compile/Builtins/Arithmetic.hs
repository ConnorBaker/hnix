{-# LANGUAGE NoStrict #-}

-- | Arithmetic built-in functions for the compiled Nix runtime.
--
-- This module implements arithmetic operations available in Nix:
-- * Basic operations: add, sub, mul, div
-- * Rounding: floor, ceil
-- * Bitwise operations: bitAnd, bitOr, bitXor
-- * Comparison: lessThan
--
-- Most operations are curried for multi-argument support, returning VBuiltin
-- for partial application.
module Nix.Compile.Builtins.Arithmetic
  ( -- * Arithmetic operations
    builtinAdd
  , builtinSub
  , builtinMul
  , builtinDiv
    -- * Rounding
  , builtinFloor
  , builtinCeil
    -- * Bitwise operations
  , builtinBitAnd
  , builtinBitOr
  , builtinBitXor
    -- * Comparison
  , builtinLessThan
  ) where

import Relude
import Data.Bits ((.&.), (.|.), xor)
import Nix.Compile.Value
import Nix.Compile.Primops

-- * Arithmetic builtins

builtinAdd :: NixValue
builtinAdd = VBuiltin "add" $ \v1 ->
  VBuiltin "add y" $ \v2 -> nixAdd v1 v2
{-# NOINLINE builtinAdd #-}

builtinSub :: NixValue
builtinSub = VBuiltin "sub" $ \v1 ->
  VBuiltin "sub y" $ \v2 -> nixSub v1 v2
{-# NOINLINE builtinSub #-}

builtinMul :: NixValue
builtinMul = VBuiltin "mul" $ \v1 ->
  VBuiltin "mul y" $ \v2 -> nixMul v1 v2
{-# NOINLINE builtinMul #-}

builtinDiv :: NixValue
builtinDiv = VBuiltin "div" $ \v1 ->
  VBuiltin "div y" $ \v2 -> nixDiv v1 v2
{-# NOINLINE builtinDiv #-}

builtinFloor :: NixValue -> NixValue
builtinFloor (VFloat n) = VInt (floor n)
builtinFloor v@(VInt _) = v
builtinFloor v = throwNixError $ TypeError "a number" (valueTypeName v)
{-# INLINE builtinFloor #-}

builtinCeil :: NixValue -> NixValue
builtinCeil (VFloat n) = VInt (ceiling n)
builtinCeil v@(VInt _) = v
builtinCeil v = throwNixError $ TypeError "a number" (valueTypeName v)
{-# INLINE builtinCeil #-}

builtinBitAnd :: NixValue
builtinBitAnd = VBuiltin "bitAnd" $ \v1 ->
  VBuiltin "bitAnd y" $ \v2 ->
    VInt $ expectInt v1 .&. expectInt v2
{-# NOINLINE builtinBitAnd #-}

builtinBitOr :: NixValue
builtinBitOr = VBuiltin "bitOr" $ \v1 ->
  VBuiltin "bitOr y" $ \v2 ->
    VInt $ expectInt v1 .|. expectInt v2
{-# NOINLINE builtinBitOr #-}

builtinBitXor :: NixValue
builtinBitXor = VBuiltin "bitXor" $ \v1 ->
  VBuiltin "bitXor y" $ \v2 ->
    VInt $ expectInt v1 `xor` expectInt v2
{-# NOINLINE builtinBitXor #-}

builtinLessThan :: NixValue
builtinLessThan = VBuiltin "lessThan" $ \v1 ->
  VBuiltin "lessThan y" $ \v2 -> nixLt v1 v2
{-# NOINLINE builtinLessThan #-}
