{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Arithmetic and comparison builtins.
--
-- This module contains builtins for arithmetic operations:
-- add, mul, div, lessThan, bitAnd, bitOr, bitXor.
module Nix.Builtins.Arithmetic
  ( -- * Arithmetic operations
    addNix
  , mulNix
  , divNix
    -- * Comparison
  , lessThanNix
    -- * Bitwise operations
  , bitAndNix
  , bitOrNix
  , bitXorNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Bits
import           Nix.Atoms                      ( NAtom(..), checkedAdd, checkedMul, checkedDiv )
import           Nix.Builtins.Internal          ( pattern NVBool )
import           Nix.Convert
import           Nix.Exec
import           Nix.Frames
import           Nix.String                     ( ignoreContext )
import           Nix.Value
import           Nix.Value.Monad


-- * Arithmetic operations

addNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
addNix nvX nvY =
  do
    x' <- demand nvX
    y' <- demand nvY

    case (x', y') of
      (NVConstant (NInt   x), NVConstant (NInt   y)) ->
        case checkedAdd x y of
          Left err -> throwError $ ErrorCall err
          Right r  -> toValue r
      (NVConstant (NFloat x), NVConstant (NInt   y)) -> toValue $             x + fromIntegral y
      (NVConstant (NInt   x), NVConstant (NFloat y)) -> toValue $ fromIntegral x + y
      (NVConstant (NFloat x), NVConstant (NFloat y)) -> toValue $             x + y
      (_x                   , _y                   ) -> throwError $ Addition _x _y

mulNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mulNix nvX nvY =
  do
    x' <- demand nvX
    y' <- demand nvY

    case (x', y') of
      (NVConstant (NInt   x), NVConstant (NInt   y)) ->
        case checkedMul x y of
          Left err -> throwError $ ErrorCall err
          Right r  -> toValue r
      (NVConstant (NFloat x), NVConstant (NInt   y)) -> toValue (x * fromIntegral y)
      (NVConstant (NInt   x), NVConstant (NFloat y)) -> toValue (fromIntegral x * y)
      (NVConstant (NFloat x), NVConstant (NFloat y)) -> toValue (x * y            )
      (_x                   , _y                   ) -> throwError $ Multiplication _x _y

divNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
divNix nvX nvY =
  do
    x' <- demand nvX
    y' <- demand nvY
    case (x', y') of
      (NVConstant (NInt   x), NVConstant (NInt   y)) | y /= 0 ->
        case checkedDiv x y of
          Left err -> throwError $ ErrorCall err
          Right r  -> toValue r
      (NVConstant (NFloat x), NVConstant (NInt   y)) | y /= 0 -> toValue $                     x / fromIntegral y
      (NVConstant (NInt   x), NVConstant (NFloat y)) | y /= 0 -> toValue $         fromIntegral x / y
      (NVConstant (NFloat x), NVConstant (NFloat y)) | y /= 0 -> toValue $                     x / y
      (_x                   , _y                   )         -> throwError $ Division _x _y


-- * Comparison

lessThanNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
lessThanNix ta tb =
  do
    va <- demand ta
    vb <- demand tb

    let
      badType = throwError $ ErrorCall $ "builtins.lessThan: expected two numbers or two strings, got '" <> show va <> "' and '" <> show vb <> "'."

    NVBool <$>
      case (va, vb) of
        (NVConstant ca, NVConstant cb) ->
          case (ca, cb) of
            (NInt   a, NInt   b) -> pure $             a < b
            (NInt   a, NFloat b) -> pure $ fromIntegral a < b
            (NFloat a, NInt   b) -> pure $             a < fromIntegral b
            (NFloat a, NFloat b) -> pure $             a < b
            _                    -> badType
        (NVStr a, NVStr b) -> pure $ ignoreContext a < ignoreContext b
        _ -> badType


-- * Bitwise operations

bitAndNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitAndNix x y =
  do
    a <- fromValue @Integer x
    b <- fromValue @Integer y

    toValue $ a .&. b

bitOrNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitOrNix x y =
  do
    a <- fromValue @Integer x
    b <- fromValue @Integer y

    toValue $ a .|. b

bitXorNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitXorNix x y =
  do
    a <- fromValue @Integer x
    b <- fromValue @Integer y

    toValue $ a `xor` b
