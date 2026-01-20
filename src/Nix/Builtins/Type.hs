{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Type checking and introspection builtins.
--
-- This module contains builtins that check or report the type of values:
-- isAttrs, isBool, isFloat, isFunction, isInt, isList, isNull, isPath,
-- isString, typeOf, and functionArgs.
module Nix.Builtins.Type
  ( -- * Type checking builtins
    isAttrsNix
  , isBoolNix
  , isFloatNix
  , isFunctionNix
  , isIntNix
  , isListNix
  , isNullNix
  , isPathNix
  , isStringNix
  , typeOfNix
    -- * Function introspection
  , functionArgsNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Nix.Atoms                      ( NAtom(..) )
import qualified Nix.Core.AttrSet              as A
import           Nix.Core.List                  ( NixList )
import           Nix.Builtins.Internal          ( hasKind, pattern NVBool )
import           Nix.Convert
import           Nix.Exec
import           Nix.Expr.Types                 ( AttrSet, Params(..) )
import           Nix.Frames
import           Nix.String
import           Nix.Value
import           Nix.Value.Monad

-- * Type checking builtins

-- | Check if a value is an attribute set. Returns interned boolean.
isAttrsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isAttrsNix = hasKind @(AttrSet (NValue t f m))

-- | Check if a value is a boolean. Returns interned boolean.
isBoolNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isBoolNix = hasKind @Bool

-- | Check if a value is a float. Returns interned boolean.
isFloatNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isFloatNix = hasKind @Double

-- | Check if a value is a function. Returns interned boolean.
isFunctionNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
isFunctionNix nv =
  do
    v <- demand nv
    case v of
      NVClosure{} -> pure internedTrue
      _           -> pure internedFalse

-- | Check if a value is an integer. Returns interned boolean.
isIntNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isIntNix = hasKind @Int

-- | O(1) check using NixList type instead of list to avoid O(n) conversion.
isListNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isListNix = hasKind @(NixList (NValue t f m))

-- | Check if a value is null. Returns interned boolean.
isNullNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isNullNix = hasKind @()

-- | Check if a value is a path. Returns interned boolean.
isPathNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isPathNix nv = do
  v <- demand nv
  case v of
    NVPath _ -> pure internedTrue
    _        -> pure internedFalse

-- | Check if a value is a string. Returns interned boolean.
-- Note: Cannot use `hasKind` because it coerces derivations to strings.
isStringNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
isStringNix nv =
  do
    v <- demand nv
    case v of
      NVStr{} -> pure internedTrue
      _       -> pure internedFalse

-- | Get the type of a value as a string.
typeOfNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
typeOfNix nvv =
  do
    v <- demand nvv
    let
      detectType =
        case v of
          NVConstant a ->
            case a of
              NURI   _ -> "string"
              NInt   _ -> "int"
              NFloat _ -> "float"
              NBool  _ -> "bool"
              NNull    -> "null"
          NVStr     _   -> "string"
          NVList    _   -> "list"
          NVSet     _ _ -> "set"
          NVClosure{}   -> "lambda"
          NVPath    _   -> "path"
          NVBuiltin _ _ -> "lambda"
          _             -> error "Pattern synonyms obscure complete patterns"

    toValue $ mkNixStringWithoutContext detectType

-- * Function introspection

-- | Get the arguments of a function as an attribute set.
-- For each argument, returns whether it has a default value.
functionArgsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
functionArgsNix nvfun =
  do
    fun <- demand nvfun
    case fun of
      NVClosure p _ ->
        toValue @(AttrSet (NValue t f m)) $ NVBool <$>
          case p of
            Param name     -> A.singleton name False
            ParamSet _ _ pset -> isJust <$> pset
      _v -> throwError $ ErrorCall $ "builtins.functionArgs: expected function, got " <> show _v
