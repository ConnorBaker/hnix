{-# LANGUAGE NoStrict #-}

-- | Type checking and conversion builtins for the compiled Nix runtime.
--
-- This module contains all type predicates (isNull, isInt, etc.),
-- type introspection (typeOf), and type conversion builtins
-- (toJSON, fromJSON, etc.).
module Nix.Compile.Builtins.Type
  ( -- * Type predicates
    builtinIsNull
  , builtinIsInt
  , builtinIsFloat
  , builtinIsBool
  , builtinIsString
  , builtinIsList
  , builtinIsAttrs
  , builtinIsFunction
  , builtinIsPath
  , builtinTypeOf
    -- * Type conversion
  , builtinToString
  , builtinToInt
  , builtinToFloat
  , builtinToPath
  , builtinToJSON
  , builtinFromJSON
  ) where

import Relude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import Nix.Types.VarName (varNameText, mkVarName)
import Nix.Compile.Value
import Nix.Compile.Primops

-- * Type predicates

builtinIsNull :: NixValue -> NixValue
builtinIsNull v = VBool $ isNull v
{-# INLINE builtinIsNull #-}

builtinIsInt :: NixValue -> NixValue
builtinIsInt v = VBool $ isInt v
{-# INLINE builtinIsInt #-}

builtinIsFloat :: NixValue -> NixValue
builtinIsFloat v = VBool $ isFloat v
{-# INLINE builtinIsFloat #-}

builtinIsBool :: NixValue -> NixValue
builtinIsBool v = VBool $ isBool v
{-# INLINE builtinIsBool #-}

builtinIsString :: NixValue -> NixValue
builtinIsString v = VBool $ isString v
{-# INLINE builtinIsString #-}

builtinIsList :: NixValue -> NixValue
builtinIsList v = VBool $ isList v
{-# INLINE builtinIsList #-}

builtinIsAttrs :: NixValue -> NixValue
builtinIsAttrs v = VBool $ isAttrs v
{-# INLINE builtinIsAttrs #-}

builtinIsFunction :: NixValue -> NixValue
builtinIsFunction v = VBool $ isFunction v
{-# INLINE builtinIsFunction #-}

builtinIsPath :: NixValue -> NixValue
builtinIsPath v = VBool $ isPath v
{-# INLINE builtinIsPath #-}

-- | Get type name as string.
builtinTypeOf :: NixValue -> NixValue
builtinTypeOf v = VString typeName emptyContext
  where
    typeName = case v of
      VInt _ -> "int"
      VFloat _ -> "float"
      VBool _ -> "bool"
      VNull -> "null"
      VString _ _ -> "string"
      VPath _ -> "path"
      VList _ -> "list"
      VAttrs _ -> "set"
      VClosure _ _ -> "lambda"
      VBuiltin _ _ -> "lambda"
{-# INLINE builtinTypeOf #-}

-- * Type conversions

builtinToString :: NixValue -> NixValue
builtinToString = nixCoerceToString
{-# INLINE builtinToString #-}

builtinToInt :: NixValue -> NixValue
builtinToInt (VInt n) = VInt n
builtinToInt (VFloat n) = VInt (truncate n)
builtinToInt v =
  let (t, _) = expectString v
  in case readMaybe (toString t) of
       Just n -> VInt n
       Nothing -> throwNixError $ CoercionError (valueTypeName v) "an integer"
{-# INLINE builtinToInt #-}

builtinToFloat :: NixValue -> NixValue
builtinToFloat (VFloat n) = VFloat n
builtinToFloat (VInt n) = VFloat (fromIntegral n)
builtinToFloat v = throwNixError $ TypeError "a number" (valueTypeName v)
{-# INLINE builtinToFloat #-}

builtinToPath :: NixValue -> NixValue
builtinToPath (VPath p) = VPath p
builtinToPath v =
  let (t, _) = expectString v
  in VPath (fromString $ toString t)
{-# INLINE builtinToPath #-}

-- | Stub: JSON serialization
builtinToJSON :: NixValue -> NixValue
builtinToJSON v = VString (toJSONText v) emptyContext
  where
    toJSONText :: NixValue -> Text
    toJSONText VNull = "null"
    toJSONText (VBool True) = "true"
    toJSONText (VBool False) = "false"
    toJSONText (VInt n) = show n
    toJSONText (VFloat n) = show n
    toJSONText (VString t _) = show t  -- JSON string escaping
    toJSONText (VList lst) = "[" <> T.intercalate "," (V.toList $ V.map toJSONText lst) <> "]"
    toJSONText (VAttrs as) =
      "{" <> T.intercalate ","
        [show (varNameText k) <> ":" <> toJSONText v' | (k, v') <- attrToList as]
      <> "}"
    toJSONText (VPath p) = show (toText p)
    toJSONText (VClosure _ _) = throwNixError $ CoercionError "a function" "JSON"
    toJSONText (VBuiltin _ _) = throwNixError $ CoercionError "a function" "JSON"
{-# INLINE builtinToJSON #-}

-- | Parse a JSON string and convert to NixValue.
-- Handles all JSON types: objects, arrays, strings, numbers, booleans, null.
builtinFromJSON :: NixValue -> NixValue
builtinFromJSON arg =
  let (str, _) = expectString arg
      bytes = LBS.fromStrict $ encodeUtf8 str
  in case Aeson.decode bytes of
       Just val -> jsonToNix val
       Nothing -> throwNixError $ ThrownError "builtins.fromJSON: invalid JSON"
{-# INLINE builtinFromJSON #-}

-- | Convert an Aeson Value to a NixValue.
jsonToNix :: Aeson.Value -> NixValue
jsonToNix = \case
  Aeson.Object obj ->
    VAttrs $ attrsFromList
      [(mkVarName (Key.toText k), jsonToNix v) | (k, v) <- KM.toList obj]
  Aeson.Array arr ->
    VList $ V.map jsonToNix arr
  Aeson.String t ->
    VString t emptyContext
  Aeson.Number n ->
    -- Nix distinguishes int vs float
    case Scientific.floatingOrInteger n of
      Left d -> VFloat d
      Right i -> VInt (fromIntegral (i :: Integer))
  Aeson.Bool b ->
    VBool b
  Aeson.Null ->
    VNull
{-# INLINE jsonToNix #-}
