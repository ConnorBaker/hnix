{-# LANGUAGE NoStrict #-}

-- | Attrset built-in functions for the compiled Nix runtime.
--
-- This module implements attribute set operations:
-- - Querying: attrNames, attrValues, hasAttr, getAttr, functionArgs
-- - Manipulation: removeAttrs, mapAttrs, intersectAttrs
-- - Collection: catAttrs, zipAttrsWith
-- - Construction: listToAttrs, genAttrs
module Nix.Compile.Builtins.AttrSet
  ( builtinAttrNames
  , builtinAttrValues
  , builtinHasAttr
  , builtinGetAttr
  , builtinRemoveAttrs
  , builtinMapAttrs
  , builtinIntersectAttrs
  , builtinCatAttrs
  , builtinFunctionArgs
  , builtinZipAttrsWith
  , builtinListToAttrs
  , builtinGenAttrs
  ) where

import Relude
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Vector as V
import Nix.Types.VarName (VarName, mkVarName, varNameText)
import Nix.Compile.Value
import Nix.Compile.Primops

-- * Attribute set builtins

-- | Get sorted attribute names.
builtinAttrNames :: NixValue -> NixValue
builtinAttrNames v =
  let as = expectAttrs v
      names = attrKeys as
  in VList $ V.fromList $ map (\n -> VString (varNameText n) emptyContext) names
{-# INLINE builtinAttrNames #-}

-- | Get attribute values in name-sorted order.
builtinAttrValues :: NixValue -> NixValue
builtinAttrValues v =
  let as = expectAttrs v
  in VList $ V.fromList $ attrValues as
{-# INLINE builtinAttrValues #-}

-- | Check if attribute exists. builtins.hasAttr name set
builtinHasAttr :: NixValue
builtinHasAttr = VBuiltin "hasAttr" $ \nameVal ->
  VBuiltin "hasAttr set" $ \v ->
    let (name, _) = expectString nameVal
        as = expectAttrs v
    in VBool $ isJust $ lookupAttr (mkVarName name) as
{-# NOINLINE builtinHasAttr #-}

-- | Get attribute value. builtins.getAttr name set
builtinGetAttr :: NixValue
builtinGetAttr = VBuiltin "getAttr" $ \nameVal ->
  VBuiltin "getAttr set" $ \v ->
    let (name, _) = expectString nameVal
        as = expectAttrs v
    in nixSelect as (mkVarName name)
{-# NOINLINE builtinGetAttr #-}

-- | Remove attributes. builtins.removeAttrs set names
builtinRemoveAttrs :: NixValue
builtinRemoveAttrs = VBuiltin "removeAttrs" $ \setVal ->
  VBuiltin "removeAttrs names" $ \namesVal ->
    let as = expectAttrs setVal
        names = V.toList $ expectList namesVal
        toRemove = map (mkVarName . fst . expectString) names
    in VAttrs $ foldl' (flip deleteAttr) as toRemove
{-# NOINLINE builtinRemoveAttrs #-}

-- | Map over attributes. builtins.mapAttrs f set
builtinMapAttrs :: NixValue
builtinMapAttrs = VBuiltin "mapAttrs" $ \fn ->
  VBuiltin "mapAttrs set" $ \v ->
    let as = expectAttrs v
        f = expectFunction fn
        pairs = attrToList as
        mapped = [(k, expectFunction (f (VString (varNameText k) emptyContext)) val)
                 | (k, val) <- pairs]
    in VAttrs $ attrsFromList mapped
{-# NOINLINE builtinMapAttrs #-}

-- | Intersect two attribute sets (keeping left values).
builtinIntersectAttrs :: NixValue
builtinIntersectAttrs = VBuiltin "intersectAttrs" $ \v1 ->
  VBuiltin "intersectAttrs right" $ \v2 ->
    let as1 = expectAttrs v1
        as2 = expectAttrs v2
        keys1 = attrKeys as1
        intersected = [(k, nixSelect as2 k)
                      | k <- keys1, isJust $ lookupAttr k as2]
    in VAttrs $ attrsFromList intersected
{-# NOINLINE builtinIntersectAttrs #-}

-- | Collect attribute from list of sets. builtins.catAttrs attr list
builtinCatAttrs :: NixValue
builtinCatAttrs = VBuiltin "catAttrs" $ \nameVal ->
  VBuiltin "catAttrs list" $ \v ->
    let (name, _) = expectString nameVal
        lst = V.toList $ expectList v
        key = mkVarName name
        vals = [val | set <- lst, Just val <- [lookupAttr key (expectAttrs set)]]
    in VList $ V.fromList vals
{-# NOINLINE builtinCatAttrs #-}

-- | Get function argument names. Returns {} for non-pattern functions.
builtinFunctionArgs :: NixValue -> NixValue
builtinFunctionArgs (VClosure params _) =
  case params of
    RuntimeParam _ -> VAttrs emptyAttrs
    RuntimeParamSet _ _ paramList ->
      -- Build an attrset where each key is a param name and value is whether it has a default
      VAttrs $ attrsFromList
        [ (name, VBool hasDefault)
        | (name, hasDefault) <- paramList
        ]
builtinFunctionArgs (VBuiltin _ _) = VAttrs emptyAttrs
builtinFunctionArgs v = throwNixError $ TypeError "a function" (valueTypeName v)
{-# INLINE builtinFunctionArgs #-}

-- | Zip multiple attribute sets. builtins.zipAttrsWith f list
builtinZipAttrsWith :: NixValue
builtinZipAttrsWith = VBuiltin "zipAttrsWith" $ \fn ->
  VBuiltin "zipAttrsWith list" $ \v ->
    let lst = V.toList $ expectList v
        f = expectFunction fn
        -- Collect all keys
        allKeys = HS.toList $ HS.fromList $ concatMap (attrKeys . expectAttrs) lst
        -- For each key, collect values from all sets that have it
        zipKey k = ( k
                   , expectFunction
                       (f (VString (varNameText k) emptyContext))
                       (VList $ V.fromList [val | set <- lst
                                           , Just val <- [lookupAttr k (expectAttrs set)]])
                   )
    in VAttrs $ attrsFromList $ map zipKey allKeys
{-# NOINLINE builtinZipAttrsWith #-}

-- * List-to-attrs and genAttrs (shared interface)

-- | Convert a list of {name, value} attrs to an attrset.
-- IMPORTANT: On duplicate keys, the FIRST occurrence wins (Nix semantics).
builtinListToAttrs :: NixValue -> NixValue
builtinListToAttrs v =
  let lst = V.toList $ expectList v
      extractPair attrs =
        let as = expectAttrs attrs
        in ( mkVarName $ fst $ expectString $ nixSelect as (mkVarName "name")
           , nixSelect as (mkVarName "value")
           )
      pairs = map extractPair lst
      -- foldl' to keep FIRST occurrence: process left-to-right, skip duplicates
      finalAttrs = foldl' (\acc (k, val) ->
        case lookupAttr k acc of
          Just _ -> acc  -- Key already exists, keep first occurrence
          Nothing -> insertAttr k val acc
        ) emptyAttrs pairs
  in VAttrs finalAttrs
{-# INLINE builtinListToAttrs #-}

-- | Generate an attribute set from a list of names.
-- builtins.genAttrs names f returns { (n) = f n; } for each n in names.
builtinGenAttrs :: NixValue
builtinGenAttrs = VBuiltin "genAttrs" $ \namesVal ->
  VBuiltin "genAttrs f" $ \f ->
    let names = V.toList $ expectList namesVal
        fn = expectFunction f
        pairs = [ (mkVarName (fst $ expectString n), fn n)
                | n <- names
                ]
    in VAttrs $ attrsFromList pairs
{-# NOINLINE builtinGenAttrs #-}
