{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Attribute set manipulation builtins using the abstract value protocol.
--
-- This module contains builtins that operate on attribute sets using only
-- the abstract Nix.Value.Protocol and Nix.AttrSet.Sig signatures.
-- This allows the implementations to be monomorphized at link time
-- when instantiated with concrete types.
--
-- IMPORTANT: All attrset operations are obtained from Nix.Value.Protocol
-- which re-exports from Nix.AttrSet.Sig to ensure type identity matches.
-- Do NOT import Nix.AttrSet.Sig directly in client code.
--
-- Builtins provided:
--   Attribute access: hasAttr, getAttr
--   Attribute queries: attrNames, attrValues
--   Transformations: mapAttrs, catAttrs
--   Set operations: removeAttrs, intersectAttrs
--   Construction: listToAttrs
module Nix.Builtins.AttrSet
  ( -- * Attribute access
    hasAttrNix
  , getAttrNix
    -- * Attribute queries
  , attrNamesNix
  , attrValuesNix
    -- * Transformations
  , mapAttrsNix
  , catAttrsNix
    -- * Set operations
  , removeAttrsNix
  , intersectAttrsNix
    -- * Construction
  , listToAttrsNix
  ) where

import           Relude hiding (head, tail)
import           Data.Foldable                  ( foldr' )
import           Control.Monad.Catch            ( MonadThrow )

-- Import from concrete value-core - all operations via Protocol for correct type identity
import           Nix.Core.Value.Protocol        ( NValue )
import qualified Nix.Core.Value.Protocol       as V
import           Nix.Types.VarName              ( VarName, varNameText, mkVarName )

-- * Constraint aliases

-- | Constraint alias for attrset builtins that need full value operations.
-- Includes demand/defer, interned values, function calls, and error handling.
type MonadAttrSetBuiltin t f m =
  ( Monad m
  , MonadThrow m
  , V.MonadValue (NValue t f m) m
  , V.GivenInterned t f m
  , V.MonadThunk t m (NValue t f m)
  , V.NVConstraint f
  )

-- * Attribute access

-- | Check if an attribute exists in a set.
-- Returns interned false immediately for empty set (fast path).
-- Returns interned boolean for zero-allocation result.
hasAttrNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
hasAttrNix name set = do
  key <- V.demandString "builtins.hasAttr" name
  attrs <- V.demandAttrSet "builtins.hasAttr" set
  -- Fast path: empty set always returns false
  if V.attrSetNull attrs
    then pure V.internedFalse
    else pure $ V.internedBool $ V.attrSetMember (mkVarName key) attrs

-- | Get an attribute from a set.
getAttrNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
getAttrNix name set = do
  key <- V.demandString "builtins.getAttr" name
  attrs <- V.demandAttrSet "builtins.getAttr" set
  case V.attrSetLookup (mkVarName key) attrs of
    Nothing -> V.throwTypeError $ "builtins.getAttr: attribute '" <> key <> "' missing"
    Just v -> pure v

-- * Attribute queries

-- | Get attribute names from a set as a list of strings.
-- Fast path: returns interned empty list for empty set.
attrNamesNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
attrNamesNix set = do
  attrs <- V.demandAttrSet "builtins.attrNames" set
  if V.attrSetNull attrs
    then pure V.internedEmptyList
    else do
      -- Get sorted keys and convert to string values
      let sortedKeys = sort $ V.attrSetKeys attrs
      let strings = map (V.mkStringNoContext . varNameText) sortedKeys
      pure $ V.mkList $ V.listFromList strings

-- | Get attribute values from a set as a list.
-- Fast path: returns interned empty list for empty set.
attrValuesNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
attrValuesNix set = do
  attrs <- V.demandAttrSet "builtins.attrValues" set
  if V.attrSetNull attrs
    then pure V.internedEmptyList
    else do
      -- Get values sorted by key
      let sortedPairs = sortOn fst $ V.attrSetToList attrs
      let values = map snd sortedPairs
      pure $ V.mkList $ V.listFromList values

-- * Transformations

-- | Map a function over attribute values.
mapAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapAttrsNix f set = do
  attrs <- V.demandAttrSet "builtins.mapAttrs" set
  if V.attrSetNull attrs
    then pure V.internedEmptySet
    else do
      result <- V.attrSetTraverseWithKey applyF attrs
      pure $ V.mkSetRaw result
 where
  applyF key val =
    V.defer $ do
      -- Call f with the key string
      f' <- V.callFunc f $ V.mkStringNoContext (varNameText key)
      -- Call the result with the value
      V.callFunc f' val

-- | Extract attribute from each attrset in a list, if present.
catAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
catAttrsNix name list = do
  attrName <- V.demandString "builtins.catAttrs" name
  lst <- V.demandList "builtins.catAttrs" list
  if V.listNull lst
    then pure V.internedEmptyList
    else do
      -- Traverse the list, extracting the attribute from each attrset
      let key = mkVarName attrName
      maybeVals <- traverse (extractAttr key) lst
      let result = V.listFromList $ catMaybes $ V.listToList maybeVals
      if V.listNull result
        then pure V.internedEmptyList
        else pure $ V.mkList result
 where
  extractAttr :: VarName -> NValue t f m -> m (Maybe (NValue t f m))
  extractAttr key elem = do
    attrs <- V.demandAttrSet "builtins.catAttrs" elem
    pure $ V.attrSetLookup key attrs

-- * Set operations

-- | Remove attributes from a set.
-- Fast path: returns interned empty set if result is empty.
removeAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
removeAttrsNix set names = do
  attrs <- V.demandAttrSet "builtins.removeAttrs" set
  nameList <- V.demandList "builtins.removeAttrs" names
  -- Extract string names from the list
  keysToRemove <- traverse extractKey nameList
  let result = foldl' (flip V.attrSetDelete) attrs keysToRemove
  if V.attrSetNull result
    then pure V.internedEmptySet
    else pure $ V.mkSetRaw result
 where
  extractKey :: NValue t f m -> m VarName
  extractKey elem = do
    s <- V.demandString "builtins.removeAttrs" elem
    pure $ mkVarName s

-- | Intersection of two attribute sets.
intersectAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
intersectAttrsNix set1 set2 = do
  attrs1 <- V.demandAttrSet "builtins.intersectAttrs" set1
  attrs2 <- V.demandAttrSet "builtins.intersectAttrs" set2
  -- Fast path: return interned empty set if either input is empty
  if V.attrSetNull attrs1 || V.attrSetNull attrs2
    then pure V.internedEmptySet
    else do
      -- Intersection keeps values from attrs2 where keys exist in attrs1
      let result = attrs2 `V.attrSetIntersection` attrs1
      if V.attrSetNull result
        then pure V.internedEmptySet
        else pure $ V.mkSetRaw result

-- * Construction

-- | Convert a list of {name, value} attrsets to an attrset.
-- First occurrence wins for duplicate keys.
listToAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
listToAttrsNix list = do
  lst <- V.demandList "builtins.listToAttrs" list
  if V.listNull lst
    then pure V.internedEmptySet
    else do
      -- Build pairs in order
      pairs <- traverse extractPair lst
      -- Use foldr' so first occurrence wins (Nix semantics)
      let result = foldr' (uncurry V.attrSetInsert) V.attrSetEmpty pairs
      pure $ V.mkSetRaw result
 where
  extractPair :: NValue t f m -> m (VarName, NValue t f m)
  extractPair elem = do
    attrs <- V.demandAttrSet "builtins.listToAttrs" elem
    -- Get "name" attribute
    case V.attrSetLookup (mkVarName "name") attrs of
      Nothing -> V.throwTypeError "builtins.listToAttrs: element missing 'name' attribute"
      Just nameVal -> do
        key <- V.demandString "builtins.listToAttrs" nameVal
        -- Get "value" attribute
        case V.attrSetLookup (mkVarName "value") attrs of
          Nothing -> V.throwTypeError "builtins.listToAttrs: element missing 'value' attribute"
          Just val -> pure (mkVarName key, val)
