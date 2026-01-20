{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
hasAttrNix x y = do
  x' <- V.demand x
  y' <- V.demand y
  case V.extractStringNoContext x' of
    Nothing -> V.throwTypeError "builtins.hasAttr: first argument must be a string"
    Just keyText ->
      case V.extractAttrSetRaw y' of
        Nothing -> V.throwTypeError "builtins.hasAttr: second argument must be an attrset"
        Just aset
          -- Fast path: empty set always returns false
          | V.attrSetNull aset -> pure V.internedFalse
          | otherwise -> pure $ V.internedBool $ V.attrSetMember (mkVarName keyText) aset

-- | Get an attribute from a set.
getAttrNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
getAttrNix x y = do
  x' <- V.demand x
  y' <- V.demand y
  case V.extractStringNoContext x' of
    Nothing -> V.throwTypeError "builtins.getAttr: first argument must be a string"
    Just keyText ->
      case V.extractAttrSetRaw y' of
        Nothing -> V.throwTypeError "builtins.getAttr: second argument must be an attrset"
        Just aset ->
          case V.attrSetLookup (mkVarName keyText) aset of
            Nothing -> V.throwTypeError $ "builtins.getAttr: attribute '" <> keyText <> "' missing"
            Just v -> pure v

-- * Attribute queries

-- | Get attribute names from a set as a list of strings.
-- Fast path: returns interned empty list for empty set.
attrNamesNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
attrNamesNix nvset = do
  v <- V.demand nvset
  case V.extractAttrSetRaw v of
    Nothing -> V.throwTypeError "builtins.attrNames: argument must be an attrset"
    Just attrs
      | V.attrSetNull attrs -> pure V.internedEmptyList
      | otherwise -> do
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
attrValuesNix nvattrs = do
  v <- V.demand nvattrs
  case V.extractAttrSetRaw v of
    Nothing -> V.throwTypeError "builtins.attrValues: argument must be an attrset"
    Just attrs
      | V.attrSetNull attrs -> pure V.internedEmptyList
      | otherwise -> do
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
mapAttrsNix f xs = do
  xs' <- V.demand xs
  case V.extractAttrSetRaw xs' of
    Nothing -> V.throwTypeError "builtins.mapAttrs: second argument must be an attrset"
    Just nixAttrset
      | V.attrSetNull nixAttrset -> pure V.internedEmptySet
      | otherwise -> do
          result <- V.attrSetTraverseWithKey applyFunToKeyVal nixAttrset
          pure $ V.mkSetRaw result
 where
  applyFunToKeyVal key val =
    V.defer $ do
      -- Call f with the key string
      runFunForKey <- V.callFunc f $ V.mkStringNoContext (varNameText key)
      -- Call the result with the value
      V.callFunc runFunForKey val

-- | Extract attribute from each attrset in a list, if present.
catAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
catAttrsNix attrName xs = do
  attrName' <- V.demand attrName
  xs' <- V.demand xs
  case V.extractStringNoContext attrName' of
    Nothing -> V.throwTypeError "builtins.catAttrs: first argument must be a string"
    Just n ->
      case V.extractList xs' of
        Nothing -> V.throwTypeError "builtins.catAttrs: second argument must be a list"
        Just v
          | V.listNull v -> pure V.internedEmptyList
          | otherwise -> do
              -- Traverse the list, extracting the attribute from each attrset
              let key = mkVarName n
              maybeVals <- traverse (extractAttrFromSet key) v
              let result = V.listFromList $ catMaybes $ V.listToList maybeVals
              if V.listNull result
                then pure V.internedEmptyList
                else pure $ V.mkList result
 where
  extractAttrFromSet :: VarName -> NValue t f m -> m (Maybe (NValue t f m))
  extractAttrFromSet key nv' = do
    v' <- V.demand nv'
    case V.extractAttrSetRaw v' of
      Nothing -> V.throwTypeError "builtins.catAttrs: list element is not an attrset"
      Just attrs -> pure $ V.attrSetLookup key attrs

-- * Set operations

-- | Remove attributes from a set.
-- Fast path: returns interned empty set if result is empty.
removeAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
removeAttrsNix set v = do
  set' <- V.demand set
  v' <- V.demand v
  case V.extractAttrSetRaw set' of
    Nothing -> V.throwTypeError "builtins.removeAttrs: first argument must be an attrset"
    Just m ->
      case V.extractList v' of
        Nothing -> V.throwTypeError "builtins.removeAttrs: second argument must be a list"
        Just toRemoveList -> do
          -- Extract string names from the list
          toRemove <- traverse extractName toRemoveList
          let resultAttrs = foldl' (flip V.attrSetDelete) m toRemove
          if V.attrSetNull resultAttrs
            then pure V.internedEmptySet
            else pure $ V.mkSetRaw resultAttrs
 where
  extractName :: NValue t f m -> m VarName
  extractName nv' = do
    v' <- V.demand nv'
    case V.extractStringNoContext v' of
      Nothing -> V.throwTypeError "builtins.removeAttrs: list element must be a string"
      Just s -> pure $ mkVarName s

-- | Intersection of two attribute sets.
intersectAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
intersectAttrsNix set1 set2 = do
  set1' <- V.demand set1
  set2' <- V.demand set2
  case (V.extractAttrSetRaw set1', V.extractAttrSetRaw set2') of
    (Nothing, _) -> V.throwTypeError "builtins.intersectAttrs: first argument must be an attrset"
    (_, Nothing) -> V.throwTypeError "builtins.intersectAttrs: second argument must be an attrset"
    (Just s1, Just s2)
      -- Fast path: return interned empty set if either input is empty
      | V.attrSetNull s1 || V.attrSetNull s2 -> pure V.internedEmptySet
      | otherwise -> do
          -- Intersection keeps values from s2 where keys exist in s1
          let result = s2 `V.attrSetIntersection` s1
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
listToAttrsNix lst = do
  lst' <- V.demand lst
  case V.extractList lst' of
    Nothing -> V.throwTypeError "builtins.listToAttrs: argument must be a list"
    Just v
      | V.listNull v -> pure V.internedEmptySet
      | otherwise -> do
          -- Build pairs in order
          pairs <- traverse extractPair v
          -- Use foldr' so first occurrence wins (Nix semantics)
          let result = foldr' (uncurry V.attrSetInsert) V.attrSetEmpty pairs
          pure $ V.mkSetRaw result
 where
  extractPair :: NValue t f m -> m (VarName, NValue t f m)
  extractPair nv' = do
    v' <- V.demand nv'
    case V.extractAttrSetRaw v' of
      Nothing -> V.throwTypeError "builtins.listToAttrs: list element must be an attrset"
      Just a -> do
        -- Get "name" attribute
        case V.attrSetLookup (mkVarName "name") a of
          Nothing -> V.throwTypeError "builtins.listToAttrs: element missing 'name' attribute"
          Just nameNv -> do
            nameV <- V.demand nameNv
            case V.extractStringNoContext nameV of
              Nothing -> V.throwTypeError "builtins.listToAttrs: 'name' must be a string"
              Just nameText -> do
                -- Get "value" attribute
                case V.attrSetLookup (mkVarName "value") a of
                  Nothing -> V.throwTypeError "builtins.listToAttrs: element missing 'value' attribute"
                  Just val -> pure (mkVarName nameText, val)
