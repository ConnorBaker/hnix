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
  keyText <- V.demandString "builtins.hasAttr" x
  aset <- V.demandAttrSet "builtins.hasAttr" y
  -- Fast path: empty set always returns false
  if V.attrSetNull aset
    then pure V.internedFalse
    else pure $ V.internedBool $ V.attrSetMember (mkVarName keyText) aset

-- | Get an attribute from a set.
getAttrNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
getAttrNix x y = do
  keyText <- V.demandString "builtins.getAttr" x
  aset <- V.demandAttrSet "builtins.getAttr" y
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
  attrs <- V.demandAttrSet "builtins.attrNames" nvset
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
attrValuesNix nvattrs = do
  attrs <- V.demandAttrSet "builtins.attrValues" nvattrs
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
mapAttrsNix f xs = do
  nixAttrset <- V.demandAttrSet "builtins.mapAttrs" xs
  if V.attrSetNull nixAttrset
    then pure V.internedEmptySet
    else do
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
  n <- V.demandString "builtins.catAttrs" attrName
  v <- V.demandList "builtins.catAttrs" xs
  if V.listNull v
    then pure V.internedEmptyList
    else do
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
    attrs <- V.demandAttrSet "builtins.catAttrs" nv'
    pure $ V.attrSetLookup key attrs

-- * Set operations

-- | Remove attributes from a set.
-- Fast path: returns interned empty set if result is empty.
removeAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
removeAttrsNix set v = do
  m <- V.demandAttrSet "builtins.removeAttrs" set
  toRemoveList <- V.demandList "builtins.removeAttrs" v
  -- Extract string names from the list
  toRemove <- traverse extractName toRemoveList
  let resultAttrs = foldl' (flip V.attrSetDelete) m toRemove
  if V.attrSetNull resultAttrs
    then pure V.internedEmptySet
    else pure $ V.mkSetRaw resultAttrs
 where
  extractName :: NValue t f m -> m VarName
  extractName nv' = do
    s <- V.demandString "builtins.removeAttrs" nv'
    pure $ mkVarName s

-- | Intersection of two attribute sets.
intersectAttrsNix
  :: forall t f m . MonadAttrSetBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
intersectAttrsNix set1 set2 = do
  s1 <- V.demandAttrSet "builtins.intersectAttrs" set1
  s2 <- V.demandAttrSet "builtins.intersectAttrs" set2
  -- Fast path: return interned empty set if either input is empty
  if V.attrSetNull s1 || V.attrSetNull s2
    then pure V.internedEmptySet
    else do
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
  v <- V.demandList "builtins.listToAttrs" lst
  if V.listNull v
    then pure V.internedEmptySet
    else do
      -- Build pairs in order
      pairs <- traverse extractPair v
      -- Use foldr' so first occurrence wins (Nix semantics)
      let result = foldr' (uncurry V.attrSetInsert) V.attrSetEmpty pairs
      pure $ V.mkSetRaw result
 where
  extractPair :: NValue t f m -> m (VarName, NValue t f m)
  extractPair nv' = do
    a <- V.demandAttrSet "builtins.listToAttrs" nv'
    -- Get "name" attribute
    case V.attrSetLookup (mkVarName "name") a of
      Nothing -> V.throwTypeError "builtins.listToAttrs: element missing 'name' attribute"
      Just nameNv -> do
        nameText <- V.demandString "builtins.listToAttrs" nameNv
        -- Get "value" attribute
        case V.attrSetLookup (mkVarName "value") a of
          Nothing -> V.throwTypeError "builtins.listToAttrs: element missing 'value' attribute"
          Just val -> pure (mkVarName nameText, val)
