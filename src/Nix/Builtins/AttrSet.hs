{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Attribute set manipulation builtins.
--
-- This module contains builtins that operate on attribute sets:
-- hasAttr, getAttr, attrNames, attrValues, mapAttrs, zipAttrsWith,
-- catAttrs, removeAttrs, intersectAttrs, listToAttrs.
module Nix.Builtins.AttrSet
  ( -- * Attribute access
    hasAttrNix
  , getAttrNix
    -- * Attribute queries
  , attrNamesNix
  , attrValuesNix
    -- * Transformations
  , mapAttrsNix
  , zipAttrsWithNix
  , catAttrsNix
    -- * Set operations
  , removeAttrsNix
  , intersectAttrsNix
    -- * Construction
  , listToAttrsNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Data.HashMap.Strict           as HM
import qualified Nix.Core.AttrSet              as A
import           Nix.Core.List                  ( NixList )
import qualified Nix.Core.List                 as L
import           Nix.Builtins.Internal          ( attrsetGet )
import           Nix.Convert
import           Nix.Exec
import           Nix.Expr.Types                 ( AttrSet, mkVarName, emptyPositionSet, PositionSet, VarName, varNameText )
import           Nix.Frames
import           Nix.String
import           Nix.Value
import           Nix.Value.Monad


-- * Attribute access

-- | Check if an attribute exists in a set.
-- Returns interned false immediately for empty set (fast path).
-- Returns interned boolean for zero-allocation result.
hasAttrNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
hasAttrNix x y =
  do
    (mkVarName -> key) <- fromStringNoContext =<< fromValue x
    (aset, _) <- fromValue @(AttrSet (NValue t f m), PositionSet) y
    -- Fast path: empty set always returns false
    if A.null aset
      then askInternedFalse
      else askInternedBool $ A.member key aset

getAttrNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
getAttrNix x y =
  do
    (mkVarName -> key) <- fromStringNoContext =<< fromValue x
    (aset, _) <- fromValue @(AttrSet (NValue t f m), PositionSet) y

    attrsetGet key aset


-- * Attribute queries

-- | Get attribute names from a set as a list of strings.
-- Fast path: returns interned empty list for empty set.
attrNamesNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
attrNamesNix nvset = do
  attrs <- fromValue @(AttrSet (NValue t f m)) nvset
  if A.null attrs
    then askInternedEmptyList
    else fmap coersion $ toValue @[NixString] $
      fmap (mkNixStringWithoutContext . varNameText) $ sort $ A.keys attrs
 where
  coersion = coerce :: CoerceDeeperToNValue t f m

-- | Get attribute values from a set as a list.
-- Fast path: returns interned empty list for empty set.
attrValuesNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
attrValuesNix nvattrs =
  do
    attrs <- fromValue @(AttrSet (NValue t f m)) nvattrs
    if A.null attrs
      then askInternedEmptyList
      else toValue $
        snd <$>
          sortOn
            (fst @VarName @(NValue t f m))
            (A.toList attrs)


-- * Transformations

mapAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapAttrsNix f xs =
  do
    nixAttrset <- fromValue @(AttrSet (NValue t f m)) xs
    -- Fast path: return interned empty set for empty input
    if A.null nixAttrset
      then askInternedEmptySet
      else do
        result <- A.traverseWithKey applyFunToKeyVal nixAttrset
        toValue result
 where
  applyFunToKeyVal key val =
    defer @(NValue t f m) . withFrame Debug (ErrorCall "While applying f in mapAttrs:\n") $ do
      runFunForKey <- callFunc f $ mkNVStrWithoutContext (varNameText key)
      callFunc runFunForKey val

zipAttrsWithNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
zipAttrsWithNix f nvSets =
  do
    sets <- fromValue @(NixList (NValue t f m)) =<< demand nvSets

    -- Fast path: return interned empty set for empty input
    if L.nlNull sets
      then askInternedEmptySet
      else do
        -- Collect values by key, accumulating as lists (O(1) prepend) then converting to Vector at end
        -- Uses (++) which prepends [val] to existing list in O(1), building lists in reverse order
        collected <-
          L.nlFoldM'
            (\ acc v -> do
              v' <- demand v
              case v' of
                NVSet _ attrs ->
                  pure $ A.foldlWithKey' (\m k val -> HM.insertWith (++) k [val] m) acc attrs
                _ ->
                  throwError $ ErrorCall $ "builtins.zipAttrsWith: expected a list of attrsets, got " <> show v'
            )
            mempty
            sets

        -- Fast path: return interned empty set if all input sets were empty
        if HM.null collected
          then askInternedEmptySet
          else do
            result <- HM.traverseWithKey applyFunToKeyVals collected
            toValue (A.fromList $ HM.toList result)
 where
  applyFunToKeyVals key vals =
    defer @(NValue t f m) . withFrame Debug (ErrorCall "While applying f in zipAttrsWith:\n") $ do
      runFunForKey <- callFunc f $ mkNVStrWithoutContext (varNameText key)
      -- Reverse to restore original order (we prepended during accumulation)
      callFunc runFunForKey (NVList (L.nlReverse (L.nlFromList vals)))

catAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
catAttrsNix attrName xs =
  do
    n <- fromStringNoContext =<< fromValue attrName
    v <- fromValue @(NixList (NValue t f m)) xs

    -- Fast path: return interned empty list for empty input
    if L.nlNull v
      then askInternedEmptyList
      else do
        -- Use L.nlMapMaybe to filter and transform in one pass
        result <- L.nlMapMaybe id <$>
          L.nlMapM
            (fmap (A.lookup $ mkVarName n) . fromValue <=< demand)
            v
        -- Fast path: return interned empty list if result is empty
        if L.nlNull result
          then askInternedEmptyList
          else pure $ NVList result


-- * Set operations

-- | Remove attributes from a set.
-- Fast path: returns interned empty set if result is empty.
removeAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
removeAttrsNix set v =
  do
    (m, p) <- fromValue @(AttrSet (NValue t f m), PositionSet) set
    (nsToRemove :: [NixString]) <- fromValue $ Deeper v
    (fmap mkVarName -> toRemove) <- traverse fromStringNoContext nsToRemove
    let resultAttrs = fun m toRemove
    if A.null resultAttrs
      then askInternedEmptySet
      else toValue (resultAttrs, fun p toRemove)
 where
  fun :: AttrSet a -> [VarName] -> AttrSet a
  fun = foldl' (flip A.delete)

intersectAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
intersectAttrsNix set1 set2 =
  do
    (s1, p1) <- fromValue @(AttrSet (NValue t f m), PositionSet) set1
    (s2, p2) <- fromValue @(AttrSet (NValue t f m), PositionSet) set2

    -- Fast path: return interned empty set if either input is empty
    if A.null s1 || A.null s2
      then askInternedEmptySet
      else do
        let result = s2 `A.intersection` s1
        -- Fast path: return interned empty set if result is empty
        if A.null result
          then askInternedEmptySet
          else pure $ NVSet (p2 `A.intersection` p1) result


-- * Construction

listToAttrsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
listToAttrsNix lst =
  do
    v <- fromValue @(NixList (NValue t f m)) lst
    -- Fast path: return interned empty set for empty input
    if L.nlNull v
      then askInternedEmptySet
      else do
        -- Build pairs in order, then use HM.fromList with reversed order
        -- so first occurrence wins (Nix semantics: first key wins)
        pairs <- L.nlMapM
          (\ nvattrset ->
            do
              a <- fromValue @(AttrSet (NValue t f m)) =<< demand nvattrset
              (mkVarName -> name) <- fromStringNoContext =<< fromValue =<< demand =<< attrsetGet "name" a
              val  <- attrsetGet "value" a
              pure (name, val)
          )
          v
        -- A.fromList keeps the last occurrence, but we want the first.
        -- Using L.nlFoldr' processes right-to-left, so first occurrence is inserted last and wins.
        pure $ NVSet emptyPositionSet $ L.nlFoldr' (uncurry A.insert) mempty pairs
