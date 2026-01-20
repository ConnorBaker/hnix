{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | List manipulation builtins using the abstract value protocol.
--
-- This module contains builtins that operate on lists using only
-- the abstract Nix.Value.Protocol and Nix.List.Sig signatures.
-- This allows the implementations to be monomorphized at link time
-- when instantiated with concrete types.
--
-- IMPORTANT: All list operations are obtained from Nix.Value.Protocol
-- which re-exports from Nix.List.Sig to ensure type identity matches.
-- Do NOT import Nix.List.Sig directly in client code.
--
-- Builtins provided:
--   Basic: length, head, tail, elemAt
--   Predicates: any, all, elem
--   Transformations: map, filter, foldl'
--   Generation: genList
--   Combining: concatLists, concatMap
module Nix.Builtins.List
  ( -- * Basic operations
    lengthNix
  , headNix
  , tailNix
  , elemAtNix
    -- * Predicates
  , anyNix
  , allNix
  , elemNix
    -- * Transformations
  , mapNix
  , filterNix
  , foldl'Nix
    -- * Generation
  , genListNix
    -- * Combining lists
  , concatListsNix
  , concatMapNix
  ) where

import           Relude hiding (head, tail)
import qualified Data.Text as Text
import           Control.Monad.Catch            ( MonadThrow )

-- Import from concrete value-core - all operations via Protocol for correct type identity
import           Nix.Core.Value.Protocol        ( NValue, NixList )
import qualified Nix.Core.Value.Protocol       as V

-- * Constraint aliases

-- | Constraint alias for builtins that need full value operations.
-- Includes demand/defer, interned values, function calls, and error handling.
type MonadListBuiltin t f m =
  ( Monad m
  , MonadThrow m
  , V.MonadValue (NValue t f m) m
  , V.GivenInterned t f m
  , V.MonadThunk t m (NValue t f m)
  , V.NVConstraint f
  )

-- * Basic operations

-- | Get the length of a list.
--
-- O(1) - uses underlying list implementation's length directly.
lengthNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
lengthNix list = do
  lst <- V.demandList "builtins.length" list
  pure $ V.mkInt $ fromIntegral $ V.listLength lst

-- | Get the first element of a list.
--
-- O(1) - uses underlying list implementation's head directly.
headNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
headNix list = do
  lst <- V.demandList "builtins.head" list
  case V.listHead lst of
    Nothing -> V.throwTypeError "builtins.head: empty list"
    Just x  -> pure x

-- | Get all elements after the first.
--
-- O(1) - uses underlying list implementation's tail (shares memory for Vector).
tailNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
tailNix list = do
  lst <- V.demandList "builtins.tail" list
  case V.listTail lst of
    Nothing -> V.throwTypeError "builtins.tail: empty list"
    Just tl
      | V.listNull tl -> pure V.internedEmptyList
      | otherwise     -> pure $ V.mkList tl

-- | Get the element at a given index.
--
-- O(1) - uses underlying list implementation's indexing directly.
elemAtNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemAtNix list n = do
  idx <- V.demandInt "builtins.elemAt" n
  lst <- V.demandList "builtins.elemAt" list
  case V.listElemAt lst (fromIntegral idx) of
    Nothing -> V.throwTypeError $ "builtins.elemAt: index " <> Text.pack (show idx) <>
                                  " too large for list of length " <> Text.pack (show (V.listLength lst))
    Just x -> pure x

-- * Predicates

-- | Short-circuit evaluation: returns True as soon as any element satisfies the predicate.
anyNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
anyNix pred list = do
  lst <- V.demandList "builtins.any" list
  if V.listNull lst
    then pure V.internedFalse
    else do
      -- Use foldr for short-circuit evaluation
      result <- foldr
        (\x acc -> do
          b <- V.demandBool "builtins.any" =<< V.callFunc pred x
          if b then pure True else acc)
        (pure False)
        lst
      pure $ V.internedBool result

-- | Short-circuit evaluation: returns False as soon as any element fails the predicate.
allNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
allNix pred list = do
  lst <- V.demandList "builtins.all" list
  if V.listNull lst
    then pure V.internedTrue
    else do
      result <- foldr
        (\x acc -> do
          b <- V.demandBool "builtins.all" =<< V.callFunc pred x
          if b then acc else pure False)
        (pure True)
        lst
      pure $ V.internedBool result

-- | Check if an element is in a list.
elemNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemNix x xs = do
  lst <- V.demandList "builtins.elem" xs
  if V.listNull lst
    then pure V.internedFalse
    else do
      result <- anyMVec (V.valueEq x) lst
      pure $ V.internedBool result
 where
  anyMVec :: (a -> m Bool) -> NixList a -> m Bool
  anyMVec p vec = go 0
   where
    len = V.listLength vec
    go i
      | i >= len  = pure False
      | otherwise = do
          ok <- p (V.listUnsafeElemAt vec i)
          if ok
            then pure True
            else go (i + 1)

-- * Transformations

-- | Map a function over a list.
mapNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapNix f list = do
  lst <- V.demandList "builtins.map" list
  if V.listNull lst
    then pure V.internedEmptyList
    else do
      result <- traverse (V.defer . V.callFunc f) lst
      pure $ V.mkList result

-- | Filter a list by a predicate.
filterNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
filterNix pred list = do
  lst <- V.demandList "builtins.filter" list
  if V.listNull lst
    then pure V.internedEmptyList
    else do
      result <- V.listFilterM applyPred lst
      if V.listNull result
        then pure V.internedEmptyList
        else pure $ V.mkList result
 where
  applyPred :: NValue t f m -> m Bool
  applyPred x = V.demandBool "builtins.filter" =<< V.callFunc pred x

-- | Strict left fold over a list.
foldl'Nix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
foldl'Nix op init list = do
  lst <- V.demandList "builtins.foldl'" list
  V.listFoldM' go init lst
 where
  go acc x = do
    op' <- V.callFunc op acc
    V.callFunc op' x

-- * Generation

-- | Generate a list by applying function to indices 0..n-1.
genListNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
genListNix generator len = do
  n <- V.demandInt "builtins.genList" len
  if n < 0
    then V.throwTypeError $ "builtins.genList: expected a non-negative number, got " <> Text.pack (show n)
    else if n == 0
      then pure V.internedEmptyList
      else do
        result <- V.listGenListM (fromIntegral n) genElement
        pure $ V.mkList result
 where
  genElement i = V.defer $ V.callFunc generator (V.mkInt $ fromIntegral i)

-- * Combining lists

-- | Concatenate a list of lists into a single list.
concatListsNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
concatListsNix lists = do
  outer <- V.demandList "builtins.concatLists" lists
  if V.listNull outer
    then pure V.internedEmptyList
    else do
      inner <- traverse (V.demandList "builtins.concatLists") outer
      let result = fold inner
      if V.listNull result
        then pure V.internedEmptyList
        else pure $ V.mkList result

-- | Map and concatenate.
concatMapNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
concatMapNix f list = do
  outer <- V.demandList "builtins.concatMap" list
  if V.listNull outer
    then pure V.internedEmptyList
    else do
      inner <- traverse (\x -> V.demandList "builtins.concatMap" =<< V.callFunc f x) outer
      let result = fold inner
      if V.listNull result
        then pure V.internedEmptyList
        else pure $ V.mkList result
