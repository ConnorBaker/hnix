{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

-- Import from concrete value-core and abstract list signature
import           Nix.List.Sig                   ( NixList )
import           Nix.Core.Value.Protocol        ( NValue )
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
lengthNix nv = do
  v <- V.demand nv
  case V.extractList v of
    Just lst -> pure $ V.mkInt $ fromIntegral $ V.listLength lst
    Nothing  -> V.throwTypeError "builtins.length: expected a list"

-- | Get the first element of a list.
--
-- O(1) - uses underlying list implementation's head directly.
headNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
headNix nv = do
  v <- V.demand nv
  case V.extractList v of
    Just lst -> case V.listHead lst of
      Nothing -> V.throwTypeError "builtins.head: empty list"
      Just a  -> pure a
    Nothing -> V.throwTypeError "builtins.head: expected a list"

-- | Get all elements after the first.
--
-- O(1) - uses underlying list implementation's tail (shares memory for Vector).
tailNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
tailNix nv = do
  v <- V.demand nv
  case V.extractList v of
    Just lst -> case V.listTail lst of
      Nothing -> V.throwTypeError "builtins.tail: empty list"
      Just t
        | V.listNull t  -> pure V.internedEmptyList
        | otherwise     -> pure $ V.mkList t
    Nothing -> V.throwTypeError "builtins.tail: expected a list"

-- | Get the element at a given index.
--
-- O(1) - uses underlying list implementation's indexing directly.
elemAtNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemAtNix xs n = do
  n' <- V.demand n
  xs' <- V.demand xs
  case V.extractInt n' of
    Nothing -> V.throwTypeError "builtins.elemAt: second argument must be an integer"
    Just i -> case V.extractList xs' of
      Nothing -> V.throwTypeError "builtins.elemAt: first argument must be a list"
      Just lst -> case V.listElemAt lst (fromIntegral i) of
        Nothing -> V.throwTypeError $ "builtins.elemAt: index " <> Text.pack (show i) <>
                                      " too large for list of length " <> Text.pack (show (V.listLength lst))
        Just v' -> pure v'

-- * Predicates

-- | Short-circuit evaluation: returns True as soon as any element satisfies the predicate.
anyNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
anyNix f nvList = do
  v <- V.demand nvList
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.any: second argument must be a list"
    Just lst
      | V.listNull lst -> pure V.internedFalse
      | otherwise  -> do
          -- Use foldr for short-circuit evaluation
          result <- foldr
            (\x acc -> do
              r <- V.callFunc f x
              r' <- V.demand r
              case V.extractBool r' of
                Just True -> pure True
                Just False -> acc
                Nothing -> V.throwTypeError "builtins.any: predicate must return a boolean")
            (pure False)
            lst
          pure $ V.internedBool result

-- | Short-circuit evaluation: returns False as soon as any element fails the predicate.
allNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
allNix f nvList = do
  v <- V.demand nvList
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.all: second argument must be a list"
    Just lst
      | V.listNull lst -> pure V.internedTrue
      | otherwise  -> do
          result <- foldr
            (\x acc -> do
              r <- V.callFunc f x
              r' <- V.demand r
              case V.extractBool r' of
                Just True -> acc
                Just False -> pure False
                Nothing -> V.throwTypeError "builtins.all: predicate must return a boolean")
            (pure True)
            lst
          pure $ V.internedBool result

-- | Check if an element is in a list.
elemNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemNix x lst = do
  v <- V.demand lst
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.elem: second argument must be a list"
    Just vec
      | V.listNull vec -> pure V.internedFalse
      | otherwise  -> do
          result <- anyMVec (V.valueEq x) vec
          pure $ V.internedBool result
 where
  anyMVec :: (a -> m Bool) -> NixList a -> m Bool
  anyMVec p v = go 0
   where
    n = V.listLength v
    go i
      | i >= n    = pure False
      | otherwise = do
          ok <- p (V.listUnsafeElemAt v i)
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
mapNix f nv = do
  v <- V.demand nv
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.map: second argument must be a list"
    Just lst
      | V.listNull lst -> pure V.internedEmptyList
      | otherwise  -> do
          result <- traverse (V.defer . V.callFunc f) lst
          pure $ V.mkList result

-- | Filter a list by a predicate.
filterNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
filterNix f nv = do
  v <- V.demand nv
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.filter: second argument must be a list"
    Just lst
      | V.listNull lst -> pure V.internedEmptyList
      | otherwise  -> do
          result <- V.listFilterM predicate lst
          if V.listNull result
            then pure V.internedEmptyList
            else pure $ V.mkList result
 where
  predicate :: NValue t f m -> m Bool
  predicate x = do
    r <- V.callFunc f x
    r' <- V.demand r
    case V.extractBool r' of
      Just b  -> pure b
      Nothing -> V.throwTypeError "builtins.filter: predicate must return a boolean"

-- | Strict left fold over a list.
foldl'Nix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
foldl'Nix f z xs = do
  v <- V.demand xs
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.foldl': third argument must be a list"
    Just lst -> V.listFoldM' go z lst
 where
  go b a = do
    f' <- V.callFunc f b
    V.callFunc f' a

-- * Generation

-- | Generate a list by applying function to indices 0..n-1.
genListNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
genListNix f nixN = do
  n' <- V.demand nixN
  case V.extractInt n' of
    Nothing -> V.throwTypeError "builtins.genList: second argument must be an integer"
    Just n
      | n < 0     -> V.throwTypeError $ "builtins.genList: expected a non-negative number, got " <> Text.pack (show n)
      | n == 0    -> pure V.internedEmptyList
      | otherwise -> do
          result <- V.listGenListM (fromIntegral n) genElement
          pure $ V.mkList result
 where
  genElement i = V.defer $ V.callFunc f (V.mkInt $ fromIntegral i)

-- * Combining lists

-- | Concatenate a list of lists into a single list.
concatListsNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> m (NValue t f m)
concatListsNix nv = do
  v <- V.demand nv
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.concatLists: argument must be a list"
    Just outerLst
      | V.listNull outerLst -> pure V.internedEmptyList
      | otherwise       -> do
          innerLists <- traverse extractInner outerLst
          let result = fold innerLists
          if V.listNull result
            then pure V.internedEmptyList
            else pure $ V.mkList result
 where
  extractInner nv' = do
    v' <- V.demand nv'
    case V.extractList v' of
      Just lst -> pure lst
      Nothing  -> V.throwTypeError "builtins.concatLists: element is not a list"

-- | Map and concatenate.
concatMapNix
  :: forall t f m . MonadListBuiltin t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
concatMapNix f nv = do
  v <- V.demand nv
  case V.extractList v of
    Nothing -> V.throwTypeError "builtins.concatMap: second argument must be a list"
    Just outerLst
      | V.listNull outerLst -> pure V.internedEmptyList
      | otherwise       -> do
          innerLists <- traverse (\x -> extractInner =<< V.callFunc f x) outerLst
          let result = fold innerLists
          if V.listNull result
            then pure V.internedEmptyList
            else pure $ V.mkList result
 where
  extractInner nv' = do
    v' <- V.demand nv'
    case V.extractList v' of
      Just lst -> pure lst
      Nothing  -> V.throwTypeError "builtins.concatMap: function must return a list"
