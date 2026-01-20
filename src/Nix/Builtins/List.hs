{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | List manipulation builtins.
--
-- This module contains builtins that operate on lists:
-- length, head, tail, map, filter, any, all, foldl', elem, elemAt,
-- genList, sort, concatLists, concatMap, genericClosure, groupBy, partition.
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
  , sortNix
    -- * Generation
  , genListNix
    -- * Combining lists
  , concatListsNix
  , concatMapNix
    -- * Advanced operations
  , genericClosureNix
  , groupByNix
  , partitionNix
  ) where

import           Nix.Prelude
import           Data.List                      ( partition )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad.ListM            ( sortByM )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( ViewL(..), (><) )
import qualified Data.Set                      as S
import qualified Nix.Core.AttrSet              as A
import           Nix.Core.List                  ( NixList )
import qualified Nix.Core.List                 as L
import           Nix.Builtins.Internal          ( WValue(..)
                                                , attrsetGet
                                                )
import           Nix.Convert
import           Nix.Exec
import           Nix.Expr.Types                 ( AttrSet, mkVarName, emptyPositionSet )
import           Nix.Frames
import           Nix.Value
import           Nix.Value.Equal                ( valueEqM, checkComparable )
import           Nix.Value.Interned             ( internedEmptyList, internedEmptySet, internedBool, internedFalse )
import           Nix.Value.Monad


-- * Basic operations

-- | Get the length of a list.
--
-- O(1) - uses Vector directly without list conversion.
lengthNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
lengthNix nv = do
  v <- fromValue @(NixList (NValue t f m)) nv
  toValue (L.length v)

-- | Get the first element of a list.
--
-- O(1) - uses Vector directly without list conversion.
headNix :: forall e t f m. MonadNix e t f m => NValue t f m -> m (NValue t f m)
headNix nv = do
  v <- fromValue @(NixList (NValue t f m)) nv
  case L.head v of
    Nothing -> throwError $ ErrorCall "builtins.head: empty list"
    Just a -> pure a

-- | Get all elements after the first.
--
-- O(1) - uses Vector slice (L.unsafeTail shares memory).
tailNix :: forall e t f m. MonadNix e t f m => NValue t f m -> m (NValue t f m)
tailNix nv = do
  v <- fromValue @(NixList (NValue t f m)) nv
  case L.tail v of
    Nothing -> throwError $ ErrorCall "builtins.tail: empty list"
    -- Fast path: return interned empty list for singleton input
    Just t
      | L.null t  -> pure internedEmptyList
      | otherwise -> pure $ NVList t

-- | Get the element at a given index.
--
-- O(1) - uses Vector indexing directly.
elemAtNix
  :: forall e t f m . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemAtNix xs n = do
  n' <- fromValue @Int n
  v <- fromValue @(NixList (NValue t f m)) xs
  case L.elemAt v n' of
    Nothing -> throwError $ ErrorCall $ "builtins.elemAt: Index " <> show n' <> " too large for list of length " <> show (L.length v)
    Just v' -> pure v'


-- * Predicates

-- | Short-circuit evaluation: returns True as soon as any element satisfies the predicate.
-- Uses L.foldr with lazy accumulator to avoid evaluating remaining elements.
-- Returns interned boolean for zero-allocation result.
anyNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
anyNix f nvList = do
  vec <- fromValue @(NixList (NValue t f m)) nvList
  -- Use Foldable's foldr for short-circuit evaluation
  result <- foldr
    (\x acc -> do
      r <- callFunc f x >>= fromValue
      if r then pure True else acc)
    (pure False)
    vec
  pure $ internedBool result

-- | Short-circuit evaluation: returns False as soon as any element fails the predicate.
-- Uses L.foldr with lazy accumulator to avoid evaluating remaining elements.
-- Returns interned boolean for zero-allocation result.
allNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
allNix f nvList = do
  vec <- fromValue @(NixList (NValue t f m)) nvList
  -- Use Foldable's foldr for short-circuit evaluation
  result <- foldr
    (\x acc -> do
      r <- callFunc f x >>= fromValue
      if r then acc else pure False)
    (pure True)
    vec
  pure $ internedBool result

-- | Check if an element is in a list.
-- Returns interned false immediately for empty list (fast path).
-- Returns interned boolean for zero-allocation result.
elemNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemNix x lst = do
  -- Use Vector directly to avoid list conversion overhead
  vec <- fromValue @(NixList (NValue t f m)) lst
  -- Fast path: empty list always returns false
  if L.null vec
    then pure internedFalse
    else pure . internedBool =<< anyMVec (valueEqM x) vec
 where
  -- | Short-circuiting any for NixList - O(1) indexing, no list conversion
  anyMVec :: Monad m => (a -> m Bool) -> NixList a -> m Bool
  anyMVec p v = go 0
   where
    n = L.length v
    go i
      | i >= n    = pure False
      | otherwise = do
          ok <- p (L.unsafeElemAt v i)
          if ok
            then pure True
            else go (i + 1)


-- * Transformations

-- | Map a function over a list.
--
-- Uses Vector traverse directly without list conversion.
mapNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapNix f nv = do
  v <- fromValue @(NixList (NValue t f m)) nv
  -- Fast path: return interned empty list for empty input
  if L.null v
    then pure internedEmptyList
    else do
      result <- traverse
        (defer . withFrame Debug (ErrorCall "While applying f in map:\n") . callFunc f)
        v
      toValue result

-- | Filter a list by a predicate.
--
-- Uses Vector filterM directly without list conversion.
filterNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
filterNix f nv = do
  v <- fromValue @(NixList (NValue t f m)) nv
  -- Fast path: return interned empty list for empty input
  if L.null v
    then pure internedEmptyList
    else do
      result <- L.filterM predicate v
      -- Fast path: return interned empty list if all elements filtered out
      if L.null result
        then pure internedEmptyList
        else toValue result
 where
  predicate :: NValue t f m -> m Bool
  predicate = fromValue <=< callFunc f

-- | Strict left fold over a list.
foldl'Nix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
foldl'Nix f z xs = do
  v <- fromValue @(NixList (NValue t f m)) xs
  L.foldM' go z v
 where
  go b a = (`callFunc` a) =<< callFunc f b

-- | Sort a list using a comparison function.
sortNix
  :: forall e t f m
  . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
sortNix comp nv = do
  v <- fromValue @[NValue t f m] nv
  -- Fast path: return interned empty list for empty/singleton input
  case v of
    []  -> pure internedEmptyList
    [x] -> toValue [x]  -- singleton doesn't need sorting
    _   -> toValue =<< sortByM cmp v
 where
  cmp :: NValue t f m -> NValue t f m -> m Ordering
  cmp a b = do
    ab <- compare a b
    if ab
      then pure LT
      else do
        ba <- compare b a
        if ba
          then pure GT
          else pure EQ
   where
    compare :: NValue t f m -> NValue t f m -> m Bool
    compare a2 a1 = fromValue =<< (`callFunc` a1) =<< callFunc comp a2


-- * Generation

-- | Generate a list by applying function to indices 0..n-1.
-- Uses L.genListM to build Vector directly without intermediate list allocation.
genListNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
genListNix f nixN =
  do
    n <- fromValue @Integer nixN
    if n < 0
      then throwError $ ErrorCall $ "builtins.genList: Expected a non-negative number, got " <> show n
      -- Fast path: return interned empty list for n=0
      else if n == 0
        then pure internedEmptyList
        else toValue =<< L.genListM (fromIntegral n) genElement
 where
  genElement i = defer $ callFunc f =<< toValue (fromIntegral i :: Integer)


-- * Combining lists

-- | Helper function, generalization of @concat@ operations.
-- Uses Vector directly to avoid list conversion overhead.
concatWith
  :: forall e t f m
   . MonadNix e t f m
  => (NValue t f m -> m (NValue t f m))
  -> NValue t f m
  -> m (NValue t f m)
concatWith f nv = do
  outerVec <- fromValue @(NixList (NValue t f m)) nv
  -- Fast path: return interned empty list for empty input
  if L.null outerVec
    then pure internedEmptyList
    else do
      innerVecs <- traverse (fromValue @(NixList (NValue t f m)) <=< f) outerVec
      -- Use fold instead of L.concat to avoid intermediate list allocation
      let result = fold innerVecs
      -- Fast path: return interned empty list if result is empty
      if L.null result
        then pure internedEmptyList
        else pure $ NVList result

-- | Nix function of Haskell:
-- > concat :: [[a]] -> [a]
--
-- Concatenate a list of lists into a single list.
concatListsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
concatListsNix = concatWith demand

-- | Nix function of Haskell:
-- > concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMapNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
concatMapNix f = concatWith (callFunc f)


-- * Advanced operations

-- | Generic closure computation.
-- Takes a set with 'startSet' (list) and 'operator' (function).
-- Iteratively applies operator to expand the set until no new elements are found.
genericClosureNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
genericClosureNix c =
  do
  s <- fromValue @(AttrSet (NValue t f m)) c

  case (A.lookup (mkVarName "startSet") s, A.lookup (mkVarName "operator") s) of
    (Nothing    , Nothing        ) -> throwError $ ErrorCall "builtins.genericClosure: Attributes 'startSet' and 'operator' required"
    (Nothing    , Just _         ) -> throwError $ ErrorCall "builtins.genericClosure: Attribute 'startSet' required"
    (Just _     , Nothing        ) -> throwError $ ErrorCall "builtins.genericClosure: Attribute 'operator' required"
    (Just startSet, Just operator) ->
      do
        -- Get startSet as Vector, convert to Seq for O(log n) worklist operations
        ssVec <- fromValue @(NixList (NValue t f m)) =<< demand startSet
        op <- demand operator
        let
          -- Use Seq for O(log n) concat instead of O(n) list concat
          go
            :: Set (WValue t f m)
            -> Seq (NValue t f m)
            -> m (Set (WValue t f m), Seq (NValue t f m))
          go ks worklist = case Seq.viewl worklist of
            EmptyL -> pure (ks, mempty)
            t :< ts ->
              do
                v <- demand t
                k <- demand =<< attrsetGet "key" =<< fromValue @(AttrSet (NValue t f m)) v

                if S.member (WValue k) ks
                  then go ks ts
                  else do
                    checkComparable k $
                      handlePresence
                        k
                        (\ (WValue j:_) -> j)
                        (S.toList ks)

                    -- Get operator result as Vector, append to worklist with O(log n) concat
                    opResult <- fromValue @(NixList (NValue t f m)) =<< callFunc op v
                    (<<$>>) (v Seq.<|) . go (S.insert (WValue k) ks) $ ts >< Seq.fromList (L.toList opResult)

        -- Convert result Seq to Vector
        (NVList . L.fromList . toList) . snd <$> go mempty (Seq.fromList (L.toList ssVec))

-- | Groups elements of list together by the string returned from the function f called on
-- each element. It returns an attribute set where each attribute value contains the
-- elements of list that are mapped to the same corresponding attribute name returned by f.
groupByNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
groupByNix nvfun nvlist = do
  list   <- demand nvlist
  fun    <- demand nvfun
  (f, v) <- extractP (fun, list)
  -- Fast path: return interned empty set for empty input
  if L.null v
    then pure internedEmptySet
    else do
      -- Build up groups maintaining order: old ++ new (flip because insertWith passes new first)
      result <- L.foldM'
        (\acc x -> do
          name <- mkVarName <$> (fromValue @Text =<< f x)
          pure $ A.insertWith (flip (L.append)) name (L.singleton x) acc
        )
        mempty
        v
      pure $ NVSet emptyPositionSet $ fmap NVList result
 where
  extractP (NVBuiltin _ f, NVList l) = pure (f, l)
  extractP (NVClosure _ f, NVList l) = pure (f, l)
  extractP _v =
    throwError
      $  ErrorCall
      $  "builtins.groupBy: expected function and list, got "
      <> show _v

-- | Partition a list into two lists based on a predicate.
-- Returns an attrset with 'right' (elements that satisfy) and 'wrong' (elements that don't).
partitionNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
partitionNix f nvlst =
  do
    v <- fromValue @(NixList (NValue t f m)) nvlst
    emptyList <- pure internedEmptyList

    -- Fast path: return interned empty lists for empty input
    if L.null v
      then toValue @(AttrSet (NValue t f m))
        $ A.fromList
            [ (mkVarName "right", emptyList)
            , (mkVarName "wrong", emptyList)
            ]
      else do
        -- Get (Bool, value) pairs
        selection <- traverse (\t -> (, t) <$> (fromValue =<< callFunc f t)) v

        let
          -- Use Data.List.partition on converted list, then convert back
          (rightPairs, wrongPairs) = partition fst (L.toList selection)
          right = L.fromList rightPairs
          wrong = L.fromList wrongPairs
          -- Use interned empty list when a partition is empty
          rightList = if L.null right then emptyList else NVList $ fmap snd right
          wrongList = if L.null wrong then emptyList else NVList $ fmap snd wrong

        toValue @(AttrSet (NValue t f m))
          $ A.fromList
              [ (mkVarName "right", rightList)
              , (mkVarName "wrong", wrongList)
              ]
