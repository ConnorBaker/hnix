{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Vector implementation of the NixList signature.
--
-- This implementation provides O(1) length and indexing,
-- making it optimal for Nix list operations that frequently
-- check length and access elements by index.
--
-- With Backpack, this module is selected at link time and the type
-- is monomorphized - no dictionary passing or indirection.
module Nix.List.Vector
  ( NixList
  -- Core operations
  , nlLength
  , nlIndex
  , nlUnsafeIndex
  , nlNull
  , nlUncons
  , nlCons
  , nlSnoc
  , nlAppend
  -- Conversion
  , nlFromList
  , nlToList
  -- Higher-order operations
  , nlMap
  , nlMapM
  , nlFilter
  , nlFilterM
  , nlMapMaybe
  , nlTraverse
  , nlReverse
  , nlEmpty
  , nlFoldl'
  , nlFoldM'
  , nlFoldr
  , nlHead
  , nlTail
  , nlGenerate
  , nlGenerateM
  , nlConcat
  , nlSingleton
  , nlUnsafeTail
  , nlFoldr'
  , nlPartition
  , nlPartitionM
  ) where

import           Relude
import           Prelude ()
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.DeepSeq ()
import           Data.Functor.Classes (Eq1(..), Show1(..), showsUnaryWith)
import           Data.Hashable (Hashable(..))
import           Data.Semialign (Semialign(..), Align(..))
import           Data.These (These(..))

-- | Concrete NixList type backed by Vector.
-- The newtype wrapper allows us to provide custom instances.
newtype NixList a = NixList { unNixList :: Vector a }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Functor, Foldable, Semigroup, Monoid)

-- Manual Traversable instance to work with newtype
instance Traversable NixList where
  traverse f (NixList v) = NixList <$> traverse f v
  {-# INLINE traverse #-}

instance NFData a => NFData (NixList a) where
  rnf (NixList v) = rnf v
  {-# INLINE rnf #-}

instance Hashable a => Hashable (NixList a) where
  hashWithSalt s (NixList v) = hashWithSalt s (V.toList v)
  {-# INLINE hashWithSalt #-}

instance Eq1 NixList where
  liftEq eq (NixList v1) (NixList v2) = liftEq eq v1 v2
  {-# INLINE liftEq #-}

instance Show1 NixList where
  liftShowsPrec sp sl d (NixList v) =
    showsUnaryWith (liftShowsPrec sp sl) "NixList" d v
  {-# INLINE liftShowsPrec #-}

instance Semialign NixList where
  align (NixList v1) (NixList v2) = NixList (align v1 v2)
  {-# INLINE align #-}
  alignWith f (NixList v1) (NixList v2) = NixList (alignWith f v1 v2)
  {-# INLINE alignWith #-}

instance Align NixList where
  nil = NixList V.empty
  {-# INLINE nil #-}

-- * Core operations

nlLength :: NixList a -> Int
nlLength (NixList v) = V.length v
{-# INLINE nlLength #-}

nlIndex :: NixList a -> Int -> Maybe a
nlIndex (NixList v) i = v V.!? i
{-# INLINE nlIndex #-}

nlUnsafeIndex :: NixList a -> Int -> a
nlUnsafeIndex (NixList v) i = V.unsafeIndex v i
{-# INLINE nlUnsafeIndex #-}

nlNull :: NixList a -> Bool
nlNull (NixList v) = V.null v
{-# INLINE nlNull #-}

nlUncons :: NixList a -> Maybe (a, NixList a)
nlUncons (NixList v) = case V.uncons v of
  Nothing -> Nothing
  Just (x, xs) -> Just (x, NixList xs)
{-# INLINE nlUncons #-}

nlCons :: a -> NixList a -> NixList a
nlCons x (NixList v) = NixList (V.cons x v)
{-# INLINE nlCons #-}

nlSnoc :: NixList a -> a -> NixList a
nlSnoc (NixList v) x = NixList (V.snoc v x)
{-# INLINE nlSnoc #-}

nlAppend :: NixList a -> NixList a -> NixList a
nlAppend (NixList v1) (NixList v2) = NixList (v1 V.++ v2)
{-# INLINE nlAppend #-}

-- * Conversion

nlFromList :: [a] -> NixList a
nlFromList = NixList . V.fromList
{-# INLINE nlFromList #-}

nlToList :: NixList a -> [a]
nlToList (NixList v) = V.toList v
{-# INLINE nlToList #-}

-- * Higher-order operations

nlMap :: (a -> b) -> NixList a -> NixList b
nlMap f (NixList v) = NixList (V.map f v)
{-# INLINE nlMap #-}

nlMapM :: Monad m => (a -> m b) -> NixList a -> m (NixList b)
nlMapM f (NixList v) = NixList <$> V.mapM f v
{-# INLINE nlMapM #-}

nlFilter :: (a -> Bool) -> NixList a -> NixList a
nlFilter p (NixList v) = NixList (V.filter p v)
{-# INLINE nlFilter #-}

nlFilterM :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a)
nlFilterM p (NixList v) = NixList <$> V.filterM p v
{-# INLINE nlFilterM #-}

nlMapMaybe :: (a -> Maybe b) -> NixList a -> NixList b
nlMapMaybe f (NixList v) = NixList (V.mapMaybe f v)
{-# INLINE nlMapMaybe #-}

nlTraverse :: Applicative m => (a -> m b) -> NixList a -> m (NixList b)
nlTraverse f (NixList v) = NixList <$> traverse f v
{-# INLINE nlTraverse #-}

nlReverse :: NixList a -> NixList a
nlReverse (NixList v) = NixList (V.reverse v)
{-# INLINE nlReverse #-}

nlEmpty :: NixList a
nlEmpty = NixList V.empty
{-# INLINE nlEmpty #-}

nlFoldl' :: (b -> a -> b) -> b -> NixList a -> b
nlFoldl' f z (NixList v) = V.foldl' f z v
{-# INLINE nlFoldl' #-}

nlFoldM' :: Monad m => (b -> a -> m b) -> b -> NixList a -> m b
nlFoldM' f z (NixList v) = V.foldM' f z v
{-# INLINE nlFoldM' #-}

nlFoldr :: (a -> b -> b) -> b -> NixList a -> b
nlFoldr f z (NixList v) = V.foldr f z v
{-# INLINE nlFoldr #-}

nlHead :: NixList a -> Maybe a
nlHead (NixList v)
  | V.null v  = Nothing
  | otherwise = Just (V.head v)
{-# INLINE nlHead #-}

nlTail :: NixList a -> Maybe (NixList a)
nlTail (NixList v)
  | V.null v  = Nothing
  | otherwise = Just (NixList (V.tail v))
{-# INLINE nlTail #-}

nlGenerate :: Int -> (Int -> a) -> NixList a
nlGenerate n f = NixList (V.generate n f)
{-# INLINE nlGenerate #-}

nlGenerateM :: Monad m => Int -> (Int -> m a) -> m (NixList a)
nlGenerateM n f = NixList <$> V.generateM n f
{-# INLINE nlGenerateM #-}

nlConcat :: [NixList a] -> NixList a
nlConcat xs = NixList (V.concat (unNixList <$> xs))
{-# INLINE nlConcat #-}

nlSingleton :: a -> NixList a
nlSingleton x = NixList (V.singleton x)
{-# INLINE nlSingleton #-}

nlUnsafeTail :: NixList a -> NixList a
nlUnsafeTail (NixList v) = NixList (V.unsafeTail v)
{-# INLINE nlUnsafeTail #-}

nlFoldr' :: (a -> b -> b) -> b -> NixList a -> b
nlFoldr' f z (NixList v) = V.foldr' f z v
{-# INLINE nlFoldr' #-}

nlPartition :: (a -> Bool) -> NixList a -> (NixList a, NixList a)
nlPartition p (NixList v) = let (a, b) = V.partition p v in (NixList a, NixList b)
{-# INLINE nlPartition #-}

nlPartitionM :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a, NixList a)
nlPartitionM p (NixList v) = do
  results <- V.mapM (\x -> (,x) <$> p x) v
  let (trues, falses) = V.partition fst results
  pure (NixList (snd <$> trues), NixList (snd <$> falses))
{-# INLINE nlPartitionM #-}
