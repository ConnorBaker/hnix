
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
  -- Monadic operations for Nix builtins
  , filterM
  , foldM'
  , genListM
  , partitionM
  -- Core operations
  , empty
  , null
  , length
  , elemAt
  , unsafeElemAt
  , head
  , tail
  , unsafeTail
  , singleton
  , uncons
  , cons
  , snoc
  , append
  , fromList
  , toList
  , reverse
  ) where

import           Relude hiding (empty, fromList, toList, null, head, tail, reverse, length, filterM, uncons)
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

-- * Monadic operations for Nix builtins

-- | Monadic filter by a predicate.
filterM :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a)
filterM p (NixList v) = NixList <$> V.filterM p v
{-# INLINE filterM #-}

-- | Strict monadic left fold.
foldM' :: Monad m => (b -> a -> m b) -> b -> NixList a -> m b
foldM' f z (NixList v) = V.foldM' f z v
{-# INLINE foldM' #-}

-- | Monadic generation of a list.
genListM :: Monad m => Int -> (Int -> m a) -> m (NixList a)
genListM n f = NixList <$> V.generateM n f
{-# INLINE genListM #-}

-- | Partition a list by a monadic predicate.
partitionM :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a, NixList a)
partitionM p (NixList v) = do
  results <- V.mapM (\x -> (,x) <$> p x) v
  let (trues, falses) = V.partition fst results
  pure (NixList (snd <$> trues), NixList (snd <$> falses))
{-# INLINE partitionM #-}

-- * Core operations

-- | The empty list.
empty :: NixList a
empty = NixList V.empty
{-# INLINE empty #-}

-- | Check if the list is empty.
null :: NixList a -> Bool
null (NixList v) = V.null v
{-# INLINE null #-}

-- | Get the length of the list.
length :: NixList a -> Int
length (NixList v) = V.length v
{-# INLINE length #-}

-- | Safe indexing by position.
elemAt :: NixList a -> Int -> Maybe a
elemAt (NixList v) i = v V.!? i
{-# INLINE elemAt #-}

-- | Unsafe indexing by position.
unsafeElemAt :: NixList a -> Int -> a
unsafeElemAt (NixList v) i = V.unsafeIndex v i
{-# INLINE unsafeElemAt #-}

-- | Get the first element.
head :: NixList a -> Maybe a
head (NixList v)
  | V.null v  = Nothing
  | otherwise = Just (V.head v)
{-# INLINE head #-}

-- | Get all elements except the first.
tail :: NixList a -> Maybe (NixList a)
tail (NixList v)
  | V.null v  = Nothing
  | otherwise = Just (NixList (V.tail v))
{-# INLINE tail #-}

-- | Unsafe tail - undefined behavior if list is empty.
unsafeTail :: NixList a -> NixList a
unsafeTail (NixList v) = NixList (V.unsafeTail v)
{-# INLINE unsafeTail #-}

-- | Create a singleton list.
singleton :: a -> NixList a
singleton x = NixList (V.singleton x)
{-# INLINE singleton #-}

-- | Decompose into head and tail. Returns Nothing if empty.
uncons :: NixList a -> Maybe (a, NixList a)
uncons (NixList v) = case V.uncons v of
  Nothing -> Nothing
  Just (x, xs) -> Just (x, NixList xs)
{-# INLINE uncons #-}

-- | Prepend an element.
cons :: a -> NixList a -> NixList a
cons x (NixList v) = NixList (V.cons x v)
{-# INLINE cons #-}

-- | Append an element.
snoc :: NixList a -> a -> NixList a
snoc (NixList v) x = NixList (V.snoc v x)
{-# INLINE snoc #-}

-- | Concatenate two lists.
append :: NixList a -> NixList a -> NixList a
append (NixList v1) (NixList v2) = NixList (v1 V.++ v2)
{-# INLINE append #-}

-- | Build a list from a Haskell list.
fromList :: [a] -> NixList a
fromList = NixList . V.fromList
{-# INLINE fromList #-}

-- | Convert to a Haskell list.
toList :: NixList a -> [a]
toList (NixList v) = V.toList v
{-# INLINE toList #-}

-- | Reverse the list.
reverse :: NixList a -> NixList a
reverse (NixList v) = NixList (V.reverse v)
{-# INLINE reverse #-}
