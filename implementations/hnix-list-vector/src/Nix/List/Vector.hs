{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , nlFilter
  , nlTraverse
  , nlReverse
  , nlEmpty
  , nlFoldl'
  , nlHead
  , nlTail
  ) where

import           Relude
import           Prelude ()
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.DeepSeq ()

-- | Concrete NixList type backed by Vector.
-- The newtype wrapper allows us to provide custom instances.
newtype NixList a = NixList { unNixList :: Vector a }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Functor, Foldable, Semigroup, Monoid)

-- Manual Traversable instance to work with newtype
instance Traversable NixList where
  traverse f (NixList v) = NixList <$> traverse f v
  {-# INLINE traverse #-}

instance NFData a => NFData (NixList a) where
  rnf (NixList v) = rnf v
  {-# INLINE rnf #-}

-- * Core operations

nlLength :: NixList a -> Int
nlLength (NixList v) = V.length v
{-# INLINE nlLength #-}

nlIndex :: NixList a -> Int -> Maybe a
nlIndex (NixList v) i = v V.!? i
{-# INLINE nlIndex #-}

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

nlFilter :: (a -> Bool) -> NixList a -> NixList a
nlFilter p (NixList v) = NixList (V.filter p v)
{-# INLINE nlFilter #-}

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
