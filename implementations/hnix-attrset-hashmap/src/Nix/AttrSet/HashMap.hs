{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | HashMap implementation of the AttrSet signature.
--
-- This implementation provides O(1) average-case lookup and insertion,
-- making it ideal for the typical Nix evaluation workload.
--
-- With Backpack, this module is selected at link time and the type
-- is monomorphized - no dictionary passing or indirection.
module Nix.AttrSet.HashMap
  ( AttrSet
  -- Core operations
  , empty
  , singleton
  , insert
  , delete
  , lookup
  , member
  -- Bulk operations
  , union
  , unionWith
  , intersection
  , difference
  -- Conversion
  , fromList
  , toList
  , keys
  , elems
  -- Properties
  , null
  , size
  -- Higher-order operations
  , mapWithKey
  , traverseWithKey
  , foldlWithKey'
  , filterWithKey
  , alterF
  ) where

import           Relude hiding (empty, fromList, toList, null)
import           Prelude ()
import           Nix.Types.VarName (VarName)
import qualified Codec.Serialise as Serialise
import           Codec.Serialise (Serialise)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import           Data.Data (Data(..), mkNoRepType)
import           Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM
import           Control.DeepSeq ()
import           Data.Hashable ()
import qualified Text.Read as Read

-- | Concrete AttrSet type backed by HashMap.
-- The newtype wrapper allows us to provide custom instances.
newtype AttrSet a = AttrSet { unAttrSet :: HashMap VarName a }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Functor, Foldable, Semigroup, Monoid)

-- Manual Traversable instance to work with newtype
instance Traversable AttrSet where
  traverse f (AttrSet m) = AttrSet <$> traverse f m
  {-# INLINE traverse #-}

instance NFData a => NFData (AttrSet a) where
  rnf (AttrSet m) = rnf m
  {-# INLINE rnf #-}

instance Hashable a => Hashable (AttrSet a) where
  hashWithSalt s (AttrSet m) = hashWithSalt s (HM.toList m)
  {-# INLINE hashWithSalt #-}

-- | Ord instance via sorted list comparison for deterministic ordering
instance Ord a => Ord (AttrSet a) where
  compare (AttrSet m1) (AttrSet m2) = compare (sortOn fst $ HM.toList m1) (sortOn fst $ HM.toList m2)
  {-# INLINE compare #-}

-- | Read instance via list parsing
instance Read a => Read (AttrSet a) where
  readsPrec d = map (\(l, r) -> (fromList l, r)) . Read.readsPrec d

-- | Serialise via sorted list for deterministic serialization
instance Serialise a => Serialise (AttrSet a) where
  encode (AttrSet m) = Serialise.encode (sortOn fst $ HM.toList m)
  decode = AttrSet . HM.fromList <$> Serialise.decode
  {-# INLINE encode #-}
  {-# INLINE decode #-}

-- | Binary via sorted list for deterministic serialization
instance Binary a => Binary (AttrSet a) where
  put (AttrSet m) = Binary.put (sortOn fst $ HM.toList m)
  get = AttrSet . HM.fromList <$> Binary.get
  {-# INLINE put #-}
  {-# INLINE get #-}

-- | JSON serialization as object
instance ToJSON a => ToJSON (AttrSet a) where
  toJSON (AttrSet m) = Aeson.toJSON m
  toEncoding (AttrSet m) = Aeson.toEncoding m
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (AttrSet a) where
  parseJSON v = AttrSet <$> Aeson.parseJSON v
  {-# INLINE parseJSON #-}

-- | Data instance for AttrSet - enables generic programming (SYB).
-- Represents AttrSet as an abstract type with list-based construction.
instance (Data a, Typeable a) => Data (AttrSet a) where
  gfoldl f z (AttrSet m) = z (AttrSet . HM.fromList) `f` HM.toList m
  gunfold k z _ = k (z (AttrSet . HM.fromList))
  toConstr _ = error "toConstr: AttrSet is abstract"
  dataTypeOf _ = mkNoRepType "Nix.AttrSet.HashMap.AttrSet"
  {-# INLINE gfoldl #-}
  {-# INLINE gunfold #-}

-- * Core operations

empty :: AttrSet a
empty = AttrSet HM.empty
{-# INLINE empty #-}

singleton :: VarName -> a -> AttrSet a
singleton k v = AttrSet (HM.singleton k v)
{-# INLINE singleton #-}

insert :: VarName -> a -> AttrSet a -> AttrSet a
insert k v (AttrSet m) = AttrSet (HM.insert k v m)
{-# INLINE insert #-}

delete :: VarName -> AttrSet a -> AttrSet a
delete k (AttrSet m) = AttrSet (HM.delete k m)
{-# INLINE delete #-}

lookup :: VarName -> AttrSet a -> Maybe a
lookup k (AttrSet m) = HM.lookup k m
{-# INLINE lookup #-}

member :: VarName -> AttrSet a -> Bool
member k (AttrSet m) = HM.member k m
{-# INLINE member #-}

-- * Bulk operations

union :: AttrSet a -> AttrSet a -> AttrSet a
union (AttrSet m1) (AttrSet m2) = AttrSet (HM.union m1 m2)
{-# INLINE union #-}

unionWith :: (a -> a -> a) -> AttrSet a -> AttrSet a -> AttrSet a
unionWith f (AttrSet m1) (AttrSet m2) = AttrSet (HM.unionWith f m1 m2)
{-# INLINE unionWith #-}

intersection :: AttrSet a -> AttrSet b -> AttrSet a
intersection (AttrSet m1) (AttrSet m2) = AttrSet (HM.intersection m1 m2)
{-# INLINE intersection #-}

difference :: AttrSet a -> AttrSet b -> AttrSet a
difference (AttrSet m1) (AttrSet m2) = AttrSet (HM.difference m1 m2)
{-# INLINE difference #-}

-- * Conversion

fromList :: [(VarName, a)] -> AttrSet a
fromList = AttrSet . HM.fromList
{-# INLINE fromList #-}

toList :: AttrSet a -> [(VarName, a)]
toList (AttrSet m) = HM.toList m
{-# INLINE toList #-}

keys :: AttrSet a -> [VarName]
keys (AttrSet m) = HM.keys m
{-# INLINE keys #-}

elems :: AttrSet a -> [a]
elems (AttrSet m) = HM.elems m
{-# INLINE elems #-}

-- * Properties

null :: AttrSet a -> Bool
null (AttrSet m) = HM.null m
{-# INLINE null #-}

size :: AttrSet a -> Int
size (AttrSet m) = HM.size m
{-# INLINE size #-}

-- * Higher-order operations

mapWithKey :: (VarName -> a -> b) -> AttrSet a -> AttrSet b
mapWithKey f (AttrSet m) = AttrSet (HM.mapWithKey f m)
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative f => (VarName -> a -> f b) -> AttrSet a -> f (AttrSet b)
traverseWithKey f (AttrSet m) = AttrSet <$> HM.traverseWithKey f m
{-# INLINE traverseWithKey #-}

foldlWithKey' :: (b -> VarName -> a -> b) -> b -> AttrSet a -> b
foldlWithKey' f z (AttrSet m) = HM.foldlWithKey' f z m
{-# INLINE foldlWithKey' #-}

filterWithKey :: (VarName -> a -> Bool) -> AttrSet a -> AttrSet a
filterWithKey p (AttrSet m) = AttrSet (HM.filterWithKey p m)
{-# INLINE filterWithKey #-}

alterF :: Functor f => (Maybe a -> f (Maybe a)) -> VarName -> AttrSet a -> f (AttrSet a)
alterF f k (AttrSet m) = AttrSet <$> HM.alterF f k m
{-# INLINE alterF #-}
