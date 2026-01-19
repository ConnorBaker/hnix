{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
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
  , insertWith
  , intersection
  , intersectionWith
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
  , mapMaybe
  , alterF
  ) where

import           Relude hiding (empty, fromList, toList, null, mapMaybe)
import           Prelude ()
import           Nix.Types.VarName (VarName)
import qualified Codec.Serialise as Serialise
import           Codec.Serialise (Serialise)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON(..), FromJSON(..), ToJSON1(..), FromJSON1(..))
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import           Data.Data (Data(..), Constr, DataType, mkConstr, mkDataType, Fixity(..))
import           Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HM
import           Control.DeepSeq (NFData1(..))
import           Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), Read1(..))
import           Data.Hashable ()
import           Data.Hashable.Lifted (Hashable1(..))
import           Data.Functor.WithIndex (FunctorWithIndex(..))
import           Data.Foldable.WithIndex (FoldableWithIndex(..))
import           Data.Traversable.WithIndex (TraversableWithIndex(..))
import           Data.Semialign (Semialign(..), Align(..))
import           Data.Semialign.Indexed (SemialignWithIndex(..))
import qualified Text.Read as Read
import qualified Language.Haskell.TH.Syntax as TH
import           Language.Haskell.TH.Syntax (Lift(..), Exp(..), unsafeCodeCoerce)

-- | Concrete AttrSet type backed by HashMap.
-- The newtype wrapper allows us to provide custom instances.
newtype AttrSet a = AttrSet { unAttrSet :: HashMap VarName a }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Functor, Foldable, Semigroup, Monoid)

-- Manual Traversable instance to work with newtype
instance Traversable AttrSet where
  traverse f (AttrSet m) = AttrSet <$> traverse f m
  {-# INLINE traverse #-}

-- | FunctorWithIndex instance for indexed mapping
instance FunctorWithIndex VarName AttrSet where
  imap f (AttrSet m) = AttrSet (imap f m)
  {-# INLINE imap #-}

-- | FoldableWithIndex instance for indexed folding
instance FoldableWithIndex VarName AttrSet where
  ifoldMap f (AttrSet m) = ifoldMap f m
  {-# INLINE ifoldMap #-}

-- | TraversableWithIndex instance for indexed traversal
instance TraversableWithIndex VarName AttrSet where
  itraverse f (AttrSet m) = AttrSet <$> itraverse f m
  {-# INLINE itraverse #-}

instance NFData a => NFData (AttrSet a) where
  rnf (AttrSet m) = rnf m
  {-# INLINE rnf #-}

instance Hashable a => Hashable (AttrSet a) where
  hashWithSalt s (AttrSet m) = hashWithSalt s (HM.toList m)
  {-# INLINE hashWithSalt #-}

-- | Lift instance for Template Haskell support.
-- Uses fromList to avoid exposing the AttrSet constructor.
-- We use liftData rather than a TH splice to ensure proper code generation.
instance (Lift a, Typeable a, Data a) => Lift (AttrSet a) where
  lift (AttrSet m) = do
    listExpr <- TH.lift (HM.toList m)
    pure $ AppE (VarE 'fromList) listExpr
  liftTyped x = unsafeCodeCoerce (TH.lift x)

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
-- Uses list-based representation for proper lifting support.
attrSetConstr :: Constr
attrSetConstr = mkConstr attrSetDataType "AttrSet" [] Prefix

attrSetDataType :: DataType
attrSetDataType = mkDataType "Nix.AttrSet.HashMap.AttrSet" [attrSetConstr]

instance (Data a, Typeable a) => Data (AttrSet a) where
  gfoldl f z (AttrSet m) = z (AttrSet . HM.fromList) `f` HM.toList m
  gunfold k z _ = k (z (AttrSet . HM.fromList))
  toConstr (AttrSet _) = attrSetConstr
  dataTypeOf _ = attrSetDataType
  {-# INLINE gfoldl #-}
  {-# INLINE gunfold #-}

-- | NFData1 for deriving NFData1 on types containing AttrSet
instance NFData1 AttrSet where
  liftRnf f (AttrSet m) = liftRnf f m
  {-# INLINE liftRnf #-}

-- | ToJSON1 for deriving ToJSON1 on types containing AttrSet
instance ToJSON1 AttrSet where
  liftToJSON omit toJ toJList (AttrSet m) = Aeson.liftToJSON omit toJ toJList m
  liftToEncoding omit toE toEList (AttrSet m) = Aeson.liftToEncoding omit toE toEList m
  {-# INLINE liftToJSON #-}
  {-# INLINE liftToEncoding #-}

-- | FromJSON1 for deriving FromJSON1 on types containing AttrSet
instance FromJSON1 AttrSet where
  liftParseJSON maybeParser pJ pJList v = AttrSet <$> Aeson.liftParseJSON maybeParser pJ pJList v
  {-# INLINE liftParseJSON #-}

-- | Eq1 for deriving Eq1 on types containing AttrSet
instance Eq1 AttrSet where
  liftEq f (AttrSet m1) (AttrSet m2) = liftEq f m1 m2
  {-# INLINE liftEq #-}

-- | Ord1 for deriving Ord1 on types containing AttrSet
instance Ord1 AttrSet where
  liftCompare f (AttrSet m1) (AttrSet m2) = liftCompare f m1 m2
  {-# INLINE liftCompare #-}

-- | Show1 for deriving Show1 on types containing AttrSet
instance Show1 AttrSet where
  liftShowsPrec sp sl d (AttrSet m) = liftShowsPrec sp sl d m
  {-# INLINE liftShowsPrec #-}

-- | Read1 for deriving Read1 on types containing AttrSet
instance Read1 AttrSet where
  liftReadsPrec rp rl d = map (\(a, r) -> (AttrSet a, r)) . liftReadsPrec rp rl d
  {-# INLINE liftReadsPrec #-}

-- | Hashable1 for deriving Hashable1 on types containing AttrSet
instance Hashable1 AttrSet where
  liftHashWithSalt h s (AttrSet m) = liftHashWithSalt h s m
  {-# INLINE liftHashWithSalt #-}

-- | Semialign instance for alignment operations
instance Semialign AttrSet where
  align (AttrSet m1) (AttrSet m2) = AttrSet (align m1 m2)
  {-# INLINE align #-}

-- | Align instance (adds nil to Semialign)
instance Align AttrSet where
  nil = AttrSet HM.empty
  {-# INLINE nil #-}

-- | SemialignWithIndex instance for indexed alignment operations
instance SemialignWithIndex VarName AttrSet where
  ialignWith f (AttrSet m1) (AttrSet m2) = AttrSet (ialignWith f m1 m2)
  {-# INLINE ialignWith #-}

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

insertWith :: (a -> a -> a) -> VarName -> a -> AttrSet a -> AttrSet a
insertWith f k v (AttrSet m) = AttrSet (HM.insertWith f k v m)
{-# INLINE insertWith #-}

intersection :: AttrSet a -> AttrSet b -> AttrSet a
intersection (AttrSet m1) (AttrSet m2) = AttrSet (HM.intersection m1 m2)
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> AttrSet a -> AttrSet b -> AttrSet c
intersectionWith f (AttrSet m1) (AttrSet m2) = AttrSet (HM.intersectionWith f m1 m2)
{-# INLINE intersectionWith #-}

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

mapMaybe :: (a -> Maybe b) -> AttrSet a -> AttrSet b
mapMaybe f (AttrSet m) = AttrSet (HM.mapMaybe f m)
{-# INLINE mapMaybe #-}

alterF :: Functor f => (Maybe a -> f (Maybe a)) -> VarName -> AttrSet a -> f (AttrSet a)
alterF f k (AttrSet m) = AttrSet <$> HM.alterF f k m
{-# INLINE alterF #-}
