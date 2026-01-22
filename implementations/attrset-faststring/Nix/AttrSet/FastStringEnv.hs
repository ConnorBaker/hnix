{-# LANGUAGE TemplateHaskellQuotes #-}

-- | FastStringEnv implementation of the AttrSet signature.
--
-- This implementation uses GHC's UniqFM (unique-keyed finite map) with FastString
-- keys, providing:
-- - O(log n) lookup and insertion via IntMap underneath
-- - O(1) key comparison via FastString's Unique
-- - Tight integration with GHC's interning infrastructure
--
-- Note: Iteration order is non-deterministic (based on Unique values).
-- Consumers that need deterministic order (like builtins.attrNames) must sort.
--
-- With Backpack, this module can be selected at link time via mixins.
module Nix.AttrSet.FastStringEnv
  ( AttrSet
  -- Core operations
  , empty
  , singleton
  , insert
  , delete
  , lookup
  , member
  -- Bulk operations
  , unionRight
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
import           Nix.Types.VarName (VarName(..), varNameText, mkVarName, varNameFS)
import           GHC.Data.FastString (FastString)
import           GHC.Types.Unique.FM (UniqFM)
import qualified GHC.Types.Unique.FM as UFM
import           Text.Show (showParen, showsPrec, showString)
import qualified Codec.Serialise as Serialise
import           Codec.Serialise (Serialise)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON(..), FromJSON(..), ToJSON1(..), FromJSON1(..))
import qualified Data.Aeson.Encoding as AesonEnc
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM hiding (null)
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import           Data.Data (Data(..), Constr, DataType, mkConstr, mkDataType, Fixity(..))
import           Control.DeepSeq (NFData1(..))
import           Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), Read1(..))
import           Data.Hashable.Lifted (Hashable1(..))
import           Data.Functor.WithIndex (FunctorWithIndex(..))
import           Data.Foldable.WithIndex (FoldableWithIndex(..))
import           Data.Traversable.WithIndex (TraversableWithIndex(..))
import           Data.Semialign (Semialign(..), Align(..))
import           Data.Semialign.Indexed (SemialignWithIndex(..))
import           Data.These (These(..))
import qualified Text.Read as Read
import qualified Language.Haskell.TH.Syntax as TH
import           Language.Haskell.TH.Syntax (Lift(..), Exp(..), unsafeCodeCoerce)

-- | Concrete AttrSet type backed by GHC's UniqFM with FastString keys.
-- UniqFM provides O(log n) operations via IntMap keyed by FastString's Unique.
--
-- We store (VarName, a) pairs so we can recover the VarName during iteration,
-- since UniqFM only stores the value and uses the key's Unique as the map key.
newtype AttrSet a = AttrSet { unAttrSet :: UniqFM FastString (VarName, a) }
  deriving stock (Generic)

-- | Extract just the value, discarding the stored VarName
getValue :: (VarName, a) -> a
getValue = snd
{-# INLINE getValue #-}

-- | Eq compares all key-value pairs (sorted for determinism)
instance Eq a => Eq (AttrSet a) where
  s1 == s2 = sortOn fst (toList s1) == sortOn fst (toList s2)
  {-# INLINE (==) #-}

-- | Ord via sorted list comparison for deterministic ordering
instance Ord a => Ord (AttrSet a) where
  compare s1 s2 = compare (sortOn fst (toList s1)) (sortOn fst (toList s2))
  {-# INLINE compare #-}

-- | Show via sorted list
instance Show a => Show (AttrSet a) where
  showsPrec d s = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (sortOn fst (toList s))

-- | Functor maps over values
instance Functor AttrSet where
  fmap f (AttrSet m) = AttrSet (fmap (second f) m)
  {-# INLINE fmap #-}

-- | Foldable over values
instance Foldable AttrSet where
  foldr f z (AttrSet m) = UFM.nonDetFoldUFM (f . getValue) z m
  foldl' f z (AttrSet m) = UFM.nonDetStrictFoldUFM (\kv acc -> f acc (getValue kv)) z m
  length = size
  {-# INLINE foldr #-}
  {-# INLINE foldl' #-}
  {-# INLINE length #-}

-- | Traversable over values (via list conversion since UniqFM lacks Traversable)
instance Traversable AttrSet where
  traverse f s = fromList <$> traverse (\(k, v) -> (k,) <$> f v) (toList s)
  {-# INLINE traverse #-}

-- | Semigroup via left-biased union (matches HashMap semantics).
-- Note: plusUFM is right-biased, so we swap arguments to get left-biased.
instance Semigroup (AttrSet a) where
  AttrSet m1 <> AttrSet m2 = AttrSet (UFM.plusUFM m2 m1)
  {-# INLINE (<>) #-}

-- | Monoid with empty as identity
instance Monoid (AttrSet a) where
  mempty = empty
  {-# INLINE mempty #-}

-- | FunctorWithIndex instance for indexed mapping
instance FunctorWithIndex VarName AttrSet where
  imap f (AttrSet m) = AttrSet (fmap (\(k, v) -> (k, f k v)) m)
  {-# INLINE imap #-}

-- | FoldableWithIndex instance for indexed folding
instance FoldableWithIndex VarName AttrSet where
  ifoldMap f (AttrSet m) = UFM.nonDetFoldUFM (\(k, v) acc -> f k v <> acc) mempty m
  {-# INLINE ifoldMap #-}

-- | TraversableWithIndex instance for indexed traversal
instance TraversableWithIndex VarName AttrSet where
  itraverse f s = fromList <$> traverse (\(k, v) -> (k,) <$> f k v) (toList s)
  {-# INLINE itraverse #-}

instance NFData a => NFData (AttrSet a) where
  rnf s = rnf (toList s)
  {-# INLINE rnf #-}

instance Hashable a => Hashable (AttrSet a) where
  hashWithSalt salt s = hashWithSalt salt (sortOn fst (toList s))
  {-# INLINE hashWithSalt #-}

-- | Lift instance for Template Haskell support.
instance (Lift a, Typeable a, Data a) => Lift (AttrSet a) where
  lift s = do
    listExpr <- TH.lift (toList s)
    pure $ AppE (VarE 'fromList) listExpr
  liftTyped x = unsafeCodeCoerce (TH.lift x)

-- | Read instance via list parsing
instance Read a => Read (AttrSet a) where
  readsPrec d = fmap (\(l, r) -> (fromList l, r)) . Read.readsPrec d

-- | Serialise via sorted list for deterministic serialization
instance Serialise a => Serialise (AttrSet a) where
  encode s = Serialise.encode (sortOn fst (toList s))
  decode = fromList <$> Serialise.decode
  {-# INLINE encode #-}
  {-# INLINE decode #-}

-- | Binary via sorted list for deterministic serialization
instance Binary a => Binary (AttrSet a) where
  put s = Binary.put (sortOn fst (toList s))
  get = fromList <$> Binary.get
  {-# INLINE put #-}
  {-# INLINE get #-}

-- | JSON serialization as object
instance ToJSON a => ToJSON (AttrSet a) where
  toJSON s = Aeson.Object $ AesonKM.fromList
    [(AesonKey.fromText (varNameText k), toJSON v) | (k, v) <- toList s]
  toEncoding = toEncoding . toJSON
  {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (AttrSet a) where
  parseJSON = Aeson.withObject "AttrSet" $ \obj -> do
    pairs <- traverse (\(k, v) -> (toVarName k,) <$> Aeson.parseJSON v) (AesonKM.toList obj)
    pure (fromList pairs)
   where
    toVarName :: AesonKey.Key -> VarName
    toVarName = mkVarName . AesonKey.toText
  {-# INLINE parseJSON #-}

-- | Data instance for AttrSet
attrSetConstr :: Constr
attrSetConstr = mkConstr attrSetDataType "AttrSet" [] Prefix

attrSetDataType :: DataType
attrSetDataType = mkDataType "Nix.AttrSet.FastStringEnv.AttrSet" [attrSetConstr]

instance (Data a, Typeable a) => Data (AttrSet a) where
  gfoldl f z s = z fromList `f` toList s
  gunfold k z _ = k (z fromList)
  toConstr _ = attrSetConstr
  dataTypeOf _ = attrSetDataType
  {-# INLINE gfoldl #-}
  {-# INLINE gunfold #-}

-- | NFData1 for deriving NFData1 on types containing AttrSet
instance NFData1 AttrSet where
  liftRnf f s = liftRnf (\(k, v) -> rnf k `seq` f v) (toList s)
  {-# INLINE liftRnf #-}

-- | ToJSON1 for deriving ToJSON1 on types containing AttrSet
instance ToJSON1 AttrSet where
  liftToJSON _ toJ _ s = Aeson.Object $ AesonKM.fromList
    [(AesonKey.fromText (varNameText k), toJ v) | (k, v) <- toList s]
  liftToEncoding _ toE _ s =
    Aeson.pairs $ mconcat [AesonEnc.pair (AesonKey.fromText (varNameText k)) (toE v) | (k, v) <- toList s]
  {-# INLINE liftToJSON #-}
  {-# INLINE liftToEncoding #-}

-- | FromJSON1 for deriving FromJSON1 on types containing AttrSet
instance FromJSON1 AttrSet where
  liftParseJSON _ pJ _ = Aeson.withObject "AttrSet" $ \obj -> do
    pairs <- traverse (\(k, v) -> (toVarName k,) <$> pJ v) (AesonKM.toList obj)
    pure (fromList pairs)
   where
    toVarName :: AesonKey.Key -> VarName
    toVarName = mkVarName . AesonKey.toText
  {-# INLINE liftParseJSON #-}

-- | Eq1 for deriving Eq1 on types containing AttrSet
instance Eq1 AttrSet where
  liftEq f s1 s2 =
    let l1 = sortOn fst (toList s1)
        l2 = sortOn fst (toList s2)
    in length l1 == length l2 && all (\((k1,v1),(k2,v2)) -> k1 == k2 && f v1 v2) (zip l1 l2)
  {-# INLINE liftEq #-}

-- | Ord1 for deriving Ord1 on types containing AttrSet
instance Ord1 AttrSet where
  liftCompare f s1 s2 =
    liftCompare (\(k1, v1) (k2, v2) -> compare k1 k2 <> f v1 v2)
      (sortOn fst (toList s1))
      (sortOn fst (toList s2))
  {-# INLINE liftCompare #-}

-- | Show1 for deriving Show1 on types containing AttrSet
instance Show1 AttrSet where
  liftShowsPrec sp sl d s =
    liftShowsPrec (\d' (k, v) -> showParen (d' > 10) $ showsPrec 11 k . showString ", " . sp 11 v)
                  (liftShowList sp sl)
                  d (sortOn fst (toList s))
  {-# INLINE liftShowsPrec #-}

-- | Read1 for deriving Read1 on types containing AttrSet
instance Read1 AttrSet where
  liftReadsPrec rp rl d =
    fmap (\(l, r) -> (fromList l, r)) . liftReadsPrec rp' rl' d
   where
    rp' d' = Read.readParen (d' > 10) $ \s -> do
      (k, s') <- Read.readsPrec 11 s
      (",", s'') <- Read.lex s'
      (v, s''') <- rp 11 s''
      pure ((k, v), s''')
    rl' = liftReadList rp rl
  {-# INLINE liftReadsPrec #-}

-- | Hashable1 for deriving Hashable1 on types containing AttrSet
instance Hashable1 AttrSet where
  liftHashWithSalt h salt s =
    foldl' (\acc (k, v) -> h (hashWithSalt acc k) v) salt (sortOn fst (toList s))
  {-# INLINE liftHashWithSalt #-}

-- | Semialign instance for alignment operations
instance Semialign AttrSet where
  align s1 s2 = fromList $ go (sortOn fst (toList s1)) (sortOn fst (toList s2))
   where
    go :: [(VarName, a)] -> [(VarName, b)] -> [(VarName, These a b)]
    go [] ys = fmap (second That) ys
    go xs [] = fmap (second This) xs
    go xs@((kx, x):xs') ys@((ky, y):ys') =
      case compare kx ky of
        LT -> (kx, This x) : go xs' ys
        GT -> (ky, That y) : go xs ys'
        EQ -> (kx, These x y) : go xs' ys'
  {-# INLINE align #-}

-- | Align instance (adds nil to Semialign)
instance Align AttrSet where
  nil = empty
  {-# INLINE nil #-}

-- | SemialignWithIndex instance for indexed alignment operations
instance SemialignWithIndex VarName AttrSet where
  ialignWith f s1 s2 = fromList $ go (sortOn fst (toList s1)) (sortOn fst (toList s2))
   where
    go [] ys = fmap (\(k, y) -> (k, f k (That y))) ys
    go xs [] = fmap (\(k, x) -> (k, f k (This x))) xs
    go xs@((kx, x):xs') ys@((ky, y):ys') =
      case compare kx ky of
        LT -> (kx, f kx (This x)) : go xs' ys
        GT -> (ky, f ky (That y)) : go xs ys'
        EQ -> (kx, f kx (These x y)) : go xs' ys'
  {-# INLINE ialignWith #-}

-- * Core operations

empty :: AttrSet a
empty = AttrSet UFM.emptyUFM
{-# INLINE empty #-}

singleton :: VarName -> a -> AttrSet a
singleton k v = AttrSet (UFM.unitUFM (varNameFS k) (k, v))
{-# INLINE singleton #-}

insert :: VarName -> a -> AttrSet a -> AttrSet a
insert k v (AttrSet m) = AttrSet (UFM.addToUFM m (varNameFS k) (k, v))
{-# INLINE insert #-}

delete :: VarName -> AttrSet a -> AttrSet a
delete k (AttrSet m) = AttrSet (UFM.delFromUFM m (varNameFS k))
{-# INLINE delete #-}

lookup :: VarName -> AttrSet a -> Maybe a
lookup k (AttrSet m) = getValue <$> UFM.lookupUFM m (varNameFS k)
{-# INLINE lookup #-}

member :: VarName -> AttrSet a -> Bool
member k (AttrSet m) = UFM.elemUFM (varNameFS k) m
{-# INLINE member #-}

-- * Bulk operations

-- | Right-biased union: values from the second argument win for duplicate keys.
-- This matches Nix's @//@ operator semantics.
-- Note: plusUFM is right-biased (second argument wins), which is what we want.
unionRight :: AttrSet a -> AttrSet a -> AttrSet a
unionRight (AttrSet m1) (AttrSet m2) = AttrSet (UFM.plusUFM m1 m2)
{-# INLINE unionRight #-}

unionWith :: (a -> a -> a) -> AttrSet a -> AttrSet a -> AttrSet a
unionWith f (AttrSet m1) (AttrSet m2) =
  AttrSet (UFM.plusUFM_C (\(k, v1) (_, v2) -> (k, f v1 v2)) m1 m2)
{-# INLINE unionWith #-}

insertWith :: (a -> a -> a) -> VarName -> a -> AttrSet a -> AttrSet a
insertWith f k v s =
  case lookup k s of
    Nothing  -> insert k v s
    Just old -> insert k (f v old) s
{-# INLINE insertWith #-}

intersection :: AttrSet a -> AttrSet b -> AttrSet a
intersection (AttrSet m1) (AttrSet m2) =
  AttrSet (UFM.intersectUFM m1 m2)
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> AttrSet a -> AttrSet b -> AttrSet c
intersectionWith f (AttrSet m1) (AttrSet m2) =
  AttrSet (UFM.intersectUFM_C (\(k, v1) (_, v2) -> (k, f v1 v2)) m1 m2)
{-# INLINE intersectionWith #-}

difference :: AttrSet a -> AttrSet b -> AttrSet a
difference (AttrSet m1) (AttrSet m2) = AttrSet (UFM.minusUFM m1 m2)
{-# INLINE difference #-}

-- * Conversion

fromList :: [(VarName, a)] -> AttrSet a
fromList pairs = AttrSet (UFM.listToUFM [(varNameFS k, (k, v)) | (k, v) <- pairs])
{-# INLINE fromList #-}

-- | Convert to a list of key-value pairs.
-- Note: Iteration order is non-deterministic. Sort if determinism is needed.
toList :: AttrSet a -> [(VarName, a)]
toList (AttrSet m) = UFM.nonDetEltsUFM m
{-# INLINE toList #-}

-- | Get all keys.
-- Note: Order is non-deterministic. Sort if determinism is needed.
keys :: AttrSet a -> [VarName]
keys = fmap fst . toList
{-# INLINE keys #-}

-- | Get all values.
-- Note: Order is non-deterministic.
elems :: AttrSet a -> [a]
elems = fmap snd . toList
{-# INLINE elems #-}

-- * Properties

null :: AttrSet a -> Bool
null (AttrSet m) = UFM.isNullUFM m
{-# INLINE null #-}

size :: AttrSet a -> Int
size (AttrSet m) = UFM.sizeUFM m
{-# INLINE size #-}

-- * Higher-order operations

mapWithKey :: (VarName -> a -> b) -> AttrSet a -> AttrSet b
mapWithKey f (AttrSet m) = AttrSet (fmap (\(k, v) -> (k, f k v)) m)
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative f => (VarName -> a -> f b) -> AttrSet a -> f (AttrSet b)
traverseWithKey f s = fromList <$> traverse (\(k, v) -> (k,) <$> f k v) (toList s)
{-# INLINE traverseWithKey #-}

foldlWithKey' :: (b -> VarName -> a -> b) -> b -> AttrSet a -> b
foldlWithKey' f z (AttrSet m) = UFM.nonDetStrictFoldUFM (\(k, v) acc -> f acc k v) z m
{-# INLINE foldlWithKey' #-}

filterWithKey :: (VarName -> a -> Bool) -> AttrSet a -> AttrSet a
filterWithKey p (AttrSet m) = AttrSet (UFM.filterUFM (\(k, v) -> p k v) m)
{-# INLINE filterWithKey #-}

mapMaybe :: (a -> Maybe b) -> AttrSet a -> AttrSet b
mapMaybe f (AttrSet m) = AttrSet (UFM.mapMaybeUFM (\(k, v) -> (k,) <$> f v) m)
{-# INLINE mapMaybe #-}

alterF :: Functor f => (Maybe a -> f (Maybe a)) -> VarName -> AttrSet a -> f (AttrSet a)
alterF f key s =
  let current = lookup key s
  in fmap (\case
            Nothing -> delete key s
            Just v  -> insert key v s
         ) (f current)
{-# INLINE alterF #-}
