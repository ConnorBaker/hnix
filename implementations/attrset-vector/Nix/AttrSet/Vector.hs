{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Vector implementation of the AttrSet signature.
--
-- This implementation uses a sorted vector of key-value pairs, providing:
-- - O(log n) lookup via binary search
-- - O(n) insertion (maintaining sorted order)
-- - Excellent cache locality for small to medium attribute sets
--
-- With Backpack, this module can be selected at link time as an alternative
-- to the HashMap implementation for benchmarking or specialized workloads.
module Nix.AttrSet.Vector
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
import           Nix.Types.VarName (VarName, varNameText, mkVarName)
import           Text.Show (showParen, showsPrec, showString)
import           Data.List (nubBy)
import qualified Codec.Serialise as Serialise
import           Codec.Serialise (Serialise)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON(..), FromJSON(..), ToJSON1(..), FromJSON1(..))
import qualified Data.Aeson.Encoding as AesonEnc
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import           Data.Data (Data(..), Constr, DataType, mkConstr, mkDataType, Fixity(..))
import qualified Data.Vector as V
import           Data.Vector (Vector)
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

-- | Concrete AttrSet type backed by a sorted Vector.
-- The vector is kept sorted by key (VarName) to enable binary search.
newtype AttrSet a = AttrSet { unAttrSet :: Vector (VarName, a) }
  deriving stock (Generic, Show)

-- | Eq via element-wise comparison (vectors are already sorted)
instance Eq a => Eq (AttrSet a) where
  AttrSet v1 == AttrSet v2 = v1 == v2
  {-# INLINE (==) #-}

-- | Ord via lexicographic comparison (vectors are already sorted)
instance Ord a => Ord (AttrSet a) where
  compare (AttrSet v1) (AttrSet v2) = compare v1 v2
  {-# INLINE compare #-}

-- | Functor maps over values
instance Functor AttrSet where
  fmap f (AttrSet v) = AttrSet (V.map (second f) v)
  {-# INLINE fmap #-}

-- | Foldable over values
instance Foldable AttrSet where
  foldr f z (AttrSet v) = V.foldr (f . snd) z v
  foldl' f z (AttrSet v) = V.foldl' (\acc (_, a) -> f acc a) z v
  length (AttrSet v) = V.length v
  {-# INLINE foldr #-}
  {-# INLINE foldl' #-}
  {-# INLINE length #-}

-- | Traversable over values (preserves sorted order)
instance Traversable AttrSet where
  traverse f (AttrSet v) = AttrSet <$> traverse (\(k, a) -> (k,) <$> f a) v
  {-# INLINE traverse #-}

-- | Semigroup via left-biased union (matches HashMap semantics)
-- Note: HashMap's Semigroup is left-biased, so a <> b keeps values from a.
instance Semigroup (AttrSet a) where
  AttrSet v1 <> AttrSet v2 = AttrSet (V.fromList (merge (V.toList v1) (V.toList v2)))
   where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@((kx, x):xs') ys@((ky, y):ys') =
      case compare kx ky of
        LT -> (kx, x) : merge xs' ys
        GT -> (ky, y) : merge xs ys'
        EQ -> (kx, x) : merge xs' ys'  -- Left wins
  {-# INLINE (<>) #-}

-- | Monoid with empty as identity
instance Monoid (AttrSet a) where
  mempty = empty
  {-# INLINE mempty #-}

-- | FunctorWithIndex instance for indexed mapping
instance FunctorWithIndex VarName AttrSet where
  imap f (AttrSet v) = AttrSet (V.map (\(k, a) -> (k, f k a)) v)
  {-# INLINE imap #-}

-- | FoldableWithIndex instance for indexed folding
instance FoldableWithIndex VarName AttrSet where
  ifoldMap f (AttrSet v) = V.foldMap (\(k, a) -> f k a) v
  {-# INLINE ifoldMap #-}

-- | TraversableWithIndex instance for indexed traversal
instance TraversableWithIndex VarName AttrSet where
  itraverse f (AttrSet v) = AttrSet <$> traverse (\(k, a) -> (k,) <$> f k a) v
  {-# INLINE itraverse #-}

instance NFData a => NFData (AttrSet a) where
  rnf (AttrSet v) = rnf v
  {-# INLINE rnf #-}

instance Hashable a => Hashable (AttrSet a) where
  hashWithSalt s (AttrSet v) = hashWithSalt s (V.toList v)
  {-# INLINE hashWithSalt #-}

-- | Lift instance for Template Haskell support.
instance (Lift a, Typeable a, Data a) => Lift (AttrSet a) where
  lift (AttrSet v) = do
    listExpr <- TH.lift (V.toList v)
    pure $ AppE (VarE 'fromList) listExpr
  liftTyped x = unsafeCodeCoerce (TH.lift x)

-- | Read instance via list parsing
instance Read a => Read (AttrSet a) where
  readsPrec d = fmap (\(l, r) -> (fromList l, r)) . Read.readsPrec d

-- | Serialise via list
instance Serialise a => Serialise (AttrSet a) where
  encode (AttrSet v) = Serialise.encode (V.toList v)
  decode = fromList <$> Serialise.decode
  {-# INLINE encode #-}
  {-# INLINE decode #-}

-- | Binary via list
instance Binary a => Binary (AttrSet a) where
  put (AttrSet v) = Binary.put (V.toList v)
  get = fromList <$> Binary.get
  {-# INLINE put #-}
  {-# INLINE get #-}

-- | JSON serialization as object (via KeyMap for compatibility)
instance ToJSON a => ToJSON (AttrSet a) where
  toJSON (AttrSet v) = Aeson.Object $ AesonKM.fromList
    [(AesonKey.fromText (varNameText k), toJSON a) | (k, a) <- V.toList v]
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
attrSetDataType = mkDataType "Nix.AttrSet.Vector.AttrSet" [attrSetConstr]

instance (Data a, Typeable a) => Data (AttrSet a) where
  gfoldl f z (AttrSet v) = z (AttrSet . V.fromList) `f` V.toList v
  gunfold k z _ = k (z (AttrSet . V.fromList))
  toConstr (AttrSet _) = attrSetConstr
  dataTypeOf _ = attrSetDataType
  {-# INLINE gfoldl #-}
  {-# INLINE gunfold #-}

-- | NFData1 for deriving NFData1 on types containing AttrSet
instance NFData1 AttrSet where
  liftRnf f (AttrSet v) = liftRnf (\(k, a) -> rnf k `seq` f a) v
  {-# INLINE liftRnf #-}

-- | ToJSON1 for deriving ToJSON1 on types containing AttrSet
instance ToJSON1 AttrSet where
  liftToJSON _ toJ _ (AttrSet v) = Aeson.Object $ AesonKM.fromList
    [(AesonKey.fromText (varNameText k), toJ a) | (k, a) <- V.toList v]
  liftToEncoding _ toE _ (AttrSet v) =
    Aeson.pairs $ mconcat [AesonEnc.pair (AesonKey.fromText (varNameText k)) (toE a) | (k, a) <- V.toList v]
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
  liftEq f (AttrSet v1) (AttrSet v2) =
    V.length v1 == V.length v2 &&
    V.and (V.zipWith (\(k1, a1) (k2, a2) -> k1 == k2 && f a1 a2) v1 v2)
  {-# INLINE liftEq #-}

-- | Ord1 for deriving Ord1 on types containing AttrSet
instance Ord1 AttrSet where
  liftCompare f (AttrSet v1) (AttrSet v2) =
    liftCompare (\(k1, a1) (k2, a2) -> compare k1 k2 <> f a1 a2) v1 v2
  {-# INLINE liftCompare #-}

-- | Show1 for deriving Show1 on types containing AttrSet
instance Show1 AttrSet where
  liftShowsPrec sp sl d (AttrSet v) =
    liftShowsPrec (\d' (k, a) -> showParen (d' > 10) $ showsPrec 11 k . showString ", " . sp 11 a)
                  (liftShowList sp sl)
                  d v
  {-# INLINE liftShowsPrec #-}

-- | Read1 for deriving Read1 on types containing AttrSet
instance Read1 AttrSet where
  liftReadsPrec rp rl d =
    fmap (\(l, r) -> (fromList l, r)) . liftReadsPrec rp' rl' d
   where
    rp' d' = Read.readParen (d' > 10) $ \s -> do
      (k, s') <- Read.readsPrec 11 s
      (",", s'') <- Read.lex s'
      (a, s''') <- rp 11 s''
      pure ((k, a), s''')
    rl' = liftReadList rp rl
  {-# INLINE liftReadsPrec #-}

-- | Hashable1 for deriving Hashable1 on types containing AttrSet
instance Hashable1 AttrSet where
  liftHashWithSalt h s (AttrSet v) =
    V.foldl' (\acc (k, a) -> h (hashWithSalt acc k) a) s v
  {-# INLINE liftHashWithSalt #-}

-- | Semialign instance for alignment operations
instance Semialign AttrSet where
  align s1 s2 = fromList $ mergeAligned (toList s1) (toList s2)
   where
    mergeAligned :: [(VarName, a)] -> [(VarName, b)] -> [(VarName, These a b)]
    mergeAligned [] ys = fmap (second That) ys
    mergeAligned xs [] = fmap (second This) xs
    mergeAligned xs@((kx, x):xs') ys@((ky, y):ys') =
      case compare kx ky of
        LT -> (kx, This x) : mergeAligned xs' ys
        GT -> (ky, That y) : mergeAligned xs ys'
        EQ -> (kx, These x y) : mergeAligned xs' ys'
  {-# INLINE align #-}

-- | Align instance (adds nil to Semialign)
instance Align AttrSet where
  nil = empty
  {-# INLINE nil #-}

-- | SemialignWithIndex instance for indexed alignment operations
instance SemialignWithIndex VarName AttrSet where
  ialignWith f s1 s2 = fromList $ go (toList s1) (toList s2)
   where
    go [] ys = fmap (\(k, y) -> (k, f k (That y))) ys
    go xs [] = fmap (\(k, x) -> (k, f k (This x))) xs
    go xs@((kx, x):xs') ys@((ky, y):ys') =
      case compare kx ky of
        LT -> (kx, f kx (This x)) : go xs' ys
        GT -> (ky, f ky (That y)) : go xs ys'
        EQ -> (kx, f kx (These x y)) : go xs' ys'
  {-# INLINE ialignWith #-}

-- * Internal helpers

-- | Binary search for a key in the sorted vector.
-- Returns the index where the key is found, or where it should be inserted.
binarySearch :: VarName -> Vector (VarName, a) -> Int
binarySearch key vec = go 0 (V.length vec)
 where
  go !lo !hi
    | lo >= hi = lo
    | otherwise =
        let mid = lo + (hi - lo) `div` 2
            (midKey, _) = vec V.! mid
        in case compare key midKey of
             LT -> go lo mid
             GT -> go (mid + 1) hi
             EQ -> mid
{-# INLINE binarySearch #-}

-- | Look up a key using binary search.
binaryLookup :: VarName -> Vector (VarName, a) -> Maybe a
binaryLookup key vec
  | V.null vec = Nothing
  | otherwise =
      let idx = binarySearch key vec
      in if idx < V.length vec
           then let (k, v) = vec V.! idx
                in if k == key then Just v else Nothing
           else Nothing
{-# INLINE binaryLookup #-}

-- | Insert maintaining sorted order.
sortedInsert :: VarName -> a -> Vector (VarName, a) -> Vector (VarName, a)
sortedInsert key val vec =
  let idx = binarySearch key vec
  in if idx < V.length vec && fst (vec V.! idx) == key
       then V.update vec (V.singleton (idx, (key, val)))  -- Replace existing
       else let (before, after) = V.splitAt idx vec
            in before V.++ V.singleton (key, val) V.++ after  -- Insert new
{-# INLINE sortedInsert #-}

-- | Delete maintaining sorted order.
sortedDelete :: VarName -> Vector (VarName, a) -> Vector (VarName, a)
sortedDelete key vec =
  let idx = binarySearch key vec
  in if idx < V.length vec && fst (vec V.! idx) == key
       then let (before, after) = V.splitAt idx vec
            in before V.++ V.drop 1 after
       else vec
{-# INLINE sortedDelete #-}

-- * Core operations

empty :: AttrSet a
empty = AttrSet V.empty
{-# INLINE empty #-}

singleton :: VarName -> a -> AttrSet a
singleton k v = AttrSet (V.singleton (k, v))
{-# INLINE singleton #-}

insert :: VarName -> a -> AttrSet a -> AttrSet a
insert k v (AttrSet vec) = AttrSet (sortedInsert k v vec)
{-# INLINE insert #-}

delete :: VarName -> AttrSet a -> AttrSet a
delete k (AttrSet vec) = AttrSet (sortedDelete k vec)
{-# INLINE delete #-}

lookup :: VarName -> AttrSet a -> Maybe a
lookup k (AttrSet vec) = binaryLookup k vec
{-# INLINE lookup #-}

member :: VarName -> AttrSet a -> Bool
member k s = isJust (lookup k s)
{-# INLINE member #-}

-- * Bulk operations

-- | Right-biased union: values from the second argument win for duplicate keys.
-- Uses merge algorithm on sorted vectors: O(n + m).
unionRight :: AttrSet a -> AttrSet a -> AttrSet a
unionRight (AttrSet v1) (AttrSet v2) = AttrSet (V.fromList (merge (V.toList v1) (V.toList v2)))
 where
  merge :: [(VarName, a)] -> [(VarName, a)] -> [(VarName, a)]
  merge [] ys = ys
  merge xs [] = xs
  merge xs@((kx, x):xs') ys@((ky, y):ys') =
    case compare kx ky of
      LT -> (kx, x) : merge xs' ys
      GT -> (ky, y) : merge xs ys'
      EQ -> (ky, y) : merge xs' ys'  -- Right wins
{-# INLINE unionRight #-}

unionWith :: (a -> a -> a) -> AttrSet a -> AttrSet a -> AttrSet a
unionWith f (AttrSet v1) (AttrSet v2) = AttrSet (V.fromList (go (V.toList v1) (V.toList v2)))
 where
  go [] ys = ys
  go xs [] = xs
  go xs@((kx, x):xs') ys@((ky, y):ys') =
    case compare kx ky of
      LT -> (kx, x) : go xs' ys
      GT -> (ky, y) : go xs ys'
      EQ -> (kx, f x y) : go xs' ys'
{-# INLINE unionWith #-}

insertWith :: (a -> a -> a) -> VarName -> a -> AttrSet a -> AttrSet a
insertWith f key val (AttrSet vec) =
  let idx = binarySearch key vec
  in AttrSet $
       if idx < V.length vec && fst (vec V.! idx) == key
         then let (_, old) = vec V.! idx
              in V.update vec (V.singleton (idx, (key, f val old)))
         else let (before, after) = V.splitAt idx vec
              in before V.++ V.singleton (key, val) V.++ after
{-# INLINE insertWith #-}

intersection :: AttrSet a -> AttrSet b -> AttrSet a
intersection (AttrSet v1) (AttrSet v2) = AttrSet (V.fromList (merge (V.toList v1) (V.toList v2)))
 where
  merge :: [(VarName, a)] -> [(VarName, b)] -> [(VarName, a)]
  merge [] _ = []
  merge _ [] = []
  merge xs@((kx, x):xs') ys@((ky, _):ys') =
    case compare kx ky of
      LT -> merge xs' ys
      GT -> merge xs ys'
      EQ -> (kx, x) : merge xs' ys'
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> AttrSet a -> AttrSet b -> AttrSet c
intersectionWith f (AttrSet v1) (AttrSet v2) = AttrSet (V.fromList (go (V.toList v1) (V.toList v2)))
 where
  go [] _ = []
  go _ [] = []
  go xs@((kx, x):xs') ys@((ky, y):ys') =
    case compare kx ky of
      LT -> go xs' ys
      GT -> go xs ys'
      EQ -> (kx, f x y) : go xs' ys'
{-# INLINE intersectionWith #-}

difference :: AttrSet a -> AttrSet b -> AttrSet a
difference (AttrSet v1) (AttrSet v2) = AttrSet (V.fromList (go (V.toList v1) (V.toList v2)))
 where
  go xs [] = xs
  go [] _ = []
  go xs@((kx, x):xs') ys@((ky, _):ys') =
    case compare kx ky of
      LT -> (kx, x) : go xs' ys
      GT -> go xs ys'
      EQ -> go xs' ys'
{-# INLINE difference #-}

-- * Conversion

fromList :: [(VarName, a)] -> AttrSet a
fromList = AttrSet . V.fromList . sortOn fst . nubByKey
 where
  -- Keep last value for duplicate keys (matches HashMap.fromList semantics)
  nubByKey :: [(VarName, a)] -> [(VarName, a)]
  nubByKey = reverse . nubBy (\(k1, _) (k2, _) -> k1 == k2) . reverse
{-# INLINE fromList #-}

toList :: AttrSet a -> [(VarName, a)]
toList (AttrSet v) = V.toList v
{-# INLINE toList #-}

keys :: AttrSet a -> [VarName]
keys (AttrSet v) = V.toList (V.map fst v)
{-# INLINE keys #-}

elems :: AttrSet a -> [a]
elems (AttrSet v) = V.toList (V.map snd v)
{-# INLINE elems #-}

-- * Properties

null :: AttrSet a -> Bool
null (AttrSet v) = V.null v
{-# INLINE null #-}

size :: AttrSet a -> Int
size (AttrSet v) = V.length v
{-# INLINE size #-}

-- * Higher-order operations

mapWithKey :: (VarName -> a -> b) -> AttrSet a -> AttrSet b
mapWithKey f (AttrSet v) = AttrSet (V.map (\(k, a) -> (k, f k a)) v)
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative f => (VarName -> a -> f b) -> AttrSet a -> f (AttrSet b)
traverseWithKey f (AttrSet v) = AttrSet <$> traverse (\(k, a) -> (k,) <$> f k a) v
{-# INLINE traverseWithKey #-}

foldlWithKey' :: (b -> VarName -> a -> b) -> b -> AttrSet a -> b
foldlWithKey' f z (AttrSet v) = V.foldl' (\acc (k, a) -> f acc k a) z v
{-# INLINE foldlWithKey' #-}

filterWithKey :: (VarName -> a -> Bool) -> AttrSet a -> AttrSet a
filterWithKey p (AttrSet v) = AttrSet (V.filter (uncurry p) v)
{-# INLINE filterWithKey #-}

mapMaybe :: (a -> Maybe b) -> AttrSet a -> AttrSet b
mapMaybe f (AttrSet v) = AttrSet (V.mapMaybe (\(k, a) -> (k,) <$> f a) v)
{-# INLINE mapMaybe #-}

alterF :: Functor f => (Maybe a -> f (Maybe a)) -> VarName -> AttrSet a -> f (AttrSet a)
alterF f key s =
  let current = lookup key s
  in fmap (\case
            Nothing -> delete key s
            Just v  -> insert key v s
         ) (f current)
{-# INLINE alterF #-}
