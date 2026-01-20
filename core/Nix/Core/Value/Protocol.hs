{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Value protocol for Nix builtins.
--
-- This module provides the abstract interface for NValue operations needed by
-- builtin sublibraries (like hnix-builtins-list). It uses the abstract types
-- from Backpack signatures, so it can be used by indefinite sublibraries
-- that don't have access to the concrete NValue type.
--
-- The protocol provides:
--   * Value extraction (extractList, extractInt, etc.)
--   * Value construction (mkList, mkInt, etc.)
--   * Interned singleton values
--   * Thunk operations (demand, defer)
--   * Function application (callFunc)
--   * Error handling
module Nix.Core.Value.Protocol
  ( -- * Value type
    NValue
    -- * Constraint types (for builtins)
  , NVConstraint
  , MonadValue
  , MonadThunk
  , GivenInterned
    -- * List type (re-exported from signature)
  , NixList
    -- * List operations (re-exported from signature)
  , listLength
  , listHead
  , listTail
  , listElemAt
  , listUnsafeElemAt
  , listNull
  , listFilterM
  , listFoldM'
  , listGenListM
  , listPartitionM
  , listEmpty
  , listSingleton
  , listUncons
  , listCons
  , listSnoc
  , listAppend
  , listFromList
  , listToList
  , listReverse
  , listUnsafeTail
    -- * AttrSet type (re-exported from signature)
  , AttrSet
    -- * AttrSet operations (re-exported from signature)
  , attrSetEmpty
  , attrSetNull
  , attrSetSize
  , attrSetMember
  , attrSetLookup
  , attrSetInsert
  , attrSetDelete
  , attrSetKeys
  , attrSetElems
  , attrSetToList
  , attrSetFromList
  , attrSetUnionRight
  , attrSetUnionWith
  , attrSetIntersection
  , attrSetDifference
  , attrSetMapWithKey
  , attrSetTraverseWithKey
  , attrSetFoldlWithKey'
  , attrSetFilterWithKey
  , attrSetSingleton
  , attrSetInsertWith
  , attrSetIntersectionWith
  , attrSetMapMaybe
  , attrSetAlterF
    -- * Value extraction
  , extractList
  , extractInt
  , extractBool
  , extractStringNoContext
  , extractAttrSet
  , extractAttrSetRaw
    -- * Value construction
  , mkList
  , mkInt
  , mkBool
  , mkSet
  , mkSetRaw
  , mkConstant
  , mkStringNoContext
    -- * Interned singleton values
  , internedEmptyList
  , internedEmptySet
  , internedTrue
  , internedFalse
  , internedNull
  , internedBool
    -- * Thunk operations
  , demand
  , defer
    -- * Function application
  , callFunc
    -- * Error handling
  , throwTypeError
  , throwErrorWithContext
    -- * Attribute set operations
  , lookupAttr
    -- * Value equality
  , valueEq
  ) where

import           Relude hiding (empty)
import           GHC.Exception                  ( ErrorCall(..) )
import           Control.Monad.Catch            ( MonadThrow, throwM )

import           Nix.Types.Atom                 ( NAtom(..) )
import           Nix.Types.VarName              ( VarName )
import           Nix.Core.Expr.Types            ( emptyPositionSet )
import           Nix.List.Sig                   ( NixList )
import qualified Nix.List.Sig                  as L
import           Nix.AttrSet.Sig                ( AttrSet )
import qualified Nix.AttrSet.Sig               as A
import           Nix.Core.Value.String          ( mkNixStringWithoutContext
                                                , getStringNoContext
                                                )
import           Nix.Core.Value                 ( NValue
                                                , NVConstraint  -- Re-export this
                                                , pattern NVConstant
                                                , pattern NVStr
                                                , pattern NVList
                                                , pattern NVSet
                                                , pattern NVClosure
                                                , pattern NVBuiltin
                                                )
import           Nix.Core.Value.Interned        ( GivenInterned )
import qualified Nix.Core.Value.Interned       as Interned
import           Nix.Core.Value.Monad           ( MonadValue )
import qualified Nix.Core.Value.Monad          as VM
import           Nix.Core.Value.Thunk           ( MonadThunk )
import           Nix.Core.Value.Equal           ( valueEqM )

-- * Value extraction

-- | Extract a list from an NValue.
-- Returns Nothing if the value is not a list.
extractList :: NVConstraint f => NValue t f m -> Maybe (NixList (NValue t f m))
extractList (NVList l) = Just l
extractList _          = Nothing
{-# INLINE extractList #-}

-- | Extract an integer from an NValue.
-- Returns Nothing if the value is not an integer.
extractInt :: NVConstraint f => NValue t f m -> Maybe Integer
extractInt (NVConstant (NInt n)) = Just (fromIntegral n)
extractInt _                     = Nothing
{-# INLINE extractInt #-}

-- | Extract a boolean from an NValue.
-- Returns Nothing if the value is not a boolean.
extractBool :: NVConstraint f => NValue t f m -> Maybe Bool
extractBool (NVConstant (NBool b)) = Just b
extractBool _                      = Nothing
{-# INLINE extractBool #-}

-- | Extract a string without context from an NValue.
-- Returns Nothing if the value is not a string or has context.
extractStringNoContext :: NVConstraint f => NValue t f m -> Maybe Text
extractStringNoContext (NVStr ns) = getStringNoContext ns
extractStringNoContext _          = Nothing
{-# INLINE extractStringNoContext #-}

-- | Extract an attrset from an NValue.
-- Returns Nothing if the value is not an attrset.
extractAttrSet :: NVConstraint f => NValue t f m -> Maybe [(VarName, NValue t f m)]
extractAttrSet (NVSet _ s) = Just (A.toList s)
extractAttrSet _           = Nothing
{-# INLINE extractAttrSet #-}

-- | Extract an attrset from an NValue as the actual AttrSet type.
-- Returns Nothing if the value is not an attrset.
-- Use this when you need direct access to AttrSet operations.
extractAttrSetRaw :: NVConstraint f => NValue t f m -> Maybe (AttrSet (NValue t f m))
extractAttrSetRaw (NVSet _ s) = Just s
extractAttrSetRaw _           = Nothing
{-# INLINE extractAttrSetRaw #-}

-- * Value construction

-- | Construct an NValue list from a NixList.
mkList :: NVConstraint f => NixList (NValue t f m) -> NValue t f m
mkList = NVList
{-# INLINE mkList #-}

-- | Construct an NValue integer.
mkInt :: NVConstraint f => Integer -> NValue t f m
mkInt n = NVConstant (NInt (fromIntegral n))
{-# INLINE mkInt #-}

-- | Construct an NValue boolean.
mkBool :: NVConstraint f => Bool -> NValue t f m
mkBool = NVConstant . NBool
{-# INLINE mkBool #-}

-- | Construct an NValue attribute set from a list of key-value pairs.
mkSet :: NVConstraint f => [(VarName, NValue t f m)] -> NValue t f m
mkSet pairs = NVSet emptyPositionSet (A.fromList pairs)
{-# INLINE mkSet #-}

-- | Construct an NValue attribute set from an AttrSet directly.
-- Use this when you have an AttrSet and want to avoid list conversion.
mkSetRaw :: NVConstraint f => AttrSet (NValue t f m) -> NValue t f m
mkSetRaw s = NVSet emptyPositionSet s
{-# INLINE mkSetRaw #-}

-- | Construct an NValue constant from an atom.
mkConstant :: NVConstraint f => NAtom -> NValue t f m
mkConstant = NVConstant
{-# INLINE mkConstant #-}

-- | Construct an NValue string without context.
mkStringNoContext :: NVConstraint f => Text -> NValue t f m
mkStringNoContext t = NVStr (mkNixStringWithoutContext t)
{-# INLINE mkStringNoContext #-}

-- * Interned singleton values

-- | The interned empty list value [].
internedEmptyList :: GivenInterned t f m => NValue t f m
internedEmptyList = Interned.internedEmptyList
{-# INLINE internedEmptyList #-}

-- | The interned empty set value {}.
internedEmptySet :: GivenInterned t f m => NValue t f m
internedEmptySet = Interned.internedEmptySet
{-# INLINE internedEmptySet #-}

-- | Interned true value.
internedTrue :: GivenInterned t f m => NValue t f m
internedTrue = Interned.internedTrue
{-# INLINE internedTrue #-}

-- | Interned false value.
internedFalse :: GivenInterned t f m => NValue t f m
internedFalse = Interned.internedFalse
{-# INLINE internedFalse #-}

-- | Interned null value.
internedNull :: GivenInterned t f m => NValue t f m
internedNull = Interned.internedNull
{-# INLINE internedNull #-}

-- | Get interned boolean by value.
internedBool :: GivenInterned t f m => Bool -> NValue t f m
internedBool = Interned.internedBool
{-# INLINE internedBool #-}

-- * Thunk operations

-- | Force evaluation of a value, returning the WHNF value.
demand :: MonadValue (NValue t f m) m => NValue t f m -> m (NValue t f m)
demand = VM.demand
{-# INLINE demand #-}

-- | Defer evaluation, creating a thunk.
defer :: MonadValue (NValue t f m) m => m (NValue t f m) -> m (NValue t f m)
defer = VM.defer
{-# INLINE defer #-}

-- * Function application

-- | Call a Nix function with an argument.
-- The first argument is the function, the second is the argument.
callFunc
  :: ( MonadValue (NValue t f m) m
     , MonadThunk t m (NValue t f m)
     , NVConstraint f
     , MonadThrow m
     )
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
callFunc fun arg = do
  f <- VM.demand fun
  case f of
    NVClosure _ g  -> g arg
    NVBuiltin _ g  -> g arg
    _              -> throwM $ ErrorCall "Cannot call a non-function value"
{-# INLINE callFunc #-}

-- * Error handling

-- | Throw a type error with the given message.
throwTypeError :: MonadThrow m => Text -> m a
throwTypeError msg = throwM $ ErrorCall (toString msg)
{-# INLINE throwTypeError #-}

-- | Throw an error with a context message for debugging.
-- First argument is the context, second is the actual error.
throwErrorWithContext :: MonadThrow m => Text -> Text -> m a
throwErrorWithContext ctx msg = throwM $ ErrorCall (toString (ctx <> ": " <> msg))
{-# INLINE throwErrorWithContext #-}

-- * Attribute set operations

-- | Lookup a key in an attrset value.
-- Returns Nothing if not an attrset or key not found.
lookupAttr :: NVConstraint f => VarName -> NValue t f m -> Maybe (NValue t f m)
lookupAttr key (NVSet _ s) = A.lookup key s
lookupAttr _   _           = Nothing
{-# INLINE lookupAttr #-}

-- * Value equality

-- | Compare two values for equality.
-- Used by builtins.elem for membership testing.
valueEq
  :: ( MonadValue (NValue t f m) m
     , MonadThunk t m (NValue t f m)
     , NVConstraint f
     , MonadThrow m
     )
  => NValue t f m
  -> NValue t f m
  -> m Bool
valueEq = valueEqM
{-# INLINE valueEq #-}

-- * List operations (re-exported from signature)
-- These provide the correct type identity for list operations.

-- | Get the length of a list.
listLength :: NixList a -> Int
listLength = L.length
{-# INLINE listLength #-}

-- | Get the first element of a list.
listHead :: NixList a -> Maybe a
listHead = L.head
{-# INLINE listHead #-}

-- | Get all elements except the first.
listTail :: NixList a -> Maybe (NixList a)
listTail = L.tail
{-# INLINE listTail #-}

-- | Safe indexing by position.
listElemAt :: NixList a -> Int -> Maybe a
listElemAt = L.elemAt
{-# INLINE listElemAt #-}

-- | Unsafe indexing by position.
listUnsafeElemAt :: NixList a -> Int -> a
listUnsafeElemAt = L.unsafeElemAt
{-# INLINE listUnsafeElemAt #-}

-- | Check if a list is empty.
listNull :: NixList a -> Bool
listNull = L.null
{-# INLINE listNull #-}

-- | Monadic filter.
listFilterM :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a)
listFilterM = L.filterM
{-# INLINE listFilterM #-}

-- | Strict monadic left fold.
listFoldM' :: Monad m => (b -> a -> m b) -> b -> NixList a -> m b
listFoldM' = L.foldM'
{-# INLINE listFoldM' #-}

-- | Generate a list monadically.
listGenListM :: Monad m => Int -> (Int -> m a) -> m (NixList a)
listGenListM = L.genListM
{-# INLINE listGenListM #-}

-- | Partition a list by a monadic predicate.
listPartitionM :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a, NixList a)
listPartitionM = L.partitionM
{-# INLINE listPartitionM #-}

-- | The empty list.
listEmpty :: NixList a
listEmpty = L.empty
{-# INLINE listEmpty #-}

-- | Create a singleton list.
listSingleton :: a -> NixList a
listSingleton = L.singleton
{-# INLINE listSingleton #-}

-- | Decompose into head and tail.
listUncons :: NixList a -> Maybe (a, NixList a)
listUncons = L.uncons
{-# INLINE listUncons #-}

-- | Prepend an element.
listCons :: a -> NixList a -> NixList a
listCons = L.cons
{-# INLINE listCons #-}

-- | Append an element.
listSnoc :: NixList a -> a -> NixList a
listSnoc = L.snoc
{-# INLINE listSnoc #-}

-- | Concatenate two lists.
listAppend :: NixList a -> NixList a -> NixList a
listAppend = L.append
{-# INLINE listAppend #-}

-- | Build a list from a Haskell list.
listFromList :: [a] -> NixList a
listFromList = L.fromList
{-# INLINE listFromList #-}

-- | Convert to a Haskell list.
listToList :: NixList a -> [a]
listToList = L.toList
{-# INLINE listToList #-}

-- | Reverse the list.
listReverse :: NixList a -> NixList a
listReverse = L.reverse
{-# INLINE listReverse #-}

-- | Unsafe tail - undefined behavior if list is empty.
listUnsafeTail :: NixList a -> NixList a
listUnsafeTail = L.unsafeTail
{-# INLINE listUnsafeTail #-}

-- * AttrSet operations (re-exported from signature)
-- These provide the correct type identity for attrset operations.

-- | The empty attribute set.
attrSetEmpty :: AttrSet a
attrSetEmpty = A.empty
{-# INLINE attrSetEmpty #-}

-- | Check if empty.
attrSetNull :: AttrSet a -> Bool
attrSetNull = A.null
{-# INLINE attrSetNull #-}

-- | Number of key-value pairs.
attrSetSize :: AttrSet a -> Int
attrSetSize = A.size
{-# INLINE attrSetSize #-}

-- | Check if a key is present.
attrSetMember :: VarName -> AttrSet a -> Bool
attrSetMember = A.member
{-# INLINE attrSetMember #-}

-- | Look up a value by key.
attrSetLookup :: VarName -> AttrSet a -> Maybe a
attrSetLookup = A.lookup
{-# INLINE attrSetLookup #-}

-- | Insert a key-value pair.
attrSetInsert :: VarName -> a -> AttrSet a -> AttrSet a
attrSetInsert = A.insert
{-# INLINE attrSetInsert #-}

-- | Delete a key from the set.
attrSetDelete :: VarName -> AttrSet a -> AttrSet a
attrSetDelete = A.delete
{-# INLINE attrSetDelete #-}

-- | Get all keys.
attrSetKeys :: AttrSet a -> [VarName]
attrSetKeys = A.keys
{-# INLINE attrSetKeys #-}

-- | Get all values.
attrSetElems :: AttrSet a -> [a]
attrSetElems = A.elems
{-# INLINE attrSetElems #-}

-- | Convert to a list of key-value pairs.
attrSetToList :: AttrSet a -> [(VarName, a)]
attrSetToList = A.toList
{-# INLINE attrSetToList #-}

-- | Build an attribute set from a list of key-value pairs.
attrSetFromList :: [(VarName, a)] -> AttrSet a
attrSetFromList = A.fromList
{-# INLINE attrSetFromList #-}

-- | Right-biased union of two attribute sets.
attrSetUnionRight :: AttrSet a -> AttrSet a -> AttrSet a
attrSetUnionRight = A.unionRight
{-# INLINE attrSetUnionRight #-}

-- | Union with a combining function.
attrSetUnionWith :: (a -> a -> a) -> AttrSet a -> AttrSet a -> AttrSet a
attrSetUnionWith = A.unionWith
{-# INLINE attrSetUnionWith #-}

-- | Intersection of two attribute sets.
attrSetIntersection :: AttrSet a -> AttrSet b -> AttrSet a
attrSetIntersection = A.intersection
{-# INLINE attrSetIntersection #-}

-- | Difference of two attribute sets.
attrSetDifference :: AttrSet a -> AttrSet b -> AttrSet a
attrSetDifference = A.difference
{-# INLINE attrSetDifference #-}

-- | Map a function over values, with access to keys.
attrSetMapWithKey :: (VarName -> a -> b) -> AttrSet a -> AttrSet b
attrSetMapWithKey = A.mapWithKey
{-# INLINE attrSetMapWithKey #-}

-- | Traverse with an applicative effect, with access to keys.
attrSetTraverseWithKey :: Applicative f => (VarName -> a -> f b) -> AttrSet a -> f (AttrSet b)
attrSetTraverseWithKey = A.traverseWithKey
{-# INLINE attrSetTraverseWithKey #-}

-- | Strict left fold with key access.
attrSetFoldlWithKey' :: (b -> VarName -> a -> b) -> b -> AttrSet a -> b
attrSetFoldlWithKey' = A.foldlWithKey'
{-# INLINE attrSetFoldlWithKey' #-}

-- | Filter by predicate on key and value.
attrSetFilterWithKey :: (VarName -> a -> Bool) -> AttrSet a -> AttrSet a
attrSetFilterWithKey = A.filterWithKey
{-# INLINE attrSetFilterWithKey #-}

-- | Create a singleton attribute set.
attrSetSingleton :: VarName -> a -> AttrSet a
attrSetSingleton = A.singleton
{-# INLINE attrSetSingleton #-}

-- | Insert with a combining function.
-- @attrSetInsertWith f key new_value set@ inserts @new_value@ if key is absent,
-- or @f new_value old_value@ if key is present.
attrSetInsertWith :: (a -> a -> a) -> VarName -> a -> AttrSet a -> AttrSet a
attrSetInsertWith = A.insertWith
{-# INLINE attrSetInsertWith #-}

-- | Intersection with a combining function for values.
attrSetIntersectionWith :: (a -> b -> c) -> AttrSet a -> AttrSet b -> AttrSet c
attrSetIntersectionWith = A.intersectionWith
{-# INLINE attrSetIntersectionWith #-}

-- | Map a function over values, discarding Nothing results.
attrSetMapMaybe :: (a -> Maybe b) -> AttrSet a -> AttrSet b
attrSetMapMaybe = A.mapMaybe
{-# INLINE attrSetMapMaybe #-}

-- | Modify the value at a key, or insert/delete.
-- This is the fundamental operation for lens-style updates.
attrSetAlterF :: Functor f => (Maybe a -> f (Maybe a)) -> VarName -> AttrSet a -> f (AttrSet a)
attrSetAlterF = A.alterF
{-# INLINE attrSetAlterF #-}
