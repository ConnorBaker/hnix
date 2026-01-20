{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Concrete implementation of the value protocol signature.
--
-- This module implements the abstract NValue protocol defined in
-- hnix-value-protocol-sig using the actual HNix NValue type.
-- It enables the list builtins in hnix-list-vector to be linked
-- against the concrete value type at compile time.
--
-- The protocol provides:
--   * Value extraction (extractList, extractInt, etc.)
--   * Value construction (mkList, mkInt, etc.)
--   * Interned singleton values
--   * Thunk operations (demand, defer)
--   * Function application (callFunc)
--   * Error handling
module Nix.Value.Protocol
  ( -- * Value type
    NValue
    -- * Value extraction
  , extractList
  , extractInt
  , extractBool
  , extractStringNoContext
  , extractAttrSet
    -- * Value construction
  , mkList
  , mkInt
  , mkBool
  , mkSet
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

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(..) )
import           Control.Monad.Catch            ( MonadThrow, throwM )
import           Data.Text                      ( Text )

import           Nix.Atoms                      ( NAtom(..) )
import           Nix.Expr.Types                 ( VarName, emptyPositionSet )
import           Nix.String                     ( mkNixStringWithoutContext
                                                , getStringNoContext
                                                )
import           Nix.Core.List                  ( NixList )
import qualified Nix.Core.AttrSet              as A
import           Nix.Value                      ( NValue
                                                , NVConstraint
                                                , pattern NVConstant
                                                , pattern NVStr
                                                , pattern NVList
                                                , pattern NVSet
                                                , pattern NVClosure
                                                , pattern NVBuiltin
                                                )
import qualified Nix.Value.Interned            as Interned
import qualified Nix.Value.Monad               as VM
import           Nix.Thunk                     ( MonadThunk )
import           Nix.Value.Equal               ( valueEqM )

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
internedEmptyList :: Interned.GivenInterned t f m => NValue t f m
internedEmptyList = Interned.internedEmptyList
{-# INLINE internedEmptyList #-}

-- | The interned empty set value {}.
internedEmptySet :: Interned.GivenInterned t f m => NValue t f m
internedEmptySet = Interned.internedEmptySet
{-# INLINE internedEmptySet #-}

-- | Interned true value.
internedTrue :: Interned.GivenInterned t f m => NValue t f m
internedTrue = Interned.internedTrue
{-# INLINE internedTrue #-}

-- | Interned false value.
internedFalse :: Interned.GivenInterned t f m => NValue t f m
internedFalse = Interned.internedFalse
{-# INLINE internedFalse #-}

-- | Interned null value.
internedNull :: Interned.GivenInterned t f m => NValue t f m
internedNull = Interned.internedNull
{-# INLINE internedNull #-}

-- | Get interned boolean by value.
internedBool :: Interned.GivenInterned t f m => Bool -> NValue t f m
internedBool = Interned.internedBool
{-# INLINE internedBool #-}

-- * Thunk operations

-- | Force evaluation of a value, returning the WHNF value.
demand :: VM.MonadValue (NValue t f m) m => NValue t f m -> m (NValue t f m)
demand = VM.demand
{-# INLINE demand #-}

-- | Defer evaluation, creating a thunk.
defer :: VM.MonadValue (NValue t f m) m => m (NValue t f m) -> m (NValue t f m)
defer = VM.defer
{-# INLINE defer #-}

-- * Function application

-- | Call a Nix function with an argument.
-- The first argument is the function, the second is the argument.
callFunc
  :: ( VM.MonadValue (NValue t f m) m
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
  :: ( VM.MonadValue (NValue t f m) m
     , MonadThunk t m (NValue t f m)
     , NVConstraint f
     , MonadThrow m
     )
  => NValue t f m
  -> NValue t f m
  -> m Bool
valueEq = valueEqM
{-# INLINE valueEq #-}
