{-# language ConstraintKinds #-}
{-# language KindSignatures #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}

-- | Interned constant values for zero-allocation Nix evaluation.
--
-- This module provides a cache of singleton Nix values (true, false, null,
-- empty list, empty set) that are created once per evaluation context and
-- reused throughout. This eliminates redundant allocations for frequently-used
-- constant values.
--
-- Usage:
--
-- @
-- -- Access interned values via MonadReader
-- nvTrue <- askInternedTrue
-- nvFalse <- askInternedFalse
-- nvNull <- askInternedNull
-- @
--
-- The interned values are created when the evaluation context is initialized
-- and remain constant for the duration of the evaluation.
module Nix.Value.Interned
  ( InternedValues(..)
  , mkInternedValues
  ) where

import           Nix.Prelude
import           Nix.Atoms                      ( NAtom(..) )
import           Nix.Expr.Types                 ( emptyPositionSet )
import           Nix.Value                      ( NValue
                                                , NVConstraint
                                                , pattern NVConstant
                                                , pattern NVList
                                                , pattern NVSet
                                                )
import qualified Data.Vector                   as V


-- | Cache of interned constant values, created once per evaluation.
--
-- These values are parameterized by the full NValue type parameters to ensure
-- sharing within each evaluation run. The strictness annotations ensure
-- values are fully evaluated at construction time.
--
-- All stored values are constants (no thunks, no closures), so they're
-- safe to share across the entire evaluation.
data InternedValues t f m = InternedValues
  { internedTrue      :: !(NValue t f m)
  -- ^ The singleton @true@ boolean value.
  , internedFalse     :: !(NValue t f m)
  -- ^ The singleton @false@ boolean value.
  , internedNull      :: !(NValue t f m)
  -- ^ The singleton @null@ value.
  , internedEmptyList :: !(NValue t f m)
  -- ^ The singleton empty list @[]@.
  , internedEmptySet  :: !(NValue t f m)
  -- ^ The singleton empty attribute set @{}@.
  }

-- | Create interned values for a given evaluation context.
--
-- This function is called once when the evaluation context is created.
-- The 'NVConstraint' ensures we have the necessary type class instances
-- (Comonad, Applicative) to construct NValue terms.
--
-- All values are strict and fully evaluated at construction time.
mkInternedValues :: NVConstraint f => InternedValues t f m
mkInternedValues = InternedValues
  { internedTrue      = NVConstant (NBool True)
  , internedFalse     = NVConstant (NBool False)
  , internedNull      = NVConstant NNull
  , internedEmptyList = NVList V.empty
  , internedEmptySet  = NVSet emptyPositionSet mempty
  }
{-# INLINABLE mkInternedValues #-}
