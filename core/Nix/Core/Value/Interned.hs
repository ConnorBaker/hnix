{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

-- | Interned constant values for zero-allocation Nix evaluation.
--
-- This module provides a cache of singleton Nix values (true, false, null,
-- empty list, empty set) that are created once per evaluation context and
-- reused throughout. This eliminates redundant allocations for frequently-used
-- constant values.
--
-- == Pure Accessors (Recommended)
--
-- The module exports pure accessor functions that use the @Given@ constraint
-- from the @reflection@ library. This provides zero-overhead access to interned
-- values without monadic wrapper:
--
-- @
-- import Nix.Value.Core.Interned
--
-- -- Pure access to interned values (requires Given constraint)
-- myFunc :: Given (InternedValues t f m) => NValue t f m
-- myFunc = internedBool True  -- No monadic wrapper needed!
-- @
--
-- The @Given@ constraint is satisfied by wrapping the evaluation entry point
-- with @give mkInternedValues@.
module Nix.Core.Value.Interned
  ( InternedValues(..)
  , mkInternedValues
  -- * Pure accessors (use these!)
  , GivenInterned
  , internedTrue
  , internedFalse
  , internedNull
  , internedEmptyList
  , internedEmptySet
  , internedEmptyString
  , internedBool
  -- * Re-export for give
  , Given
  , given
  , give
  ) where

import           Relude hiding (empty)
import           Data.Reflection                ( Given, given, give )
import           Nix.Types.Atom                 ( NAtom(..) )
import           Nix.Core.Expr.Types            ( emptyPositionSet )
import           Nix.Core.Value                 ( NValue
                                                , NVConstraint
                                                , pattern NVConstant
                                                , pattern NVList
                                                , pattern NVSet
                                                , pattern NVStr
                                                )
import qualified Nix.List.Sig                  as L

-- | Constraint alias for functions that access interned values.
--
-- Use this in function signatures instead of writing out the full constraint:
--
-- @
-- myFunc :: GivenInterned t f m => NValue t f m
-- myFunc = internedBool True
-- @
type GivenInterned t f m = Given (InternedValues t f m)


-- | Cache of interned constant values, created once per evaluation.
--
-- These values are parameterized by the full NValue type parameters to ensure
-- sharing within each evaluation run. The strictness annotations ensure
-- values are fully evaluated at construction time.
--
-- All stored values are constants (no thunks, no closures), so they're
-- safe to share across the entire evaluation.
data InternedValues t f m = InternedValues
  { internedTrue'        :: !(NValue t f m)
  -- ^ The singleton @true@ boolean value (internal field, use 'internedTrue').
  , internedFalse'       :: !(NValue t f m)
  -- ^ The singleton @false@ boolean value (internal field, use 'internedFalse').
  , internedNull'        :: !(NValue t f m)
  -- ^ The singleton @null@ value (internal field, use 'internedNull').
  , internedEmptyList'   :: !(NValue t f m)
  -- ^ The singleton empty list @[]@ (internal field, use 'internedEmptyList').
  , internedEmptySet'    :: !(NValue t f m)
  -- ^ The singleton empty attribute set @{}@ (internal field, use 'internedEmptySet').
  , internedEmptyString' :: !(NValue t f m)
  -- ^ The singleton empty string @""@ (internal field, use 'internedEmptyString').
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
  { internedTrue'        = NVConstant (NBool True)
  , internedFalse'       = NVConstant (NBool False)
  , internedNull'        = NVConstant NNull
  , internedEmptyList'   = NVList L.empty
  , internedEmptySet'    = NVSet emptyPositionSet mempty
  , internedEmptyString' = NVStr mempty  -- mempty for NixString is ""
  }
{-# INLINABLE mkInternedValues #-}

-- * Pure accessors using Given constraint
--
-- These functions provide zero-overhead access to interned values.
-- The Given constraint is satisfied by using 'give mkInternedValues'
-- at the evaluation entry point.

-- | The singleton @true@ boolean value.
internedTrue :: GivenInterned t f m => NValue t f m
internedTrue = internedTrue' given
{-# INLINE internedTrue #-}

-- | The singleton @false@ boolean value.
internedFalse :: GivenInterned t f m => NValue t f m
internedFalse = internedFalse' given
{-# INLINE internedFalse #-}

-- | The singleton @null@ value.
internedNull :: GivenInterned t f m => NValue t f m
internedNull = internedNull' given
{-# INLINE internedNull #-}

-- | The singleton empty list @[]@.
internedEmptyList :: GivenInterned t f m => NValue t f m
internedEmptyList = internedEmptyList' given
{-# INLINE internedEmptyList #-}

-- | The singleton empty attribute set @{}@.
internedEmptySet :: GivenInterned t f m => NValue t f m
internedEmptySet = internedEmptySet' given
{-# INLINE internedEmptySet #-}

-- | The singleton empty string @""@.
internedEmptyString :: GivenInterned t f m => NValue t f m
internedEmptyString = internedEmptyString' given
{-# INLINE internedEmptyString #-}

-- | Return an interned boolean value based on a condition.
--
-- @
-- internedBool True  = internedTrue
-- internedBool False = internedFalse
-- @
internedBool :: GivenInterned t f m => Bool -> NValue t f m
internedBool True  = internedTrue
internedBool False = internedFalse
{-# INLINE internedBool #-}
