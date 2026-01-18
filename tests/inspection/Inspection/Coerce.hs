{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for newtype coercion (zero-cost wrapper erasure).
--
-- These tests verify that:
--
-- 1. Coercing between newtype wrappers is zero-cost
-- 2. The Cited/CitedF/ThunkF newtype chain compiles away
-- 3. No wrapper constructors appear in the generated Core
--
-- GHC's newtype optimization should erase all wrapper overhead when coercing
-- between representationally equal types.
module Inspection.Coerce
  ( -- * Direct coercion tests
    testCoerceCitedToRep
  , testCoerceRepToCited
    -- * Identity tests
  , testCoerceRoundtrip
    -- * Expected implementations
  , expectedId
  ) where

import           Relude

import           Data.Coerce                    ( coerce )
import           Data.Functor.Identity          ( Identity(..) )
import           Test.Inspection

import           Nix.Cited                      ( NCited )
import           Nix.Cited.Basic                ( Cited(..), CitedRep )
import           Nix.Standard                   ( CitedF(..) )
import           Nix.Value                      ( NValue )


-- * Direct coercion tests

-- | Test coercing Cited to its representation when @prov ~ 'False@.
--
-- CitedRep 'False m v a = Identity a, so this should be zero-cost.
testCoerceCitedToRep :: Cited 'False t f m a -> Identity a
testCoerceCitedToRep = coerce
{-# NOINLINE testCoerceCitedToRep #-}

-- | Test coercing representation back to Cited when @prov ~ 'False@.
testCoerceRepToCited :: Identity a -> Cited 'False t f m a
testCoerceRepToCited = coerce
{-# NOINLINE testCoerceRepToCited #-}


-- * Identity tests

-- | Test that coercing Cited -> Identity -> Cited is identity.
testCoerceRoundtrip :: Cited 'False t f m a -> Cited 'False t f m a
testCoerceRoundtrip = coerce . (coerce :: Cited 'False t f m a -> Identity a)
{-# NOINLINE testCoerceRoundtrip #-}


-- * Expected implementations

-- | Expected implementation: identity function.
expectedId :: a -> a
expectedId x = x
{-# NOINLINE expectedId #-}


-- * Inspection tests

-- Direct coercion tests - should have no type class dictionaries
inspect $ hasNoTypeClasses 'testCoerceCitedToRep
inspect $ hasNoTypeClasses 'testCoerceRepToCited

-- No NCited type in prov ~ 'False coercions
inspect $ 'testCoerceCitedToRep `hasNoType` ''NCited
inspect $ 'testCoerceRepToCited `hasNoType` ''NCited

-- Roundtrip tests - should be equivalent to identity
inspect $ hasNoTypeClasses 'testCoerceRoundtrip
inspect $ 'testCoerceRoundtrip ==- 'expectedId

-- No Identity type in generated Core (newtype should be erased)
inspect $ 'testCoerceRoundtrip `hasNoType` ''Identity
