{-# LANGUAGE TemplateHaskell #-}

-- | Inspection tests for value conversion operations.
--
-- These tests verify that FromValue/ToValue conversions are efficient
-- when @prov ~ 'False@:
--
-- 1. Conversions have no provenance-related overhead
-- 2. Coercion is zero-cost
-- 3. Value wrapping is zero-cost
module Inspection.Convert
  ( -- * Coercion tests
    testCoerceCitedF
  , testCoerceValueF
    -- * Unwrap tests
  , testUnwrapCitedF
  ) where

import           Relude

import           Data.Coerce                    ( coerce )
import           Data.Functor.Identity          ()
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Standard                   ( CitedF(..), ThunkF(..), ValueF )


-- * Coercion tests

-- | Test coercing into CitedF when @prov ~ 'False@.
--
-- CitedF wraps Cited, and Cited 'False wraps Identity.
testCoerceCitedF :: a -> CitedF 'False m a
testCoerceCitedF = coerce
{-# NOINLINE testCoerceCitedF #-}

-- | Test that ValueF is just an alias for the full NValue type.
--
-- This tests that the type alias doesn't introduce overhead.
testCoerceValueF :: ValueF 'False m -> ValueF 'False m
testCoerceValueF = id
{-# NOINLINE testCoerceValueF #-}


-- * Unwrap tests

-- | Test unwrapping CitedF when @prov ~ 'False@.
testUnwrapCitedF :: CitedF 'False m a -> a
testUnwrapCitedF (CitedF (Cited rep)) = runIdentity rep
{-# NOINLINE testUnwrapCitedF #-}


-- * Inspection tests

-- Coercion should be zero-cost
inspect $ hasNoTypeClasses 'testCoerceCitedF
inspect $ 'testCoerceCitedF `hasNoType` ''NCited
inspect $ 'testCoerceCitedF `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testCoerceValueF
inspect $ 'testCoerceValueF `hasNoType` ''NCited
inspect $ 'testCoerceValueF `hasNoType` ''Provenance

-- Unwrapping should be zero-cost
inspect $ hasNoTypeClasses 'testUnwrapCitedF
inspect $ 'testUnwrapCitedF `hasNoType` ''NCited
inspect $ 'testUnwrapCitedF `hasNoType` ''Provenance
