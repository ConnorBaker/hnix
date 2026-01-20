{-# LANGUAGE TemplateHaskell #-}

-- | Inspection tests for thunk type specialization.
--
-- These tests verify that when @prov ~ 'False@:
--
-- 1. 'CitedF' wrapper is properly erased (no runtime overhead)
-- 2. 'ThunkF' wrapper is properly erased (no runtime overhead)
-- 3. Provenance-related functions are not used in the generated code
--
-- The goal is to verify that the unified @prov@-parameterized types compile
-- to zero-overhead code for the common case (@prov ~ 'False@).
module Inspection.Thunk
  ( -- * Test wrappers
    testCitedFExtract
  , testCitedFFmap
    -- * Expected implementations
  , expectedCitedFExtract
  , expectedCitedFFmap
  ) where

import           Relude

import           Data.Functor.Identity          ()  -- for ''Identity in hasNoType
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..)
                                                , extractCited
                                                )
import           Nix.Standard                   ( CitedF(..) )


-- * Test wrappers for CitedF operations

-- | Extract value from 'CitedF' when @prov ~ 'False@.
--
-- CitedF is a newtype over Cited, and when prov ~ 'False, Cited wraps Identity.
-- After newtype erasure, this should compile to the identity function.
testCitedFExtract :: CitedF 'False m a -> a
testCitedFExtract (CitedF cited) = extractCited cited
{-# NOINLINE testCitedFExtract #-}

-- | Map over 'CitedF' when @prov ~ 'False@.
--
-- This tests that the Functor instance for CitedF compiles without overhead.
testCitedFFmap :: (a -> b) -> CitedF 'False m a -> CitedF 'False m b
testCitedFFmap f (CitedF cited) = CitedF (fmap f cited)
{-# NOINLINE testCitedFFmap #-}


-- * Expected implementations

-- | Expected implementation of 'testCitedFExtract'.
--
-- After all newtype erasure (CitedF -> Cited -> Identity), this should be
-- equivalent to extracting from Identity: \\(CitedF (Cited (Identity a))) -> a
expectedCitedFExtract :: CitedF 'False m a -> a
expectedCitedFExtract (CitedF (Cited rep)) = runIdentity rep
{-# NOINLINE expectedCitedFExtract #-}

-- | Expected implementation of 'testCitedFFmap'.
--
-- After newtype erasure, fmap over Identity should be zero-cost.
expectedCitedFFmap :: (a -> b) -> CitedF 'False m a -> CitedF 'False m b
expectedCitedFFmap f (CitedF (Cited rep)) = CitedF (Cited (fmap f rep))
{-# NOINLINE expectedCitedFFmap #-}


-- * Inspection tests
--
-- These are registered at compile time via Template Haskell.

-- Test 1: CitedF extract has no SBoolI dictionary
inspect $ hasNoTypeClasses 'testCitedFExtract

-- Test 2: CitedF extract has no NCited type
inspect $ 'testCitedFExtract `hasNoType` ''NCited

-- Test 3: CitedF extract has no Provenance type
-- (provenance should not appear in prov ~ 'False code)
inspect $ 'testCitedFExtract `hasNoType` ''Provenance

-- Test 4: CitedF extract is equivalent to expected (newtype erasure)
inspect $ 'testCitedFExtract ==- 'expectedCitedFExtract

-- Test 5: CitedF fmap has no SBoolI dictionary
inspect $ hasNoTypeClasses 'testCitedFFmap

-- Test 6: CitedF fmap has no NCited type
inspect $ 'testCitedFFmap `hasNoType` ''NCited

-- Test 7: CitedF fmap is equivalent to expected
inspect $ 'testCitedFFmap ==- 'expectedCitedFFmap

-- Test 8: testCitedFExtract has no Identity type (newtype fully erased)
inspect $ 'testCitedFExtract `hasNoType` ''Identity

-- Test 9: testCitedFFmap has no Identity type (newtype fully erased)
inspect $ 'testCitedFFmap `hasNoType` ''Identity
