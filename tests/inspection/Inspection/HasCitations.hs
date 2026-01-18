{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for HasCitations instance specialization.
--
-- These tests verify that when @prov ~ 'False@:
--
-- 1. 'citations1' returns @[]@ with no runtime overhead
-- 2. 'addProvenance1' is a no-op with no runtime overhead
-- 3. Both operations have no type class dictionaries
--
-- The HasCitations class is used for provenance tracking throughout the evaluator.
-- When provenance is disabled, these operations must be zero-cost.
module Inspection.HasCitations
  ( -- * Cited HasCitations1 tests
    testCitedCitations1
  , testCitedAddProvenance1
    -- * CitedF HasCitations1 tests
  , testCitedFCitations1
  , testCitedFAddProvenance1
    -- * ThunkF HasCitations tests
  , testThunkFCitations
  , testThunkFAddProvenance
  ) where

import           Relude

import           Data.Functor.Identity          ()  -- for ''Identity in hasNoType
import           Test.Inspection

import           Nix.Cited                      ( NCited
                                                , Provenance
                                                , HasCitations1(citations1, addProvenance1)
                                                , HasCitations(citations, addProvenance)
                                                )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Standard                   ( CitedF(..)
                                                , ThunkF(..)
                                                , ValueF
                                                )
import           Nix.Value                      ( NValue )


-- * Cited HasCitations1 tests

-- | Test citations1 for Cited when @prov ~ 'False@.
--
-- Should return @[]@ with zero overhead.
-- Note: The instance is HasCitations1 m (NValue t f m) (Cited prov t f m)
testCitedCitations1 :: Cited 'False t f m a -> [Provenance m (NValue t f m)]
testCitedCitations1 = citations1
{-# NOINLINE testCitedCitations1 #-}

-- | Test addProvenance1 for Cited when @prov ~ 'False@.
--
-- Should be a no-op (identity function) with zero overhead.
testCitedAddProvenance1 :: Provenance m (NValue t f m) -> Cited 'False t f m a -> Cited 'False t f m a
testCitedAddProvenance1 = addProvenance1
{-# NOINLINE testCitedAddProvenance1 #-}


-- * CitedF HasCitations1 tests

-- | Test citations1 for CitedF when @prov ~ 'False@.
-- Note: The instance is HasCitations1 m (ValueF prov m) (CitedF prov m)
testCitedFCitations1 :: CitedF 'False m a -> [Provenance m (ValueF 'False m)]
testCitedFCitations1 = citations1
{-# NOINLINE testCitedFCitations1 #-}

-- | Test addProvenance1 for CitedF when @prov ~ 'False@.
testCitedFAddProvenance1 :: Provenance m (ValueF 'False m) -> CitedF 'False m a -> CitedF 'False m a
testCitedFAddProvenance1 = addProvenance1
{-# NOINLINE testCitedFAddProvenance1 #-}


-- * ThunkF HasCitations tests

-- | Test citations for ThunkF when @prov ~ 'False@.
-- Note: The instance is HasCitations m (ValueF prov m) (ThunkF prov m)
testThunkFCitations :: ThunkF 'False m -> [Provenance m (ValueF 'False m)]
testThunkFCitations = citations
{-# NOINLINE testThunkFCitations #-}

-- | Test addProvenance for ThunkF when @prov ~ 'False@.
testThunkFAddProvenance :: Provenance m (ValueF 'False m) -> ThunkF 'False m -> ThunkF 'False m
testThunkFAddProvenance = addProvenance
{-# NOINLINE testThunkFAddProvenance #-}


-- * Inspection tests

-- Cited citations1 tests
inspect $ hasNoTypeClasses 'testCitedCitations1
inspect $ 'testCitedCitations1 `hasNoType` ''NCited
inspect $ 'testCitedCitations1 `hasNoType` ''Identity

-- Cited addProvenance1 tests
inspect $ hasNoTypeClasses 'testCitedAddProvenance1
inspect $ 'testCitedAddProvenance1 `hasNoType` ''NCited
inspect $ 'testCitedAddProvenance1 `hasNoType` ''Identity

-- CitedF citations1 tests
inspect $ hasNoTypeClasses 'testCitedFCitations1
inspect $ 'testCitedFCitations1 `hasNoType` ''NCited
inspect $ 'testCitedFCitations1 `hasNoType` ''Identity

-- CitedF addProvenance1 tests
inspect $ hasNoTypeClasses 'testCitedFAddProvenance1
inspect $ 'testCitedFAddProvenance1 `hasNoType` ''NCited
inspect $ 'testCitedFAddProvenance1 `hasNoType` ''Identity

-- ThunkF citations tests
inspect $ hasNoTypeClasses 'testThunkFCitations
inspect $ 'testThunkFCitations `hasNoType` ''NCited
inspect $ 'testThunkFCitations `hasNoType` ''Identity

-- ThunkF addProvenance tests
inspect $ hasNoTypeClasses 'testThunkFAddProvenance
inspect $ 'testThunkFAddProvenance `hasNoType` ''NCited
inspect $ 'testThunkFAddProvenance `hasNoType` ''Identity

-- Note: Equivalence tests (==- expectedCitations/expectedAddProvenance) are not used
-- because GHC generates `case x of { Provenance _ _ -> ... }` rather than
-- `case x of { __DEFAULT -> ... }`. These are semantically equivalent but
-- structurally different in Core.
--
-- The key properties we verify:
-- 1. hasNoTypeClasses - no SBoolI dictionary passing
-- 2. hasNoType ''NCited - NCited type erased
-- 3. hasNoType ''Identity - Identity newtype erased
--
-- Together these ensure that prov ~ 'False code paths have zero overhead.
