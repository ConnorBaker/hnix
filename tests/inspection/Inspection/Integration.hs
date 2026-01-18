{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-inaccessible-code #-}

-- | Integration inspection tests for the full evaluator type stack.
--
-- These tests verify end-to-end specialization properties:
--
-- 1. The full Cited -> CitedF wrapper chain is erased
-- 2. Config singleton dispatch (via KnownEvalCfg) is specialized
-- 3. No provenance types appear in the common case code path
--
-- Unlike the unit tests in other modules, these tests exercise combinations
-- of the optimization mechanisms working together.
module Inspection.Integration
  ( -- * Full wrapper chain tests
    testFullExtract
    -- * Config-aware tests
  , testConfigDispatch
    -- * Expected implementations
  , expectedConfigDispatch
  ) where

import           Relude

import           Data.Functor.Identity          ()  -- for ''Identity in hasNoType
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( extractCited )
import           Nix.Config.Singleton           ( DefaultCfg
                                                , SBool(..)
                                                , singStats
                                                )
import           Nix.Standard                   ( CitedF(..) )


-- * Full wrapper chain tests

-- | Extract from CitedF -> Cited chain when @prov ~ 'False@.
--
-- CitedF wraps Cited, so this tests the full extraction path.
-- After newtype erasure, this should compile to the identity function.
testFullExtract :: CitedF 'False m a -> a
testFullExtract (CitedF cited) = extractCited cited
{-# NOINLINE testFullExtract #-}


-- * Config-aware tests

-- | Test that config singleton dispatch specializes correctly.
--
-- When DefaultCfg is used, all flags are 'False, so:
-- - singStats @DefaultCfg = SFalse
-- - singProv @DefaultCfg = SFalse
-- - singTrace @DefaultCfg = SFalse
--
-- The case dispatch should be eliminated, selecting the second branch.
testConfigDispatch :: a -> a -> a
testConfigDispatch trueCase falseCase =
  case singStats @DefaultCfg of
    STrue  -> trueCase
    SFalse -> falseCase
{-# NOINLINE testConfigDispatch #-}


-- * Expected implementations

-- | Expected implementation: return false case.
expectedConfigDispatch :: a -> a -> a
expectedConfigDispatch _ falseCase = falseCase
{-# NOINLINE expectedConfigDispatch #-}


-- * Inspection tests
--
-- These are registered at compile time via Template Haskell.

-- Test 1: Full extraction chain has no SBoolI dictionary
inspect $ hasNoTypeClasses 'testFullExtract

-- Test 2: Full extraction chain has no NCited type
inspect $ 'testFullExtract `hasNoType` ''NCited

-- Test 3: Full extraction chain has no Provenance type
inspect $ 'testFullExtract `hasNoType` ''Provenance

-- Test 4: Full extraction chain has no Identity type (fully erased)
inspect $ 'testFullExtract `hasNoType` ''Identity

-- Test 5: Config dispatch has no type class dictionaries
inspect $ hasNoTypeClasses 'testConfigDispatch

-- Test 6: Config dispatch compiles to selecting false case
inspect $ 'testConfigDispatch ==- 'expectedConfigDispatch
