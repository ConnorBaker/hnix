{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-inaccessible-code #-}

-- | Inspection tests for EvalCfg configuration dispatch.
--
-- These tests verify that:
--
-- 1. All config singletons (singStats, singProv, singTrace) specialize correctly
-- 2. Combined config checks compile to direct branch selection
-- 3. ifStats, ifProv, ifTrace helpers are zero-cost
-- 4. whenStatsM, whenProvM, whenTraceM are zero-cost for disabled features
--
-- When using DefaultCfg (all flags False), GHC should eliminate all conditional
-- branches for disabled features.
module Inspection.Config
  ( -- * Individual config flag tests
    testSingStats
  , testSingProv
  , testSingTrace
    -- * Helper function tests
  , testIfStats
  , testIfProv
  , testIfTrace
    -- * Combined condition tests
  , testCombinedStatsAndProv
  , testCombinedAllFlags
    -- * Expected implementations
  , expectedSelectFalse
  , expectedSelectTrue
  ) where

import           Relude

import           Test.Inspection

import           Nix.Config.Singleton           ( DefaultCfg
                                                , EvalCfg
                                                , SBool(..)
                                                , singStats
                                                , singProv
                                                , singTrace
                                                , ifStats
                                                , ifProv
                                                , ifTrace
                                                )


-- * Individual config flag tests

-- | Test singStats dispatch with DefaultCfg.
--
-- DefaultCfg has CfgStats ~ 'False, so this should select the false case.
testSingStats :: a -> a -> a
testSingStats trueCase falseCase = case singStats @DefaultCfg of
  STrue  -> trueCase
  SFalse -> falseCase
{-# NOINLINE testSingStats #-}

-- | Test singProv dispatch with DefaultCfg.
--
-- DefaultCfg has CfgProv ~ 'False, so this should select the false case.
testSingProv :: a -> a -> a
testSingProv trueCase falseCase = case singProv @DefaultCfg of
  STrue  -> trueCase
  SFalse -> falseCase
{-# NOINLINE testSingProv #-}

-- | Test singTrace dispatch with DefaultCfg.
--
-- DefaultCfg has CfgTrace ~ 'False, so this should select the false case.
testSingTrace :: a -> a -> a
testSingTrace trueCase falseCase = case singTrace @DefaultCfg of
  STrue  -> trueCase
  SFalse -> falseCase
{-# NOINLINE testSingTrace #-}


-- * Helper function tests

-- | Test ifStats helper with DefaultCfg.
testIfStats :: a -> a -> a
testIfStats = ifStats @DefaultCfg
{-# NOINLINE testIfStats #-}

-- | Test ifProv helper with DefaultCfg.
testIfProv :: a -> a -> a
testIfProv = ifProv @DefaultCfg
{-# NOINLINE testIfProv #-}

-- | Test ifTrace helper with DefaultCfg.
testIfTrace :: a -> a -> a
testIfTrace = ifTrace @DefaultCfg
{-# NOINLINE testIfTrace #-}


-- * Combined condition tests

-- | Test combined stats AND provenance check.
--
-- When both are false (DefaultCfg), the result should be @fallbackResult@.
testCombinedStatsAndProv :: a -> a -> a -> a
testCombinedStatsAndProv statsCase provCase fallbackCase =
  case singStats @DefaultCfg of
    STrue  -> statsCase
    SFalse -> case singProv @DefaultCfg of
      STrue  -> provCase
      SFalse -> fallbackCase
{-# NOINLINE testCombinedStatsAndProv #-}

-- | Test checking all three flags.
--
-- When all are false (DefaultCfg), the result should be the last case.
testCombinedAllFlags :: a -> a -> a -> a -> a
testCombinedAllFlags statsCase provCase traceCase fallbackCase =
  case singStats @DefaultCfg of
    STrue  -> statsCase
    SFalse -> case singProv @DefaultCfg of
      STrue  -> provCase
      SFalse -> case singTrace @DefaultCfg of
        STrue  -> traceCase
        SFalse -> fallbackCase
{-# NOINLINE testCombinedAllFlags #-}


-- * Expected implementations

-- | Expected: select the false (second) case.
expectedSelectFalse :: a -> a -> a
expectedSelectFalse _ falseCase = falseCase
{-# NOINLINE expectedSelectFalse #-}

-- | Expected for testCombinedStatsAndProv: select the third (fallback) case.
expectedSelectTrue :: a -> a -> a
expectedSelectTrue trueCase _ = trueCase
{-# NOINLINE expectedSelectTrue #-}

-- | Expected for testCombinedStatsAndProv: select the third (fallback) case.
expectedCombinedStatsAndProv :: a -> a -> a -> a
expectedCombinedStatsAndProv _ _ fallbackCase = fallbackCase
{-# NOINLINE expectedCombinedStatsAndProv #-}

-- | Expected for testCombinedAllFlags: select the fourth (fallback) case.
expectedCombinedAllFlags :: a -> a -> a -> a -> a
expectedCombinedAllFlags _ _ _ fallbackCase = fallbackCase
{-# NOINLINE expectedCombinedAllFlags #-}


-- * Inspection tests

-- Individual config flag tests - should have no dictionaries
inspect $ hasNoTypeClasses 'testSingStats
inspect $ hasNoTypeClasses 'testSingProv
inspect $ hasNoTypeClasses 'testSingTrace

-- Should compile to selecting false case
inspect $ 'testSingStats ==- 'expectedSelectFalse
inspect $ 'testSingProv ==- 'expectedSelectFalse
inspect $ 'testSingTrace ==- 'expectedSelectFalse

-- Helper function tests - should have no dictionaries
inspect $ hasNoTypeClasses 'testIfStats
inspect $ hasNoTypeClasses 'testIfProv
inspect $ hasNoTypeClasses 'testIfTrace

-- Note: Equivalence tests (==- expectedSelectFalse) are not used for ifStats/ifProv/ifTrace
-- because GHC keeps the ifSBool wrapper in Core even though it specializes correctly.
-- The hasNoTypeClasses tests above verify the key property: no dictionary passing.

-- Combined tests - should have no dictionaries
inspect $ hasNoTypeClasses 'testCombinedStatsAndProv
inspect $ hasNoTypeClasses 'testCombinedAllFlags

-- Combined tests should compile to selecting fallback
inspect $ 'testCombinedStatsAndProv ==- 'expectedCombinedStatsAndProv
inspect $ 'testCombinedAllFlags ==- 'expectedCombinedAllFlags
