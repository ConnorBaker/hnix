{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-inaccessible-code #-}

-- | Inspection tests for EvalCfg configuration dispatch.
--
-- These tests verify that config singletons (singStats, singTrace) specialize
-- correctly and compile to direct branch selection.
--
-- When using DefaultCfg (stats=False, trace=False), GHC should eliminate all
-- conditional branches for disabled features.
module Inspection.Config
  ( -- * Individual config flag tests
    testSingStats
  , testSingTrace
    -- * Helper function tests
  , testIfStats
  , testIfTrace
    -- * Combined condition tests
  , testCombinedStatsAndTrace
    -- * Expected implementations
  , expectedSelectFalse
  ) where

import           Relude

import           Test.Inspection

import           Nix.Config.Singleton           ( DefaultCfg
                                                , SBool(..)
                                                , singStats
                                                , singTrace
                                                , ifStats
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

-- | Test ifTrace helper with DefaultCfg.
testIfTrace :: a -> a -> a
testIfTrace = ifTrace @DefaultCfg
{-# NOINLINE testIfTrace #-}


-- * Combined condition tests

-- | Test combined stats AND trace check.
--
-- When both are false (DefaultCfg), the result should be @fallbackCase@.
testCombinedStatsAndTrace :: a -> a -> a -> a
testCombinedStatsAndTrace statsCase traceCase fallbackCase =
  case singStats @DefaultCfg of
    STrue  -> statsCase
    SFalse -> case singTrace @DefaultCfg of
      STrue  -> traceCase
      SFalse -> fallbackCase
{-# NOINLINE testCombinedStatsAndTrace #-}


-- * Expected implementations

-- | Expected: select the false (second) case.
expectedSelectFalse :: a -> a -> a
expectedSelectFalse _ falseCase = falseCase
{-# NOINLINE expectedSelectFalse #-}

-- | Expected for testCombinedStatsAndTrace: select the third (fallback) case.
expectedCombinedStatsAndTrace :: a -> a -> a -> a
expectedCombinedStatsAndTrace _ _ fallbackCase = fallbackCase
{-# NOINLINE expectedCombinedStatsAndTrace #-}


-- * Inspection tests

-- Individual config flag tests - should have no dictionaries
inspect $ hasNoTypeClasses 'testSingStats
inspect $ hasNoTypeClasses 'testSingTrace

-- Should compile to selecting false case
inspect $ 'testSingStats ==- 'expectedSelectFalse
inspect $ 'testSingTrace ==- 'expectedSelectFalse

-- Helper function tests - should have no dictionaries
inspect $ hasNoTypeClasses 'testIfStats
inspect $ hasNoTypeClasses 'testIfTrace

-- Note: Equivalence tests (==- expectedSelectFalse) are not used for ifStats/ifTrace
-- because GHC keeps the ifSBool wrapper in Core even though it specializes correctly.
-- The hasNoTypeClasses tests above verify the key property: no dictionary passing.

-- Combined tests - should have no dictionaries
inspect $ hasNoTypeClasses 'testCombinedStatsAndTrace

-- Combined tests should compile to selecting fallback
inspect $ 'testCombinedStatsAndTrace ==- 'expectedCombinedStatsAndTrace
