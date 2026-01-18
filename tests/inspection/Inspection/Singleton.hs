{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-inaccessible-code #-}

-- | Inspection tests for singleton bool dispatch.
--
-- These tests verify that GHC's case-of-known-constructor optimization
-- eliminates the unused branch when 'sbool' is used with a concrete
-- type-level Bool.
--
-- When @sbool @'False@ is used in:
-- @
-- case sbool @'False of
--   STrue  -> expensiveTrueBranch
--   SFalse -> cheapFalseBranch
-- @
--
-- GHC should eliminate the entire 'STrue' branch at compile time.
module Inspection.Singleton
  ( -- * Test wrappers
    testSboolFalse
  , testSboolTrue
  , testIfSBoolFalse
  , testIfSBoolTrue
    -- * Expected implementations
  , expectedSboolFalse
  , expectedSboolTrue
  , expectedIfSBoolFalse
  , expectedIfSBoolTrue
  ) where

import           Relude

import           Test.Inspection

import           Nix.Config.Singleton           ( SBool(..)
                                                , sbool
                                                , ifSBool
                                                )


-- * Test wrappers for sbool dispatch

-- | Test case dispatch on @sbool @'False@.
--
-- Should compile to direct selection of the second argument.
testSboolFalse :: a -> a -> a
testSboolFalse trueCase falseCase = case sbool @'False of
  STrue  -> trueCase
  SFalse -> falseCase
{-# NOINLINE testSboolFalse #-}

-- | Test case dispatch on @sbool @'True@.
--
-- Should compile to direct selection of the first argument.
testSboolTrue :: a -> a -> a
testSboolTrue trueCase falseCase = case sbool @'True of
  STrue  -> trueCase
  SFalse -> falseCase
{-# NOINLINE testSboolTrue #-}

-- | Test 'ifSBool' helper on @SFalse@.
--
-- 'ifSBool' is a helper that wraps the sbool dispatch pattern.
-- Should compile to direct selection of the second argument.
testIfSBoolFalse :: a -> a -> a
testIfSBoolFalse trueCase falseCase = ifSBool SFalse trueCase falseCase
{-# NOINLINE testIfSBoolFalse #-}

-- | Test 'ifSBool' helper on @STrue@.
--
-- Should compile to direct selection of the first argument.
testIfSBoolTrue :: a -> a -> a
testIfSBoolTrue trueCase falseCase = ifSBool STrue trueCase falseCase
{-# NOINLINE testIfSBoolTrue #-}


-- * Expected implementations

-- | Expected result of @testSboolFalse@: returns the false case.
expectedSboolFalse :: a -> a -> a
expectedSboolFalse _ falseCase = falseCase
{-# NOINLINE expectedSboolFalse #-}

-- | Expected result of @testSboolTrue@: returns the true case.
expectedSboolTrue :: a -> a -> a
expectedSboolTrue trueCase _ = trueCase
{-# NOINLINE expectedSboolTrue #-}

-- | Expected result of @testIfSBoolFalse@: returns the false case.
expectedIfSBoolFalse :: a -> a -> a
expectedIfSBoolFalse _ falseCase = falseCase
{-# NOINLINE expectedIfSBoolFalse #-}

-- | Expected result of @testIfSBoolTrue@: returns the true case.
expectedIfSBoolTrue :: a -> a -> a
expectedIfSBoolTrue trueCase _ = trueCase
{-# NOINLINE expectedIfSBoolTrue #-}


-- * Inspection tests
--
-- These are registered at compile time via Template Haskell.

-- Test 1: sbool @'False dispatch compiles to selecting false case
inspect $ 'testSboolFalse ==- 'expectedSboolFalse

-- Test 2: sbool @'True dispatch compiles to selecting true case
inspect $ 'testSboolTrue ==- 'expectedSboolTrue

-- Test 3: ifSBool SFalse compiles to selecting false case
inspect $ 'testIfSBoolFalse ==- 'expectedIfSBoolFalse

-- Test 4: ifSBool STrue compiles to selecting true case
inspect $ 'testIfSBoolTrue ==- 'expectedIfSBoolTrue

-- Test 5: testSboolFalse has no SBoolI dictionary
-- (type class constraint should be resolved at compile time)
inspect $ hasNoTypeClasses 'testSboolFalse

-- Test 6: testSboolTrue has no SBoolI dictionary
inspect $ hasNoTypeClasses 'testSboolTrue

-- Test 7: testIfSBoolFalse has no type class dictionaries
inspect $ hasNoTypeClasses 'testIfSBoolFalse

-- Test 8: testIfSBoolTrue has no type class dictionaries
inspect $ hasNoTypeClasses 'testIfSBoolTrue
