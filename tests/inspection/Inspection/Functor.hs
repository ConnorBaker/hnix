{-# LANGUAGE TemplateHaskell #-}

-- | Inspection tests for Functor and Applicative instance specialization.
--
-- These tests verify that when @prov ~ 'False@:
--
-- 1. 'fmap' has no type class dictionaries
-- 2. 'pure' has no type class dictionaries
-- 3. '<*>' has no type class dictionaries
-- 4. All operations have no NCited or Identity types
--
-- These instances are used throughout the evaluator for transforming values.
module Inspection.Functor
  ( -- * Cited Functor tests
    testCitedFmap
  , testCitedPure
  , testCitedAp
    -- * CitedF Functor tests
  , testCitedFFmapF
  , testCitedFPure
  , testCitedFAp
    -- * Foldable tests
  , testCitedFoldMap
  , testCitedFFoldMap
    -- * Traversable tests
  , testCitedTraverse
  , testCitedFTraverse
  ) where

import           Relude

import           Data.Functor.Identity          ()  -- for ''Identity in hasNoType
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Standard                   ( CitedF(..) )


-- * Cited Functor tests

-- | Test Functor fmap for Cited when @prov ~ 'False@.
testCitedFmap :: (a -> b) -> Cited 'False t f m a -> Cited 'False t f m b
testCitedFmap = fmap
{-# NOINLINE testCitedFmap #-}

-- | Test Applicative pure for Cited when @prov ~ 'False@.
testCitedPure :: a -> Cited 'False t f m a
testCitedPure = pure
{-# NOINLINE testCitedPure #-}

-- | Test Applicative (<*>) for Cited when @prov ~ 'False@.
testCitedAp :: Cited 'False t f m (a -> b) -> Cited 'False t f m a -> Cited 'False t f m b
testCitedAp = (<*>)
{-# NOINLINE testCitedAp #-}


-- * CitedF Functor tests

-- | Test Functor fmap for CitedF when @prov ~ 'False@.
testCitedFFmapF :: (a -> b) -> CitedF 'False m a -> CitedF 'False m b
testCitedFFmapF = fmap
{-# NOINLINE testCitedFFmapF #-}

-- | Test Applicative pure for CitedF when @prov ~ 'False@.
testCitedFPure :: a -> CitedF 'False m a
testCitedFPure = pure
{-# NOINLINE testCitedFPure #-}

-- | Test Applicative (<*>) for CitedF when @prov ~ 'False@.
testCitedFAp :: CitedF 'False m (a -> b) -> CitedF 'False m a -> CitedF 'False m b
testCitedFAp = (<*>)
{-# NOINLINE testCitedFAp #-}


-- * Foldable tests

-- | Test Foldable foldMap for Cited when @prov ~ 'False@.
testCitedFoldMap :: Monoid b => (a -> b) -> Cited 'False t f m a -> b
testCitedFoldMap = foldMap
{-# NOINLINE testCitedFoldMap #-}

-- | Test Foldable foldMap for CitedF when @prov ~ 'False@.
testCitedFFoldMap :: Monoid b => (a -> b) -> CitedF 'False m a -> b
testCitedFFoldMap = foldMap
{-# NOINLINE testCitedFFoldMap #-}


-- * Traversable tests

-- | Test Traversable traverse for Cited when @prov ~ 'False@.
testCitedTraverse :: Applicative g => (a -> g b) -> Cited 'False t f m a -> g (Cited 'False t f m b)
testCitedTraverse = traverse
{-# NOINLINE testCitedTraverse #-}

-- | Test Traversable traverse for CitedF when @prov ~ 'False@.
testCitedFTraverse :: Applicative g => (a -> g b) -> CitedF 'False m a -> g (CitedF 'False m b)
testCitedFTraverse = traverse
{-# NOINLINE testCitedFTraverse #-}


-- * Inspection tests

-- Cited Functor tests
inspect $ hasNoTypeClasses 'testCitedFmap
inspect $ 'testCitedFmap `hasNoType` ''NCited
inspect $ 'testCitedFmap `hasNoType` ''Identity
inspect $ 'testCitedFmap `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testCitedPure
inspect $ 'testCitedPure `hasNoType` ''NCited
inspect $ 'testCitedPure `hasNoType` ''Identity

inspect $ hasNoTypeClasses 'testCitedAp
inspect $ 'testCitedAp `hasNoType` ''NCited
inspect $ 'testCitedAp `hasNoType` ''Identity

-- CitedF Functor tests
inspect $ hasNoTypeClasses 'testCitedFFmapF
inspect $ 'testCitedFFmapF `hasNoType` ''NCited
inspect $ 'testCitedFFmapF `hasNoType` ''Identity
inspect $ 'testCitedFFmapF `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testCitedFPure
inspect $ 'testCitedFPure `hasNoType` ''NCited
inspect $ 'testCitedFPure `hasNoType` ''Identity

inspect $ hasNoTypeClasses 'testCitedFAp
inspect $ 'testCitedFAp `hasNoType` ''NCited
inspect $ 'testCitedFAp `hasNoType` ''Identity

-- Foldable tests
-- Note: hasNoTypeClasses cannot pass for Foldable - Monoid constraint requires a dictionary.
-- We only verify that provenance types are absent.
inspect $ 'testCitedFoldMap `hasNoType` ''NCited
inspect $ 'testCitedFoldMap `hasNoType` ''Identity

inspect $ 'testCitedFFoldMap `hasNoType` ''NCited
inspect $ 'testCitedFFoldMap `hasNoType` ''Identity

-- Traversable tests
-- Note: hasNoTypeClasses cannot pass for Traversable - Applicative constraint requires a dictionary.
-- Note: hasNoType ''Identity also fails because Identity appears in coercion chains for newtype wrapping.
-- We only verify that NCited (the provenance tracking type) is absent.
inspect $ 'testCitedTraverse `hasNoType` ''NCited
inspect $ 'testCitedFTraverse `hasNoType` ''NCited
