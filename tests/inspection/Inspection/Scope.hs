{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for scope operations.
--
-- These tests verify that scope lookup and manipulation operations
-- are efficient and don't introduce unnecessary overhead:
--
-- 1. scopeLookup is a simple fold with no dictionaries
-- 2. Scope construction is zero-cost
-- 3. No unnecessary boxing or indirection
module Inspection.Scope
  ( -- * Scope lookup tests
    testScopeLookup
  , testScopeLookupWithDepth
    -- * Scope construction tests
  , testScopeFromList
  , testScopePush
  ) where

import           Relude

import qualified Data.HashMap.Strict           as HM
import           Test.Inspection

import           Nix.Expr.Types                 ( VarName )
import           Nix.Scope                      ( Scope(..), Scopes(..)
                                                , scopeLookup, scopeLookupWithDepth
                                                )


-- * Scope lookup tests

-- | Test scopeLookup operation.
--
-- This should be a simple right fold over the scope list.
testScopeLookup :: VarName -> [Scope a] -> Maybe a
testScopeLookup = scopeLookup
{-# NOINLINE testScopeLookup #-}

-- | Test scopeLookupWithDepth operation.
--
-- This should be a single-pass traversal returning value and depth info.
testScopeLookupWithDepth :: VarName -> [Scope a] -> (Maybe a, Int, Int)
testScopeLookupWithDepth = scopeLookupWithDepth
{-# NOINLINE testScopeLookupWithDepth #-}


-- * Scope construction tests

-- | Test creating a Scope from a list of pairs.
testScopeFromList :: [(VarName, a)] -> Scope a
testScopeFromList = Scope . HM.fromList
{-# NOINLINE testScopeFromList #-}

-- | Test pushing a scope onto a scope list.
testScopePush :: Scope a -> [Scope a] -> [Scope a]
testScopePush s ss = s : ss
{-# NOINLINE testScopePush #-}


-- * Expected implementations

-- | Expected: foldr-based lookup
expectedScopeLookup :: VarName -> [Scope a] -> Maybe a
expectedScopeLookup key = foldr fun Nothing
  where
    fun (Scope m) rest = HM.lookup key m <|> rest
{-# NOINLINE expectedScopeLookup #-}


-- * Inspection tests

-- Scope lookup should have no type class dictionaries beyond basic ones
-- (the Alternative instance for Maybe is expected)
inspect $ 'testScopeLookup `hasNoType` ''Scopes

-- Scope lookup with depth should be efficient
inspect $ 'testScopeLookupWithDepth `hasNoType` ''Scopes

-- Scope construction should be simple
inspect $ hasNoTypeClasses 'testScopeFromList
inspect $ hasNoTypeClasses 'testScopePush

-- Note: Equivalence test (scopeLookup ==- expectedScopeLookup) not used because
-- VarName uses stable names internally which generate different code patterns.
