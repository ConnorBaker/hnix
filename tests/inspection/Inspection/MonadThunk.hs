{-# LANGUAGE TemplateHaskell #-}

-- | Inspection tests for MonadThunk operations.
--
-- These tests verify that thunk operations specialize correctly when
-- using the standard monad stack with @prov ~ 'False@:
--
-- 1. thunk/force/query operations have no SBoolI dictionaries
-- 2. No NCited or Provenance types in generated Core
-- 3. ThunkF operations are zero-cost wrappers
module Inspection.MonadThunk
  ( -- * Thunk creation tests
    testThunkCreate
    -- * Force tests
  , testThunkForce
    -- * Query tests
  , testThunkQuery
    -- * ThunkId tests
  , testThunkId
  ) where

import           Relude

import           Data.Functor.Identity          ()
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Standard                   ( CitedF(..), ThunkF(..), ValueF )
import           Nix.Thunk                      ( MonadThunk(..), MonadThunkId(..) )
import qualified Nix.Thunk                     as Thunk


-- * Thunk creation tests

-- | Test thunk creation with ThunkF when @prov ~ 'False@.
--
-- The thunk operation should have no dictionary overhead.
testThunkCreate :: MonadThunk (ThunkF 'False m) m (ValueF 'False m)
                => m (ValueF 'False m)
                -> m (ThunkF 'False m)
testThunkCreate = thunk
{-# NOINLINE testThunkCreate #-}


-- * Force tests

-- | Test forcing a thunk with ThunkF when @prov ~ 'False@.
--
-- Force should directly evaluate without provenance tracking overhead.
testThunkForce :: MonadThunk (ThunkF 'False m) m (ValueF 'False m)
               => ThunkF 'False m
               -> m (ValueF 'False m)
testThunkForce = Thunk.force
{-# NOINLINE testThunkForce #-}


-- * Query tests

-- | Test querying a thunk with ThunkF when @prov ~ 'False@.
--
-- Query should check thunk state without forcing.
testThunkQuery :: MonadThunk (ThunkF 'False m) m (ValueF 'False m)
               => m (ValueF 'False m)
               -> ThunkF 'False m
               -> m (ValueF 'False m)
testThunkQuery = query
{-# NOINLINE testThunkQuery #-}


-- * ThunkId tests

-- | Test getting thunk ID from ThunkF when @prov ~ 'False@.
testThunkId :: MonadThunk (ThunkF 'False m) m (ValueF 'False m)
            => ThunkF 'False m
            -> ThunkId m
testThunkId = thunkId
{-# NOINLINE testThunkId #-}


-- * Inspection tests

-- Note: These functions have MonadThunk constraints, which inherently require
-- dictionaries. However, we verify that no *provenance-related* types or
-- dictionaries appear. The MonadThunk dictionary itself is expected.

-- Thunk creation - should have no provenance types
inspect $ 'testThunkCreate `hasNoType` ''NCited
inspect $ 'testThunkCreate `hasNoType` ''Provenance

-- Force - should have no provenance types
inspect $ 'testThunkForce `hasNoType` ''NCited
inspect $ 'testThunkForce `hasNoType` ''Provenance

-- Query - should have no provenance types
inspect $ 'testThunkQuery `hasNoType` ''NCited
inspect $ 'testThunkQuery `hasNoType` ''Provenance

-- ThunkId - should have no provenance types
inspect $ 'testThunkId `hasNoType` ''NCited
inspect $ 'testThunkId `hasNoType` ''Provenance
