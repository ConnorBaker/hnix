{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for data structure operations re-exported via Nix.Scope.
--
-- These tests verify that re-exported list and attrset operations are properly
-- specialized with no typeclass dictionary overhead. This ensures that builtins
-- using these operations get monomorphized code.
--
-- 1. List operations (nixListLength, nixListFromList, etc.) have no dictionaries
-- 2. AttrSet operations (attrSetLookup, etc.) have no dictionaries
-- 3. Operations are direct calls, not wrapped
--
-- Note: We test via Nix.Scope re-exports because that's what's exposed by the
-- main library. These re-exports ultimately call through Nix.Core.AttrSet and
-- Nix.Core.List which use the Backpack signatures.
module Inspection.Protocol
  ( -- * List operation tests
    testNixListLength
  , testNixListFromList
  , testNixListToList
  , testNixListNull
  , testNixListIndex
  , testNixListEmpty
    -- * AttrSet operation tests
  , testAttrSetLookup
  , testAttrSetInsert
  , testAttrSetFromList
  , testAttrSetToList
  , testAttrSetKeys
  ) where

import           Relude

import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Expr.Types                 ( VarName, AttrSet )
import           Nix.Core.List                  ( NixList )
import           Nix.Scope
    ( nixListLength, nixListFromList, nixListToList, nixListNull, nixListIndex, nixListEmpty
    , attrSetLookup, attrSetInsert, attrSetFromList, attrSetToList, attrSetKeys
    )


-- * List operation tests (via Nix.Scope re-exports)

-- | Test nixListLength.
testNixListLength :: NixList a -> Int
testNixListLength = nixListLength
{-# NOINLINE testNixListLength #-}

-- | Test nixListFromList.
testNixListFromList :: [a] -> NixList a
testNixListFromList = nixListFromList
{-# NOINLINE testNixListFromList #-}

-- | Test nixListToList.
testNixListToList :: NixList a -> [a]
testNixListToList = nixListToList
{-# NOINLINE testNixListToList #-}

-- | Test nixListNull.
testNixListNull :: NixList a -> Bool
testNixListNull = nixListNull
{-# NOINLINE testNixListNull #-}

-- | Test nixListIndex (elemAt).
testNixListIndex :: NixList a -> Int -> Maybe a
testNixListIndex = nixListIndex
{-# NOINLINE testNixListIndex #-}

-- | Test nixListEmpty.
testNixListEmpty :: NixList a
testNixListEmpty = nixListEmpty
{-# NOINLINE testNixListEmpty #-}


-- * AttrSet operation tests (via Nix.Scope re-exports)

-- | Test attrSetLookup.
testAttrSetLookup :: VarName -> AttrSet a -> Maybe a
testAttrSetLookup = attrSetLookup
{-# NOINLINE testAttrSetLookup #-}

-- | Test attrSetInsert.
testAttrSetInsert :: VarName -> a -> AttrSet a -> AttrSet a
testAttrSetInsert = attrSetInsert
{-# NOINLINE testAttrSetInsert #-}

-- | Test attrSetFromList.
testAttrSetFromList :: [(VarName, a)] -> AttrSet a
testAttrSetFromList = attrSetFromList
{-# NOINLINE testAttrSetFromList #-}

-- | Test attrSetToList.
testAttrSetToList :: AttrSet a -> [(VarName, a)]
testAttrSetToList = attrSetToList
{-# NOINLINE testAttrSetToList #-}

-- | Test attrSetKeys.
testAttrSetKeys :: AttrSet a -> [VarName]
testAttrSetKeys = attrSetKeys
{-# NOINLINE testAttrSetKeys #-}


-- * Inspection tests

-- List operations should have no type class dictionaries
inspect $ hasNoTypeClasses 'testNixListLength
inspect $ hasNoTypeClasses 'testNixListFromList
inspect $ hasNoTypeClasses 'testNixListToList
inspect $ hasNoTypeClasses 'testNixListNull
inspect $ hasNoTypeClasses 'testNixListIndex
inspect $ hasNoTypeClasses 'testNixListEmpty

-- AttrSet operations should have no type class dictionaries
inspect $ hasNoTypeClasses 'testAttrSetLookup
inspect $ hasNoTypeClasses 'testAttrSetInsert
inspect $ hasNoTypeClasses 'testAttrSetFromList
inspect $ hasNoTypeClasses 'testAttrSetToList
inspect $ hasNoTypeClasses 'testAttrSetKeys

-- Verify no provenance wrappers appear in list operations
inspect $ 'testNixListLength `hasNoType` ''NCited
inspect $ 'testNixListLength `hasNoType` ''Provenance
inspect $ 'testNixListLength `hasNoType` ''Cited

-- Verify no provenance wrappers appear in attrset operations
inspect $ 'testAttrSetLookup `hasNoType` ''NCited
inspect $ 'testAttrSetLookup `hasNoType` ''Provenance
inspect $ 'testAttrSetLookup `hasNoType` ''Cited
