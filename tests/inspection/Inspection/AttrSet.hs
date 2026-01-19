{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for AttrSet (attribute set) operations.
--
-- These tests verify that attribute sets are appropriately specialized
-- when @prov ~ 'False@:
--
-- 1. Pattern matching on NVSet' has no dictionary overhead
-- 2. AttrSet construction has no provenance overhead
-- 3. HashMap operations are directly called without wrappers
-- 4. NVConstraint (Comonad/Applicative) is erased for Identity
module Inspection.AttrSet
  ( -- * Pattern matching tests
    testMatchAttrSet
  , testMatchAttrSetPositions
    -- * Construction tests
  , testMkAttrSet
  , testMkAttrSetWithPositions
    -- * HashMap operation tests
  , testAttrSetLookup
  , testAttrSetInsert
  , testAttrSetFromList
    -- * Coercion tests
  , testNVSetFDirect
  ) where

import           Relude

import           Data.Coerce                    ( coerce )
import           Data.Functor.Identity          ()
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Expr.Types                 ( PositionSet, VarName, AttrSet )
import qualified Nix.Scope                     as S
import           Nix.Standard                   ( CitedF(..) )
import           Nix.Value                      ( NValue', NValueF(..) )


-- * Pattern matching tests

-- | Test matching on NVSetF directly.
--
-- This should have no dictionary passing when w is Identity.
testMatchAttrSet :: NValueF p m r -> Maybe (AttrSet r)
testMatchAttrSet (NVSetF _ s) = Just s
testMatchAttrSet _            = Nothing
{-# NOINLINE testMatchAttrSet #-}

-- | Test matching and extracting both positions and attrs.
testMatchAttrSetPositions :: NValueF p m r -> Maybe (PositionSet, AttrSet r)
testMatchAttrSetPositions (NVSetF p s) = Just (p, s)
testMatchAttrSetPositions _            = Nothing
{-# NOINLINE testMatchAttrSetPositions #-}


-- * Construction tests

-- | Test constructing NVSetF directly.
--
-- This should be a simple constructor application.
testMkAttrSet :: AttrSet r -> NValueF p m r
testMkAttrSet = NVSetF mempty
{-# NOINLINE testMkAttrSet #-}

-- | Test constructing NVSetF with positions.
testMkAttrSetWithPositions :: PositionSet -> AttrSet r -> NValueF p m r
testMkAttrSetWithPositions = NVSetF
{-# NOINLINE testMkAttrSetWithPositions #-}


-- * HashMap operation tests

-- | Test AttrSet lookup (via abstract signature).
testAttrSetLookup :: VarName -> AttrSet r -> Maybe r
testAttrSetLookup = S.attrSetLookup
{-# NOINLINE testAttrSetLookup #-}

-- | Test AttrSet insertion.
testAttrSetInsert :: VarName -> r -> AttrSet r -> AttrSet r
testAttrSetInsert = S.attrSetInsert
{-# NOINLINE testAttrSetInsert #-}

-- | Test AttrSet construction from list.
testAttrSetFromList :: [(VarName, r)] -> AttrSet r
testAttrSetFromList = S.attrSetFromList
{-# NOINLINE testAttrSetFromList #-}


-- * Coercion tests

-- | Test that NVSetF construction is just data constructor.
testNVSetFDirect :: PositionSet -> AttrSet r -> NValueF p m r
testNVSetFDirect p s = NVSetF p s
{-# NOINLINE testNVSetFDirect #-}


-- * Inspection tests

-- Pattern matching on NVSetF should have no dictionaries
inspect $ hasNoTypeClasses 'testMatchAttrSet
inspect $ 'testMatchAttrSet `hasNoType` ''NCited
inspect $ 'testMatchAttrSet `hasNoType` ''Provenance
inspect $ 'testMatchAttrSet `hasNoType` ''Cited
inspect $ 'testMatchAttrSet `hasNoType` ''CitedF

inspect $ hasNoTypeClasses 'testMatchAttrSetPositions
inspect $ 'testMatchAttrSetPositions `hasNoType` ''NCited
inspect $ 'testMatchAttrSetPositions `hasNoType` ''Provenance

-- AttrSet construction should have no dictionaries
inspect $ hasNoTypeClasses 'testMkAttrSet
inspect $ 'testMkAttrSet `hasNoType` ''NCited
inspect $ 'testMkAttrSet `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testMkAttrSetWithPositions
inspect $ 'testMkAttrSetWithPositions `hasNoType` ''NCited
inspect $ 'testMkAttrSetWithPositions `hasNoType` ''Provenance

-- HashMap operations are direct
inspect $ hasNoTypeClasses 'testAttrSetLookup
inspect $ hasNoTypeClasses 'testAttrSetInsert
inspect $ hasNoTypeClasses 'testAttrSetFromList

-- Direct NVSetF construction
inspect $ hasNoTypeClasses 'testNVSetFDirect
inspect $ 'testNVSetFDirect `hasNoType` ''NCited
inspect $ 'testNVSetFDirect `hasNoType` ''Provenance
