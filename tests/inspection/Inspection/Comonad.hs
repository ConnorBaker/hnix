{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for Comonad instance specialization.
--
-- These tests verify that when @prov ~ 'False@:
--
-- 1. 'extract' has no type class dictionaries (Comonad, SBoolI eliminated)
-- 2. 'duplicate' has no type class dictionaries
-- 3. Both operations have no NCited or Identity types in generated Core
--
-- The Comonad instance is critical for value extraction in the evaluator.
module Inspection.Comonad
  ( -- * Cited Comonad tests
    testCitedExtract
  , testCitedDuplicate
    -- * CitedF Comonad tests
  , testCitedFComonadExtract
  , testCitedFDuplicate
  ) where

import           Relude

import           Control.Comonad                ( Comonad(extract, duplicate) )
import           Data.Functor.Identity          ()  -- for ''Identity in hasNoType
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Standard                   ( CitedF(..) )


-- * Cited Comonad tests

-- | Test Comonad extract for Cited when @prov ~ 'False@.
--
-- extract = extractCited, which should compile to the identity function
-- after newtype erasure.
testCitedExtract :: Cited 'False t f m a -> a
testCitedExtract = extract
{-# NOINLINE testCitedExtract #-}

-- | Test Comonad duplicate for Cited when @prov ~ 'False@.
--
-- duplicate should compile to wrapping the value in Identity (which is erased).
testCitedDuplicate :: Cited 'False t f m a -> Cited 'False t f m (Cited 'False t f m a)
testCitedDuplicate = duplicate
{-# NOINLINE testCitedDuplicate #-}


-- * CitedF Comonad tests

-- | Test Comonad extract for CitedF when @prov ~ 'False@.
testCitedFComonadExtract :: CitedF 'False m a -> a
testCitedFComonadExtract = extract
{-# NOINLINE testCitedFComonadExtract #-}

-- | Test Comonad duplicate for CitedF when @prov ~ 'False@.
testCitedFDuplicate :: CitedF 'False m a -> CitedF 'False m (CitedF 'False m a)
testCitedFDuplicate = duplicate
{-# NOINLINE testCitedFDuplicate #-}


-- * Inspection tests

-- Cited extract tests
inspect $ hasNoTypeClasses 'testCitedExtract
inspect $ 'testCitedExtract `hasNoType` ''NCited
inspect $ 'testCitedExtract `hasNoType` ''Identity
inspect $ 'testCitedExtract `hasNoType` ''Provenance

-- Cited duplicate tests
inspect $ hasNoTypeClasses 'testCitedDuplicate
inspect $ 'testCitedDuplicate `hasNoType` ''NCited
inspect $ 'testCitedDuplicate `hasNoType` ''Provenance

-- CitedF extract tests
inspect $ hasNoTypeClasses 'testCitedFComonadExtract
inspect $ 'testCitedFComonadExtract `hasNoType` ''NCited
inspect $ 'testCitedFComonadExtract `hasNoType` ''Identity
inspect $ 'testCitedFComonadExtract `hasNoType` ''Provenance

-- CitedF duplicate tests
inspect $ hasNoTypeClasses 'testCitedFDuplicate
inspect $ 'testCitedFDuplicate `hasNoType` ''NCited
inspect $ 'testCitedFDuplicate `hasNoType` ''Provenance
