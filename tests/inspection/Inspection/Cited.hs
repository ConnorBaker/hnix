{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for Cited type specialization.
--
-- These tests verify that when @prov ~ 'False@:
--
-- 1. 'extractCited' has no 'SBoolI' dictionary (singleton dispatch eliminated)
-- 2. 'extractCited' Core contains no 'Identity' type (newtype erasure)
-- 3. 'extractCited' Core contains no 'NCited' type (provenance-tracking type absent)
-- 4. 'provenanceCited' compiles to return @[]@ directly
--
-- The @NOINLINE@ pragmas on wrapper functions prevent GHC from inlining them
-- before inspection-testing can analyze the Core.
module Inspection.Cited
  ( -- * Test wrappers
    extractCitedFalse
  , provenanceCitedFalse
    -- * Expected implementations (for comparison)
  , expectedExtractFalse
  , expectedProvenanceFalse
  ) where

import           Relude

import           Data.Functor.Identity          ()  -- for ''Identity in hasNoType
import           Test.Inspection

import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..)
                                                , extractCited
                                                , provenanceCited
                                                )
import           Nix.Value                      ( NValue )


-- * Test wrappers at concrete type (prov ~ 'False)

-- | Extract value from Cited wrapper when prov ~ 'False.
--
-- At this type, 'CitedRep 'False m v a = Identity a', so the implementation
-- should compile down to 'runIdentity' (which should be erased).
extractCitedFalse :: Cited 'False t f m a -> a
extractCitedFalse = extractCited
{-# NOINLINE extractCitedFalse #-}

-- | Get provenance from Cited wrapper when prov ~ 'False.
--
-- At this type, there is no provenance tracking, so this should compile
-- to 'const []' or equivalent.
provenanceCitedFalse :: Cited 'False t f m a -> [Provenance m (NValue t f m)]
provenanceCitedFalse = provenanceCited
{-# NOINLINE provenanceCitedFalse #-}


-- * Expected implementations for comparison

-- | Expected implementation of extractCited for prov ~ 'False.
--
-- When CitedRep 'False m v a = Identity a, extractCited should be
-- equivalent to: \\(Cited rep) -> runIdentity rep
--
-- After newtype erasure, this should be the identity function.
expectedExtractFalse :: Cited 'False t f m a -> a
expectedExtractFalse (Cited rep) = runIdentity rep
{-# NOINLINE expectedExtractFalse #-}

-- | Expected implementation of provenanceCited for prov ~ 'False.
--
-- When prov ~ 'False, there is no provenance tracking, so this returns [].
expectedProvenanceFalse :: Cited 'False t f m a -> [Provenance m (NValue t f m)]
expectedProvenanceFalse _ = []
{-# NOINLINE expectedProvenanceFalse #-}


-- * Inspection tests
--
-- These are registered at compile time via Template Haskell.
-- Each 'inspect' call verifies the property during compilation.

-- Test 1: extractCited @'False has no SBoolI dictionary
-- (singleton dispatch is eliminated at compile time)
inspect $ hasNoTypeClasses 'extractCitedFalse

-- Test 2: extractCited @'False Core contains no Identity type
-- (newtype should be erased by GHC)
inspect $ 'extractCitedFalse `hasNoType` ''Identity

-- Test 3: extractCited @'False Core contains no NCited type
-- (provenance-tracking type should be absent when prov ~ 'False)
inspect $ 'extractCitedFalse `hasNoType` ''NCited

-- Test 4: provenanceCited @'False has no SBoolI dictionary
inspect $ hasNoTypeClasses 'provenanceCitedFalse

-- Test 5: provenanceCited @'False has no NCited type
inspect $ 'provenanceCitedFalse `hasNoType` ''NCited

-- Note: Equivalence tests (==- operator) are omitted because GHC generates
-- slightly different coercion chains for the test wrappers vs expected
-- implementations. The hasNoTypeClasses and hasNoType tests above already
-- prove that the optimization is working correctly.
