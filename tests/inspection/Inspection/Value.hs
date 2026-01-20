{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for NValue and NValue' operations.
--
-- These tests verify that value construction and pattern matching
-- have no overhead when @prov ~ 'False@:
--
-- 1. Value constructors have no dictionaries
-- 2. Pattern matching extracts values without provenance overhead
-- 3. Free monad operations are zero-cost for the common case
module Inspection.Value
  ( -- * Value construction tests
    testMkNVConstant
  , testMkNVStr
  , testMkNVPath
  , testMkNVList
    -- * Value extraction tests
  , testExtractConstant
  , testExtractStr
    -- * Free monad tests
  , testFreePure
  , testFreeFree
  ) where

import           Relude

import           Control.Monad.Free             ( Free(..) )
import           Data.Functor.Identity          ()
import           Nix.List.Vector                ( NixList )
import           Test.Inspection

import           Nix.Atoms                      ( NAtom(..) )
import           Nix.Cited                      ( NCited, Provenance )
import           Nix.Cited.Basic                ( Cited(..) )
import           Nix.Standard                   ( CitedF(..), ThunkF(..), ValueF )
import           Nix.String                     ( NixString )
import           Nix.Utils                      ( Path )
import           Nix.Value                      ( NValue, NValue'(..), NValueF(..) )


-- * Value construction tests

-- | Test constructing an NVConstantF directly.
--
-- This should have no provenance overhead.
testMkNVConstant :: NAtom -> NValueF p m r
testMkNVConstant = NVConstantF
{-# NOINLINE testMkNVConstant #-}

-- | Test constructing an NVStrF directly.
testMkNVStr :: NixString -> NValueF p m r
testMkNVStr = NVStrF
{-# NOINLINE testMkNVStr #-}

-- | Test constructing an NVPathF directly.
testMkNVPath :: Path -> NValueF p m r
testMkNVPath = NVPathF
{-# NOINLINE testMkNVPath #-}

-- | Test constructing an NVListF directly.
testMkNVList :: NixList r -> NValueF p m r
testMkNVList = NVListF
{-# NOINLINE testMkNVList #-}


-- * Value extraction tests

-- | Test extracting a constant from NValueF.
testExtractConstant :: NValueF p m r -> Maybe NAtom
testExtractConstant (NVConstantF atom) = Just atom
testExtractConstant _                  = Nothing
{-# NOINLINE testExtractConstant #-}

-- | Test extracting a string from NValueF.
testExtractStr :: NValueF p m r -> Maybe NixString
testExtractStr (NVStrF ns) = Just ns
testExtractStr _           = Nothing
{-# NOINLINE testExtractStr #-}


-- * Free monad tests

-- | Test creating a Pure thunk.
testFreePure :: t -> NValue t f m
testFreePure = Pure
{-# NOINLINE testFreePure #-}

-- | Test wrapping in Free.
testFreeFree :: NValue' t f m (NValue t f m) -> NValue t f m
testFreeFree = Free
{-# NOINLINE testFreeFree #-}


-- * Inspection tests

-- Value construction - should have no type class dictionaries
inspect $ hasNoTypeClasses 'testMkNVConstant
inspect $ 'testMkNVConstant `hasNoType` ''NCited
inspect $ 'testMkNVConstant `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testMkNVStr
inspect $ 'testMkNVStr `hasNoType` ''NCited
inspect $ 'testMkNVStr `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testMkNVPath
inspect $ 'testMkNVPath `hasNoType` ''NCited
inspect $ 'testMkNVPath `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testMkNVList
inspect $ 'testMkNVList `hasNoType` ''NCited
inspect $ 'testMkNVList `hasNoType` ''Provenance

-- Value extraction - should have no type class dictionaries
inspect $ hasNoTypeClasses 'testExtractConstant
inspect $ 'testExtractConstant `hasNoType` ''NCited
inspect $ 'testExtractConstant `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testExtractStr
inspect $ 'testExtractStr `hasNoType` ''NCited
inspect $ 'testExtractStr `hasNoType` ''Provenance

-- Free monad operations - should have no type class dictionaries
inspect $ hasNoTypeClasses 'testFreePure
inspect $ 'testFreePure `hasNoType` ''NCited
inspect $ 'testFreePure `hasNoType` ''Provenance

inspect $ hasNoTypeClasses 'testFreeFree
inspect $ 'testFreeFree `hasNoType` ''NCited
inspect $ 'testFreeFree `hasNoType` ''Provenance
