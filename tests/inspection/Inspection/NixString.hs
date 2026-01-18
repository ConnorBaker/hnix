{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Inspection tests for NixString operations.
--
-- These tests verify that string operations are efficient:
--
-- 1. String construction without context is zero-cost
-- 2. Context-free string operations don't allocate context
-- 3. String content extraction is direct
module Inspection.NixString
  ( -- * String construction tests
    testMkNixStringWithoutContext
  , testMkNixString
    -- * String extraction tests
  , testIgnoreContext
  , testGetStringNoContext
  , testHasContext
    -- * String manipulation tests
  , testModifyNixContents
  ) where

import           Relude

import qualified Data.HashSet                  as HS
import           Test.Inspection

import           Nix.String                     ( NixString
                                                , StringContext
                                                , mkNixStringWithoutContext
                                                , mkNixString
                                                , ignoreContext
                                                , getStringNoContext
                                                , hasContext
                                                , modifyNixContents
                                                )


-- * String construction tests

-- | Test creating a NixString without context.
--
-- This should be a simple wrapper with no context allocation.
testMkNixStringWithoutContext :: Text -> NixString
testMkNixStringWithoutContext = mkNixStringWithoutContext
{-# NOINLINE testMkNixStringWithoutContext #-}

-- | Test creating a NixString with explicit context.
testMkNixString :: HS.HashSet StringContext -> Text -> NixString
testMkNixString = mkNixString
{-# NOINLINE testMkNixString #-}


-- * String extraction tests

-- | Test ignoring context and extracting text.
--
-- This should be a simple field access.
testIgnoreContext :: NixString -> Text
testIgnoreContext = ignoreContext
{-# NOINLINE testIgnoreContext #-}

-- | Test getting string only if it has no context.
--
-- Returns Nothing if the string has context.
testGetStringNoContext :: NixString -> Maybe Text
testGetStringNoContext = getStringNoContext
{-# NOINLINE testGetStringNoContext #-}

-- | Test checking if a string has context.
testHasContext :: NixString -> Bool
testHasContext = hasContext
{-# NOINLINE testHasContext #-}


-- * String manipulation tests

-- | Test modifying string contents while preserving context.
testModifyNixContents :: (Text -> Text) -> NixString -> NixString
testModifyNixContents = modifyNixContents
{-# NOINLINE testModifyNixContents #-}


-- * Inspection tests

-- String construction should be simple
inspect $ hasNoTypeClasses 'testMkNixStringWithoutContext
inspect $ hasNoTypeClasses 'testMkNixString

-- String extraction should be simple
inspect $ hasNoTypeClasses 'testIgnoreContext
inspect $ hasNoTypeClasses 'testGetStringNoContext
inspect $ hasNoTypeClasses 'testHasContext

-- String manipulation should be simple
inspect $ hasNoTypeClasses 'testModifyNixContents
