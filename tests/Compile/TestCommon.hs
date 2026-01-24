-- | Shared test utilities for the Nix compiler tests.
--
-- This module provides common helper functions used by all compiler test modules.
-- It wraps the NixSession management and provides assertion helpers for different
-- value types.
module Compile.TestCommon
  ( -- * Session management
    withSessionTest
  , eval
  , evalMayFail
  , expectError
  , expectErrorContaining
    -- * Assertions
  , assertInt
  , assertFloat
  , assertFloatApprox
  , assertBoolVal
  , assertNull
  , assertStringVal
  , assertStringContains
  , assertIsString
  , assertListLength
  , assertList
  , assertAttrsSize
  , assertAttrs
  , assertIsPath
  , assertPath
  , assertIsFunction
    -- * Re-exports for convenience
  , TestTree
  , testGroup
  , testCase
  , assertFailure
  , assertBool
  , (@?=)
  , Assertion
  , NixSession
  , NixValue(..)
  , NixAttrs(..)
  , lookupAttr
  , attrsSize
  ) where

import Relude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Exception (SomeException, try, throwIO)
import Data.List (isInfixOf)

import Nix.Compile.Driver (NixSession, initSession, evalNixText, NixCompileError(..))
import Nix.Compile.Value

-- | Helper to run a test that needs a NixSession.
-- Returns a test that skips if session initialization fails (e.g., in CI without runtime).
withSessionTest :: String -> (NixSession -> IO ()) -> TestTree
withSessionTest name test = testCase name $ do
  eSession <- initSession
  case eSession of
    Left (RuntimeLoadError _) ->
      -- Skip test if runtime not available (common in CI)
      assertFailure "Skipped: hnix-compile-runtime not available"
    Left err ->
      assertFailure $ "Session init failed: " <> show err
    Right session ->
      test session

-- | Evaluate Nix text and return result, failing the test on error.
eval :: NixSession -> Text -> IO NixValue
eval session text = do
  result <- evalNixText session text
  case result of
    Left err -> assertFailure ("Eval failed: " <> show err) >> error "unreachable"
    Right val -> pure val

-- | Evaluate Nix text, returning Left on error instead of failing the test
evalMayFail :: NixSession -> Text -> IO (Either SomeException NixValue)
evalMayFail session text = do
  result <- try @SomeException $ do
    r <- evalNixText session text
    case r of
      Left err -> throwIO err
      Right val -> pure val
  pure result

-- | Helper to expect an error from evaluation
expectError :: NixSession -> Text -> Assertion
expectError session text = do
  result <- evalMayFail session text
  case result of
    Left _ -> pure ()
    Right v -> assertFailure $ "Expected error, got: " <> show v

-- | Helper to expect an error containing specific text
expectErrorContaining :: NixSession -> Text -> Text -> Assertion
expectErrorContaining session expr expectedSubstr = do
  result <- evalMayFail session expr
  case result of
    Left exc -> do
      let errText = show exc
      assertBool ("Expected error containing '" <> toString expectedSubstr <> "', got: " <> errText)
                 (toString expectedSubstr `isInfixOf` errText)
    Right v -> assertFailure $ "Expected error, got: " <> show v

-- | Helper: assert the value is an integer with given value
assertInt :: NixValue -> Int64 -> Assertion
assertInt (VInt n) expected = n @?= expected
assertInt v _ = assertFailure $ "Expected int, got: " <> show v

-- | Helper: assert the value is a float with given value
assertFloat :: NixValue -> Double -> Assertion
assertFloat (VFloat n) expected = n @?= expected
assertFloat v _ = assertFailure $ "Expected float, got: " <> show v

-- | Helper: assert the value is a float approximately equal to expected
assertFloatApprox :: NixValue -> Double -> Double -> Assertion
assertFloatApprox (VFloat n) expected epsilon =
  assertBool ("Expected float ~" <> show expected <> ", got: " <> show n)
             (abs (n - expected) < epsilon)
assertFloatApprox v _ _ = assertFailure $ "Expected float, got: " <> show v

-- | Helper: assert the value is a bool with given value
assertBoolVal :: NixValue -> Bool -> Assertion
assertBoolVal (VBool b) expected = b @?= expected
assertBoolVal v _ = assertFailure $ "Expected bool, got: " <> show v

-- | Helper: assert the value is null
assertNull :: NixValue -> Assertion
assertNull VNull = pure ()
assertNull v = assertFailure $ "Expected null, got: " <> show v

-- | Helper: assert the value is a string with given text
assertStringVal :: NixValue -> Text -> Assertion
assertStringVal (VString t _) expected = t @?= expected
assertStringVal v _ = assertFailure $ "Expected string, got: " <> show v

-- | Helper: assert the value is a string containing given text
assertStringContains :: NixValue -> Text -> Assertion
assertStringContains (VString t _) substr =
  assertBool ("Expected string containing " <> show substr <> ", got: " <> show t)
             (substr `T.isInfixOf` t)
assertStringContains v _ = assertFailure $ "Expected string, got: " <> show v

-- | Helper: assert the value is a string (any string)
assertIsString :: NixValue -> Assertion
assertIsString (VString _ _) = pure ()
assertIsString v = assertFailure $ "Expected string, got: " <> show v

-- | Helper: assert the value is an attrset with given size
assertAttrsSize :: NixValue -> Int -> Assertion
assertAttrsSize (VAttrs as) expected = attrsSize as @?= expected
assertAttrsSize v _ = assertFailure $ "Expected attrset, got: " <> show v

-- | Helper: assert the value is an attrset and run assertions on it
assertAttrs :: NixValue -> (NixAttrs -> Assertion) -> Assertion
assertAttrs (VAttrs as) f = f as
assertAttrs v _ = assertFailure $ "Expected attrset, got: " <> show v

-- | Helper: assert the value is a list with given length
assertListLength :: NixValue -> Int -> Assertion
assertListLength (VList v) expected = V.length v @?= expected
assertListLength v _ = assertFailure $ "Expected list, got: " <> show v

-- | Helper: assert the value is a list and check each element
assertList :: NixValue -> [NixValue -> Assertion] -> Assertion
assertList (VList v) assertions = do
  V.length v @?= length assertions
  zipWithM_ (\i f -> f (v V.! i)) [0..] assertions
assertList v _ = assertFailure $ "Expected list, got: " <> show v

-- | Helper: assert the value is a path
assertIsPath :: NixValue -> Assertion
assertIsPath (VPath _) = pure ()
assertIsPath v = assertFailure $ "Expected path, got: " <> show v

-- | Helper: assert the value is a path with specific text
assertPath :: NixValue -> Text -> Assertion
assertPath (VPath p) expected = toText p @?= expected
assertPath v _ = assertFailure $ "Expected path, got: " <> show v

-- | Helper: assert the value is a function (closure or builtin)
assertIsFunction :: NixValue -> Assertion
assertIsFunction (VClosure _ _) = pure ()
assertIsFunction (VBuiltin _ _) = pure ()
assertIsFunction v = assertFailure $ "Expected function, got: " <> show v
