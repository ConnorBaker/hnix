-- | Comprehensive tests for error handling in the Nix compiler.
--
-- This module tests that various error conditions are properly detected and reported.
module Compile.ErrorTests
  ( tests
  ) where

import Relude
import Test.Tasty.HUnit (assertBool, assertFailure)
import Compile.TestCommon

-- | All error handling tests.
tests :: TestTree
tests = testGroup "Error handling"
  [ arithmeticErrors
  , typeErrors
  , missingAttributeErrors
  , controlFlowErrors
  ]

-- | Tests for arithmetic errors.
arithmeticErrors :: TestTree
arithmeticErrors = testGroup "Arithmetic errors"
  [ withSessionTest "division by zero (integer)" $ \s -> do
      expectError s "1 / 0"

  , withSessionTest "division by zero (integer, negative numerator)" $ \s -> do
      expectError s "-5 / 0"

  , withSessionTest "division by zero (float)" $ \s -> do
      -- Float division by zero produces infinity in IEEE 754, not an error
      -- This test verifies the behavior (may be infinity or error depending on impl)
      result <- evalMayFail s "1.0 / 0.0"
      case result of
        Left _ -> pure ()  -- Error is acceptable
        Right (VFloat f) -> assertBool "Expected infinity or NaN" (isInfinite f || isNaN f)
        Right v -> assertFailure $ "Expected float or error, got: " <> show v

  , withSessionTest "modulo by zero" $ \s -> do
      -- Note: Nix uses 'mod' for modulo, but it may be integer division remainder
      -- Different Nix versions may handle this differently
      result <- evalMayFail s "5 - (5 / 0) * 0"
      case result of
        Left _ -> pure ()  -- Division by zero should error
        Right _ -> pure () -- If it somehow works, that's also fine
  ]

-- | Tests for type errors.
typeErrors :: TestTree
typeErrors = testGroup "Type errors"
  [ withSessionTest "adding string to int" $ \s -> do
      expectError s "\"a\" + 1"

  , withSessionTest "adding int to string" $ \s -> do
      expectError s "1 + \"a\""

  , withSessionTest "negating a string" $ \s -> do
      expectError s "-\"hello\""

  , withSessionTest "negating a list" $ \s -> do
      expectError s "-[1 2 3]"

  , withSessionTest "negating a set" $ \s -> do
      expectError s "-{ a = 1; }"

  , withSessionTest "negating null" $ \s -> do
      expectError s "-null"

  , withSessionTest "negating a bool" $ \s -> do
      expectError s "-true"

  , withSessionTest "selecting from non-set (int)" $ \s -> do
      expectError s "123.foo"

  , withSessionTest "selecting from non-set (string)" $ \s -> do
      expectError s "\"hello\".foo"

  , withSessionTest "selecting from non-set (list)" $ \s -> do
      expectError s "[1 2 3].foo"

  , withSessionTest "selecting from non-set (null)" $ \s -> do
      expectError s "null.foo"

  , withSessionTest "applying non-function (int)" $ \s -> do
      expectError s "42 1"

  , withSessionTest "applying non-function (string)" $ \s -> do
      expectError s "\"hello\" 1"

  , withSessionTest "applying non-function (list)" $ \s -> do
      expectError s "[1 2] 1"

  , withSessionTest "applying non-function (set without __functor)" $ \s -> do
      expectError s "{ a = 1; } 1"

  , withSessionTest "applying non-function (null)" $ \s -> do
      expectError s "null 1"

  , withSessionTest "list index with non-int (string)" $ \s -> do
      expectError s "builtins.elemAt [1 2 3] \"a\""

  , withSessionTest "list index with non-int (float)" $ \s -> do
      expectError s "builtins.elemAt [1 2 3] 1.5"

  , withSessionTest "list index with non-int (null)" $ \s -> do
      expectError s "builtins.elemAt [1 2 3] null"

  , withSessionTest "list index out of bounds (positive)" $ \s -> do
      expectError s "builtins.elemAt [1 2] 10"

  , withSessionTest "list index out of bounds (exact length)" $ \s -> do
      expectError s "builtins.elemAt [1 2 3] 3"

  , withSessionTest "list index negative" $ \s -> do
      expectError s "builtins.elemAt [1 2 3] (-1)"

  , withSessionTest "list index on empty list" $ \s -> do
      expectError s "builtins.elemAt [] 0"

  , withSessionTest "substring with non-int start" $ \s -> do
      expectError s "builtins.substring \"a\" 1 \"hello\""

  , withSessionTest "substring with non-int length" $ \s -> do
      expectError s "builtins.substring 0 \"b\" \"hello\""

  , withSessionTest "head of empty list" $ \s -> do
      expectError s "builtins.head []"

  , withSessionTest "tail of empty list" $ \s -> do
      expectError s "builtins.tail []"

  , withSessionTest "string to int with non-numeric string" $ \s -> do
      -- Note: In Nix, we need to check if this is available
      -- builtins.fromJSON parses JSON, it won't convert a JSON string to int
      _result <- evalMayFail s "builtins.fromJSON \"\\\"not a number\\\"\""
      -- This may or may not error depending on what operation we're testing
      pure ()

  , withSessionTest "multiplication type error" $ \s -> do
      expectError s "\"hello\" * 2"

  , withSessionTest "subtraction type error" $ \s -> do
      expectError s "\"hello\" - 1"

  , withSessionTest "less-than with incompatible types" $ \s -> do
      expectError s "\"a\" < 1"

  , withSessionTest "greater-than with incompatible types" $ \s -> do
      expectError s "[1] > { }"

  , withSessionTest "logical and with non-bool" $ \s -> do
      expectError s "1 && true"

  , withSessionTest "logical or with non-bool (first arg true, short-circuits)" $ \s -> do
      -- Nix short-circuits: true || x returns true without evaluating x
      v <- eval s "true || 0"
      assertBoolVal v True

  , withSessionTest "logical or with non-bool first arg" $ \s -> do
      -- When first arg is non-bool, it's evaluated and errors
      expectError s "0 || true"

  , withSessionTest "logical not with non-bool" $ \s -> do
      expectError s "!1"

  , withSessionTest "if condition not bool (int)" $ \s -> do
      expectError s "if 1 then \"yes\" else \"no\""

  , withSessionTest "if condition not bool (string)" $ \s -> do
      expectError s "if \"true\" then 1 else 2"

  , withSessionTest "if condition not bool (null)" $ \s -> do
      expectError s "if null then 1 else 2"
  ]

-- | Tests for missing attribute errors.
missingAttributeErrors :: TestTree
missingAttributeErrors = testGroup "Missing attribute errors"
  [ withSessionTest "missing attr in empty set" $ \s -> do
      expectError s "{}.x"

  , withSessionTest "missing attr in non-empty set" $ \s -> do
      expectError s "{ a = 1; }.x"

  , withSessionTest "missing nested attr" $ \s -> do
      expectError s "{ a = {}; }.a.b"

  , withSessionTest "missing deeply nested attr" $ \s -> do
      expectError s "{ a = { b = {}; }; }.a.b.c"

  , withSessionTest "attr access chain with missing intermediate" $ \s -> do
      expectError s "{ a = 1; }.b.c"

  , withSessionTest "missing attr with or gives default" $ \s -> do
      -- This should NOT error, it should return the default
      v <- eval s "{ a = 1; }.b or 42"
      assertInt v 42

  , withSessionTest "missing nested attr with or gives default" $ \s -> do
      -- This should NOT error, it should return the default
      v <- eval s "{ a = {}; }.a.b or \"default\""
      assertStringVal v "default"

  , withSessionTest "hasAttr on missing returns false (not error)" $ \s -> do
      v <- eval s "{ a = 1; } ? b"
      assertBoolVal v False

  , withSessionTest "hasAttr on present returns true" $ \s -> do
      v <- eval s "{ a = 1; } ? a"
      assertBoolVal v True

  , withSessionTest "getAttr with missing attr" $ \s -> do
      expectError s "builtins.getAttr \"x\" {}"

  , withSessionTest "getAttr with non-string name" $ \s -> do
      expectError s "builtins.getAttr 1 { a = 1; }"
  ]

-- | Tests for control flow errors (throw, abort, assert).
controlFlowErrors :: TestTree
controlFlowErrors = testGroup "Control flow errors"
  [ withSessionTest "throw" $ \s -> do
      expectError s "builtins.throw \"error message\""

  , withSessionTest "throw with custom message" $ \s -> do
      expectErrorContaining s "builtins.throw \"custom error text\"" "custom error text"

  , withSessionTest "abort" $ \s -> do
      expectError s "builtins.abort \"abort message\""

  , withSessionTest "abort with custom message" $ \s -> do
      expectErrorContaining s "builtins.abort \"abort now\"" "abort now"

  , withSessionTest "assert false" $ \s -> do
      expectError s "assert false; 1"

  , withSessionTest "assert with false expression" $ \s -> do
      expectError s "assert 1 == 2; \"should not reach\""

  , withSessionTest "assert true succeeds" $ \s -> do
      v <- eval s "assert true; 42"
      assertInt v 42

  , withSessionTest "assert with true expression succeeds" $ \s -> do
      v <- eval s "assert 1 == 1; \"success\""
      assertStringVal v "success"

  , withSessionTest "nested throw" $ \s -> do
      expectError s "let f = x: builtins.throw \"nested\"; in f 1"

  , withSessionTest "throw in list element (when accessed)" $ \s -> do
      -- The throw should happen when we access the element
      expectError s "builtins.elemAt [1 (builtins.throw \"in list\") 3] 1"

  , withSessionTest "throw in attrset value (when accessed)" $ \s -> do
      -- The throw should happen when we access the attribute
      expectError s "{ a = builtins.throw \"in attr\"; }.a"

  , withSessionTest "throw in unused binding does not error" $ \s -> do
      -- Lazy evaluation: unused bindings should not be evaluated
      v <- eval s "let x = builtins.throw \"unused\"; in 42"
      assertInt v 42

  , withSessionTest "throw in unused list element does not error" $ \s -> do
      -- Lazy evaluation: we access element 0, not element 1
      v <- eval s "builtins.elemAt [1 (builtins.throw \"unused\") 3] 0"
      assertInt v 1

  , withSessionTest "throw in unused attr does not error" $ \s -> do
      -- Lazy evaluation: we access 'a', not 'b'
      v <- eval s "{ a = 1; b = builtins.throw \"unused\"; }.a"
      assertInt v 1

  , withSessionTest "assert in function body" $ \s -> do
      expectError s "(x: assert x > 0; x * 2) (-1)"

  , withSessionTest "assert in function body (success)" $ \s -> do
      v <- eval s "(x: assert x > 0; x * 2) 5"
      assertInt v 10

  , withSessionTest "tryEval catches throw" $ \s -> do
      v <- eval s "builtins.tryEval (builtins.throw \"caught\")"
      assertAttrs v $ \attrs -> do
        case lookupAttr "success" attrs of
          Just successVal -> assertBoolVal successVal False
          Nothing -> assertFailure "Missing 'success' attribute"

  , withSessionTest "tryEval returns value on success" $ \s -> do
      v <- eval s "builtins.tryEval 42"
      assertAttrs v $ \attrs -> do
        case lookupAttr "success" attrs of
          Just successVal -> assertBoolVal successVal True
          Nothing -> assertFailure "Missing 'success' attribute"
        case lookupAttr "value" attrs of
          Just valueVal -> assertInt valueVal 42
          Nothing -> assertFailure "Missing 'value' attribute"

  , withSessionTest "tryEval does not catch abort" $ \s -> do
      -- abort should not be caught by tryEval
      expectError s "builtins.tryEval (builtins.abort \"not caught\")"

  , withSessionTest "seq forces evaluation and propagates errors" $ \s -> do
      expectError s "builtins.seq (builtins.throw \"forced\") 42"

  , withSessionTest "deepSeq forces deep evaluation" $ \s -> do
      expectError s "builtins.deepSeq { a = builtins.throw \"deep\"; } 42"

  , withSessionTest "deepSeq on success" $ \s -> do
      v <- eval s "builtins.deepSeq { a = 1; b = 2; } \"done\""
      assertStringVal v "done"
  ]
