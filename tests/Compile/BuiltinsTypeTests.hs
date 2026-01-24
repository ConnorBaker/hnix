-- | Tests for type-related builtins in the Nix compiler.
--
-- This module tests type predicates (isNull, isInt, etc.), the typeOf builtin,
-- and type conversion functions (toString, toJSON, fromJSON).
module Compile.BuiltinsTypeTests (tests) where

import Relude
import Compile.TestCommon

-- | All type builtin tests
tests :: TestTree
tests = testGroup "Type Builtins"
  [ typePredicateTests
  , typeOfTests
  , typeConversionTests
  ]

-- | Tests for type predicate builtins (isNull, isInt, etc.)
typePredicateTests :: TestTree
typePredicateTests = testGroup "Type Predicates"
  [ testGroup "isNull"
      [ withSessionTest "isNull null" $ \s -> do
          result <- eval s "builtins.isNull null"
          assertBoolVal result True
      , withSessionTest "isNull int" $ \s -> do
          result <- eval s "builtins.isNull 42"
          assertBoolVal result False
      ]
  , testGroup "isInt"
      [ withSessionTest "isInt int" $ \s -> do
          result <- eval s "builtins.isInt 42"
          assertBoolVal result True
      , withSessionTest "isInt float" $ \s -> do
          result <- eval s "builtins.isInt 3.14"
          assertBoolVal result False
      ]
  , testGroup "isFloat"
      [ withSessionTest "isFloat float" $ \s -> do
          result <- eval s "builtins.isFloat 3.14"
          assertBoolVal result True
      , withSessionTest "isFloat int" $ \s -> do
          result <- eval s "builtins.isFloat 42"
          assertBoolVal result False
      ]
  , testGroup "isBool"
      [ withSessionTest "isBool true" $ \s -> do
          result <- eval s "builtins.isBool true"
          assertBoolVal result True
      , withSessionTest "isBool int" $ \s -> do
          result <- eval s "builtins.isBool 1"
          assertBoolVal result False
      ]
  , testGroup "isString"
      [ withSessionTest "isString string" $ \s -> do
          result <- eval s "builtins.isString \"hello\""
          assertBoolVal result True
      , withSessionTest "isString int" $ \s -> do
          result <- eval s "builtins.isString 42"
          assertBoolVal result False
      ]
  , testGroup "isList"
      [ withSessionTest "isList empty" $ \s -> do
          result <- eval s "builtins.isList []"
          assertBoolVal result True
      , withSessionTest "isList attrs" $ \s -> do
          result <- eval s "builtins.isList {}"
          assertBoolVal result False
      ]
  , testGroup "isAttrs"
      [ withSessionTest "isAttrs empty" $ \s -> do
          result <- eval s "builtins.isAttrs {}"
          assertBoolVal result True
      , withSessionTest "isAttrs list" $ \s -> do
          result <- eval s "builtins.isAttrs []"
          assertBoolVal result False
      ]
  , testGroup "isFunction"
      [ withSessionTest "isFunction lambda" $ \s -> do
          result <- eval s "builtins.isFunction (x: x)"
          assertBoolVal result True
      , withSessionTest "isFunction int" $ \s -> do
          result <- eval s "builtins.isFunction 42"
          assertBoolVal result False
      ]
  , testGroup "isPath"
      [ withSessionTest "isPath absolute" $ \s -> do
          result <- eval s "builtins.isPath /foo/bar"
          assertBoolVal result True
      , withSessionTest "isPath string" $ \s -> do
          result <- eval s "builtins.isPath \"/foo\""
          assertBoolVal result False
      ]
  ]

-- | Tests for the typeOf builtin
typeOfTests :: TestTree
typeOfTests = testGroup "typeOf"
  [ withSessionTest "typeOf int" $ \s -> do
      result <- eval s "builtins.typeOf 42"
      assertStringVal result "int"
  , withSessionTest "typeOf float" $ \s -> do
      result <- eval s "builtins.typeOf 3.14"
      assertStringVal result "float"
  , withSessionTest "typeOf bool true" $ \s -> do
      result <- eval s "builtins.typeOf true"
      assertStringVal result "bool"
  , withSessionTest "typeOf null" $ \s -> do
      result <- eval s "builtins.typeOf null"
      assertStringVal result "null"
  , withSessionTest "typeOf string" $ \s -> do
      result <- eval s "builtins.typeOf \"hello\""
      assertStringVal result "string"
  , withSessionTest "typeOf list" $ \s -> do
      result <- eval s "builtins.typeOf [1 2 3]"
      assertStringVal result "list"
  , withSessionTest "typeOf set" $ \s -> do
      result <- eval s "builtins.typeOf { a = 1; }"
      assertStringVal result "set"
  , withSessionTest "typeOf lambda" $ \s -> do
      result <- eval s "builtins.typeOf (x: x)"
      assertStringVal result "lambda"
  , withSessionTest "typeOf path" $ \s -> do
      result <- eval s "builtins.typeOf /foo"
      assertStringVal result "path"
  ]

-- | Tests for type conversion builtins (toString, toJSON, fromJSON)
typeConversionTests :: TestTree
typeConversionTests = testGroup "Type Conversions"
  [ testGroup "toString"
      [ withSessionTest "toString int" $ \s -> do
          result <- eval s "builtins.toString 42"
          assertStringVal result "42"
      , withSessionTest "toString negative int" $ \s -> do
          result <- eval s "builtins.toString (-5)"
          assertStringVal result "-5"
      , withSessionTest "toString float" $ \s -> do
          result <- eval s "builtins.toString 3.14"
          -- Note: Nix uses specific float formatting
          assertIsString result
      , withSessionTest "toString float integer" $ \s -> do
          result <- eval s "builtins.toString 3.0"
          assertIsString result
      , withSessionTest "toString path" $ \s -> do
          result <- eval s "builtins.toString /foo/bar"
          assertStringVal result "/foo/bar"
      , withSessionTest "toString bool true" $ \s -> do
          result <- eval s "builtins.toString true"
          assertStringVal result "1"
      , withSessionTest "toString bool false" $ \s -> do
          result <- eval s "builtins.toString false"
          assertStringVal result ""
      , withSessionTest "toString null" $ \s -> do
          result <- eval s "builtins.toString null"
          assertStringVal result ""
      , withSessionTest "toString string passthrough" $ \s -> do
          result <- eval s "builtins.toString \"hello\""
          assertStringVal result "hello"
      , withSessionTest "toString list of strings" $ \s -> do
          result <- eval s "builtins.toString [\"a\" \"b\" \"c\"]"
          assertStringVal result "a b c"
      , withSessionTest "toString list of ints" $ \s -> do
          result <- eval s "builtins.toString [1 2 3]"
          assertStringVal result "1 2 3"
      , withSessionTest "toString empty list" $ \s -> do
          result <- eval s "builtins.toString []"
          assertStringVal result ""
      , withSessionTest "toString set with outPath" $ \s -> do
          result <- eval s "builtins.toString { outPath = \"/nix/store/xxx\"; }"
          assertStringVal result "/nix/store/xxx"
      , withSessionTest "toString set with __toString" $ \s -> do
          result <- eval s "builtins.toString { __toString = self: \"custom\"; }"
          assertStringVal result "custom"
      ]
  , testGroup "toJSON"
      [ withSessionTest "toJSON int" $ \s -> do
          result <- eval s "builtins.toJSON 42"
          assertStringVal result "42"
      , withSessionTest "toJSON float" $ \s -> do
          result <- eval s "builtins.toJSON 3.14"
          -- Note: Nix uses specific float formatting for JSON
          assertIsString result
      , withSessionTest "toJSON bool true" $ \s -> do
          result <- eval s "builtins.toJSON true"
          assertStringVal result "true"
      , withSessionTest "toJSON bool false" $ \s -> do
          result <- eval s "builtins.toJSON false"
          assertStringVal result "false"
      , withSessionTest "toJSON null" $ \s -> do
          result <- eval s "builtins.toJSON null"
          assertStringVal result "null"
      , withSessionTest "toJSON string" $ \s -> do
          result <- eval s "builtins.toJSON \"hello\""
          assertStringVal result "\"hello\""
      , withSessionTest "toJSON string with quotes" $ \s -> do
          result <- eval s "builtins.toJSON \"say \\\"hi\\\"\""
          -- Should produce: "say \"hi\""
          assertIsString result
      , withSessionTest "toJSON empty list" $ \s -> do
          result <- eval s "builtins.toJSON []"
          assertStringVal result "[]"
      , withSessionTest "toJSON list of ints" $ \s -> do
          result <- eval s "builtins.toJSON [1 2 3]"
          assertStringVal result "[1,2,3]"
      , withSessionTest "toJSON list of mixed" $ \s -> do
          result <- eval s "builtins.toJSON [1 \"a\" true]"
          assertStringVal result "[1,\"a\",true]"
      , withSessionTest "toJSON empty object" $ \s -> do
          result <- eval s "builtins.toJSON {}"
          assertStringVal result "{}"
      , withSessionTest "toJSON simple object" $ \s -> do
          result <- eval s "builtins.toJSON { a = 1; }"
          assertStringVal result "{\"a\":1}"
      , withSessionTest "toJSON nested object" $ \s -> do
          result <- eval s "builtins.toJSON { a = { b = 1; }; }"
          assertStringVal result "{\"a\":{\"b\":1}}"
      , withSessionTest "toJSON object with list" $ \s -> do
          result <- eval s "builtins.toJSON { a = [1 2]; }"
          assertStringVal result "{\"a\":[1,2]}"
      ]
  , testGroup "fromJSON"
      [ withSessionTest "fromJSON int" $ \s -> do
          result <- eval s "builtins.fromJSON \"42\""
          assertInt result 42
      , withSessionTest "fromJSON negative int" $ \s -> do
          result <- eval s "builtins.fromJSON \"-5\""
          assertInt result (-5)
      , withSessionTest "fromJSON float" $ \s -> do
          result <- eval s "builtins.fromJSON \"3.14\""
          assertFloatApprox result 3.14 0.001
      , withSessionTest "fromJSON bool true" $ \s -> do
          result <- eval s "builtins.fromJSON \"true\""
          assertBoolVal result True
      , withSessionTest "fromJSON bool false" $ \s -> do
          result <- eval s "builtins.fromJSON \"false\""
          assertBoolVal result False
      , withSessionTest "fromJSON null" $ \s -> do
          result <- eval s "builtins.fromJSON \"null\""
          assertNull result
      , withSessionTest "fromJSON string" $ \s -> do
          result <- eval s "builtins.fromJSON \"\\\"hello\\\"\""
          assertStringVal result "hello"
      , withSessionTest "fromJSON empty array" $ \s -> do
          result <- eval s "builtins.fromJSON \"[]\""
          assertListLength result 0
      , withSessionTest "fromJSON array of ints" $ \s -> do
          result <- eval s "builtins.fromJSON \"[1, 2, 3]\""
          assertListLength result 3
      , withSessionTest "fromJSON array element access" $ \s -> do
          result <- eval s "builtins.elemAt (builtins.fromJSON \"[10, 20, 30]\") 1"
          assertInt result 20
      , withSessionTest "fromJSON empty object" $ \s -> do
          result <- eval s "builtins.fromJSON \"{}\""
          assertAttrsSize result 0
      , withSessionTest "fromJSON simple object" $ \s -> do
          result <- eval s "(builtins.fromJSON \"{\\\"a\\\": 1}\").a"
          assertInt result 1
      , withSessionTest "fromJSON nested object" $ \s -> do
          result <- eval s "(builtins.fromJSON \"{\\\"a\\\": {\\\"b\\\": 42}}\").a.b"
          assertInt result 42
      , withSessionTest "fromJSON object with array" $ \s -> do
          result <- eval s "builtins.length (builtins.fromJSON \"{\\\"arr\\\": [1,2,3]}\").arr"
          assertInt result 3
      , withSessionTest "fromJSON invalid syntax error" $ \s -> do
          expectError s "builtins.fromJSON \"invalid\""
      , withSessionTest "fromJSON unclosed brace error" $ \s -> do
          expectError s "builtins.fromJSON \"{\""
      ]
  ]
