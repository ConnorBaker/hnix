-- | Comprehensive operator tests for the Nix compiler.
--
-- This module tests operators that may not be fully covered by the main test suite,
-- including comparison operators, collection operators, string operators, and
-- boolean operators with edge cases.
module Compile.OperatorTests (tests) where

import Relude
import Test.Tasty

import Compile.TestCommon

-- | All operator tests
tests :: TestTree
tests = testGroup "Operator Tests"
  [ comparisonTests
  , collectionTests
  , stringTests
  , booleanTests
  ]

-- | Comparison operator tests
comparisonTests :: TestTree
comparisonTests = testGroup "Comparison Operators"
  [ testGroup "<= (less than or equal)"
      [ withSessionTest "less than or equal true" $ \s -> do
          result <- eval s "1 <= 2"
          assertBoolVal result True
      , withSessionTest "less than or equal (equal case)" $ \s -> do
          result <- eval s "2 <= 2"
          assertBoolVal result True
      ]
  , testGroup ">= (greater than or equal)"
      [ withSessionTest "greater than or equal true" $ \s -> do
          result <- eval s "2 >= 1"
          assertBoolVal result True
      , withSessionTest "greater than or equal (equal case)" $ \s -> do
          result <- eval s "2 >= 2"
          assertBoolVal result True
      ]
  , testGroup "String comparison"
      [ withSessionTest "string less than" $ \s -> do
          result <- eval s "\"a\" < \"b\""
          assertBoolVal result True
      , withSessionTest "string greater than" $ \s -> do
          result <- eval s "\"b\" > \"a\""
          assertBoolVal result True
      , withSessionTest "string prefix less than" $ \s -> do
          result <- eval s "\"ab\" < \"abc\""
          assertBoolVal result True
      , withSessionTest "string prefix greater than" $ \s -> do
          result <- eval s "\"abc\" > \"ab\""
          assertBoolVal result True
      , withSessionTest "string less than or equal (less)" $ \s -> do
          result <- eval s "\"a\" <= \"b\""
          assertBoolVal result True
      , withSessionTest "string less than or equal (equal)" $ \s -> do
          result <- eval s "\"abc\" <= \"abc\""
          assertBoolVal result True
      , withSessionTest "string greater than or equal (greater)" $ \s -> do
          result <- eval s "\"b\" >= \"a\""
          assertBoolVal result True
      , withSessionTest "string greater than or equal (equal)" $ \s -> do
          result <- eval s "\"xyz\" >= \"xyz\""
          assertBoolVal result True
      , withSessionTest "string comparison empty" $ \s -> do
          result <- eval s "\"\" < \"a\""
          assertBoolVal result True
      , withSessionTest "string comparison case sensitive" $ \s -> do
          -- In Nix, uppercase comes before lowercase (ASCII order)
          result <- eval s "\"A\" < \"a\""
          assertBoolVal result True
      , withSessionTest "string comparison equal" $ \s -> do
          result <- eval s "\"hello\" == \"hello\""
          assertBoolVal result True
      , withSessionTest "string comparison not equal" $ \s -> do
          result <- eval s "\"hello\" != \"world\""
          assertBoolVal result True
      ]
  , testGroup "Comparison edge cases"
      [ withSessionTest "comparison chained (left assoc)" $ \s -> do
          -- This should parse as (1 < 2) < true, which is a type error
          -- or parse as 1 < 2 resulting in true, then < 3 fails
          -- Let's test valid chained comparisons via logical ops instead
          result <- eval s "1 < 2 && 2 < 3"
          assertBoolVal result True
      , withSessionTest "comparison with negation" $ \s -> do
          result <- eval s "(-1) < 0"
          assertBoolVal result True
      , withSessionTest "comparison large numbers" $ \s -> do
          result <- eval s "9999999999 > 0"
          assertBoolVal result True
      ]
  ]

-- | Collection operator tests
collectionTests :: TestTree
collectionTests = testGroup "Collection Operators"
  [ testGroup "++ (list concatenation)"
      [ withSessionTest "list concat two lists" $ \s -> do
          result <- eval s "[1 2] ++ [3 4]"
          assertListLength result 4
      , withSessionTest "list concat first element" $ \s -> do
          result <- eval s "builtins.elemAt ([1 2] ++ [3 4]) 0"
          assertInt result 1
      , withSessionTest "list concat last element" $ \s -> do
          result <- eval s "builtins.elemAt ([1 2] ++ [3 4]) 3"
          assertInt result 4
      , withSessionTest "list concat empty left" $ \s -> do
          result <- eval s "[ ] ++ [1 2 3]"
          assertListLength result 3
      , withSessionTest "list concat empty right" $ \s -> do
          result <- eval s "[1 2 3] ++ [ ]"
          assertListLength result 3
      , withSessionTest "list concat both empty" $ \s -> do
          result <- eval s "[ ] ++ [ ]"
          assertListLength result 0
      , withSessionTest "list concat nested" $ \s -> do
          result <- eval s "[1] ++ [2] ++ [3]"
          assertListLength result 3
      , withSessionTest "list concat with mixed types" $ \s -> do
          result <- eval s "[1 \"a\"] ++ [true null]"
          assertListLength result 4
      , withSessionTest "list concat preserves order" $ \s -> do
          result <- eval s "builtins.elemAt ([10 20] ++ [30 40]) 2"
          assertInt result 30
      ]
  , testGroup "// (attrset update/merge)"
      [ withSessionTest "attrset merge disjoint" $ \s -> do
          result <- eval s "{ a = 1; } // { b = 2; }"
          assertAttrsSize result 2
      , withSessionTest "attrset merge overlapping (right wins)" $ \s -> do
          result <- eval s "({ a = 1; } // { a = 2; }).a"
          assertInt result 2
      , withSessionTest "attrset merge preserves non-overlapping" $ \s -> do
          result <- eval s "({ a = 1; b = 2; } // { b = 3; c = 4; }).a"
          assertInt result 1
      , withSessionTest "attrset merge empty left" $ \s -> do
          result <- eval s "{ } // { a = 1; }"
          assertAttrsSize result 1
      , withSessionTest "attrset merge empty right" $ \s -> do
          result <- eval s "{ a = 1; } // { }"
          assertAttrsSize result 1
      , withSessionTest "attrset merge both empty" $ \s -> do
          result <- eval s "{ } // { }"
          assertAttrsSize result 0
      , withSessionTest "attrset merge nested (shallow)" $ \s -> do
          -- // is a shallow merge, nested attrs are replaced entirely
          result <- eval s "({ a = { x = 1; y = 2; }; } // { a = { z = 3; }; }).a ? x"
          assertBoolVal result False
      , withSessionTest "attrset merge chain" $ \s -> do
          result <- eval s "({ a = 1; } // { b = 2; } // { c = 3; })"
          assertAttrsSize result 3
      , withSessionTest "attrset merge right precedence" $ \s -> do
          -- In a // b // c, c should win over b which wins over a
          result <- eval s "({ x = 1; } // { x = 2; } // { x = 3; }).x"
          assertInt result 3
      , withSessionTest "attrset merge access left key" $ \s -> do
          result <- eval s "({ left = 10; } // { right = 20; }).left"
          assertInt result 10
      , withSessionTest "attrset merge access right key" $ \s -> do
          result <- eval s "({ left = 10; } // { right = 20; }).right"
          assertInt result 20
      ]
  ]

-- | String operator tests
stringTests :: TestTree
stringTests = testGroup "String Operators"
  [ testGroup "String equality"
      [ withSessionTest "string equality true" $ \s -> do
          result <- eval s "\"hello\" == \"hello\""
          assertBoolVal result True
      , withSessionTest "string equality false" $ \s -> do
          result <- eval s "\"hello\" == \"world\""
          assertBoolVal result False
      , withSessionTest "string equality empty" $ \s -> do
          result <- eval s "\"\" == \"\""
          assertBoolVal result True
      , withSessionTest "string equality with spaces" $ \s -> do
          result <- eval s "\"a b\" == \"a b\""
          assertBoolVal result True
      , withSessionTest "string equality case sensitive" $ \s -> do
          result <- eval s "\"Hello\" == \"hello\""
          assertBoolVal result False
      ]
  , testGroup "String inequality"
      [ withSessionTest "string inequality true" $ \s -> do
          result <- eval s "\"hello\" != \"world\""
          assertBoolVal result True
      , withSessionTest "string inequality false" $ \s -> do
          result <- eval s "\"hello\" != \"hello\""
          assertBoolVal result False
      , withSessionTest "string inequality empty vs nonempty" $ \s -> do
          result <- eval s "\"\" != \"a\""
          assertBoolVal result True
      ]
  , testGroup "String concatenation with +"
      [ withSessionTest "string concat simple" $ \s -> do
          result <- eval s "\"hello\" + \" world\""
          assertStringVal result "hello world"
      , withSessionTest "string concat empty left" $ \s -> do
          result <- eval s "\"\" + \"hello\""
          assertStringVal result "hello"
      , withSessionTest "string concat empty right" $ \s -> do
          result <- eval s "\"hello\" + \"\""
          assertStringVal result "hello"
      , withSessionTest "string concat both empty" $ \s -> do
          result <- eval s "\"\" + \"\""
          assertStringVal result ""
      , withSessionTest "string concat chain" $ \s -> do
          result <- eval s "\"a\" + \"b\" + \"c\""
          assertStringVal result "abc"
      , withSessionTest "string concat with numbers coerced" $ \s -> do
          -- Nix coerces numbers to strings in concatenation
          result <- eval s "\"value: \" + toString 42"
          assertStringVal result "value: 42"
      ]
  ]

-- | Boolean operator tests with edge cases
booleanTests :: TestTree
booleanTests = testGroup "Boolean Operators"
  [ testGroup "&& (logical and) short-circuit"
      [ withSessionTest "and short-circuits on false" $ \s -> do
          -- If && doesn't short-circuit, this would evaluate 1/0 and error
          result <- eval s "false && (1/0 > 0)"
          assertBoolVal result False
      , withSessionTest "and evaluates right on true" $ \s -> do
          result <- eval s "true && true"
          assertBoolVal result True
      , withSessionTest "and false on right" $ \s -> do
          result <- eval s "true && false"
          assertBoolVal result False
      , withSessionTest "and chain all true" $ \s -> do
          result <- eval s "true && true && true"
          assertBoolVal result True
      , withSessionTest "and chain with false" $ \s -> do
          result <- eval s "true && false && true"
          assertBoolVal result False
      ]
  , testGroup "|| (logical or) short-circuit"
      [ withSessionTest "or short-circuits on true" $ \s -> do
          -- If || doesn't short-circuit, this would evaluate 1/0 and error
          result <- eval s "true || (1/0 > 0)"
          assertBoolVal result True
      , withSessionTest "or evaluates right on false" $ \s -> do
          result <- eval s "false || true"
          assertBoolVal result True
      , withSessionTest "or both false" $ \s -> do
          result <- eval s "false || false"
          assertBoolVal result False
      , withSessionTest "or chain all false" $ \s -> do
          result <- eval s "false || false || false"
          assertBoolVal result False
      , withSessionTest "or chain with true" $ \s -> do
          result <- eval s "false || true || false"
          assertBoolVal result True
      ]
  , testGroup "-> (implication)"
      [ withSessionTest "implication false -> false" $ \s -> do
          result <- eval s "false -> false"
          assertBoolVal result True
      , withSessionTest "implication false -> true" $ \s -> do
          result <- eval s "false -> true"
          assertBoolVal result True
      , withSessionTest "implication true -> false" $ \s -> do
          result <- eval s "true -> false"
          assertBoolVal result False
      , withSessionTest "implication true -> true" $ \s -> do
          result <- eval s "true -> true"
          assertBoolVal result True
      , withSessionTest "implication short-circuits on false premise" $ \s -> do
          -- false -> X should not evaluate X
          result <- eval s "false -> (1/0 > 0)"
          assertBoolVal result True
      , withSessionTest "implication evaluates consequent on true premise" $ \s -> do
          result <- eval s "true -> (1 == 1)"
          assertBoolVal result True
      , withSessionTest "implication chain" $ \s -> do
          -- a -> b -> c is a -> (b -> c) by right associativity
          result <- eval s "true -> true -> true"
          assertBoolVal result True
      , withSessionTest "implication chain with false" $ \s -> do
          -- true -> (false -> true) = true -> true = true
          result <- eval s "true -> false -> true"
          assertBoolVal result True
      ]
  , testGroup "! (logical not)"
      [ withSessionTest "not true" $ \s -> do
          result <- eval s "!true"
          assertBoolVal result False
      , withSessionTest "not false" $ \s -> do
          result <- eval s "!false"
          assertBoolVal result True
      , withSessionTest "double negation" $ \s -> do
          result <- eval s "!!true"
          assertBoolVal result True
      , withSessionTest "triple negation" $ \s -> do
          result <- eval s "!!!true"
          assertBoolVal result False
      , withSessionTest "not with expression" $ \s -> do
          result <- eval s "!(1 == 2)"
          assertBoolVal result True
      ]
  , testGroup "Boolean equality"
      [ withSessionTest "true equals true" $ \s -> do
          result <- eval s "true == true"
          assertBoolVal result True
      , withSessionTest "false equals false" $ \s -> do
          result <- eval s "false == false"
          assertBoolVal result True
      , withSessionTest "true not equals false" $ \s -> do
          result <- eval s "true != false"
          assertBoolVal result True
      , withSessionTest "bool equals int is false" $ \s -> do
          -- Nix returns false for comparing different types
          result <- eval s "true == 1"
          assertBoolVal result False
      ]
  , testGroup "Complex boolean expressions"
      [ withSessionTest "mixed and/or precedence" $ \s -> do
          -- && has higher precedence than ||
          -- false || true && true = false || (true && true) = false || true = true
          result <- eval s "false || true && true"
          assertBoolVal result True
      , withSessionTest "parentheses override precedence" $ \s -> do
          -- (false || true) && false = true && false = false
          result <- eval s "(false || true) && false"
          assertBoolVal result False
      , withSessionTest "not with and" $ \s -> do
          result <- eval s "!false && true"
          assertBoolVal result True
      , withSessionTest "not with or" $ \s -> do
          result <- eval s "!true || true"
          assertBoolVal result True
      , withSessionTest "comparison and logical" $ \s -> do
          result <- eval s "(1 < 2) && (3 > 2)"
          assertBoolVal result True
      , withSessionTest "implication with comparison" $ \s -> do
          result <- eval s "(1 < 2) -> (2 < 3)"
          assertBoolVal result True
      ]
  ]
