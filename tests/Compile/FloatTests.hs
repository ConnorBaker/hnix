-- | Comprehensive tests for float operations in the Nix compiler.
--
-- This module tests float arithmetic, mixed int/float operations,
-- float comparison, and edge cases including scientific notation.
module Compile.FloatTests (tests) where

import Relude
import Compile.TestCommon

-- | All float operation tests
tests :: TestTree
tests = testGroup "Float Operations"
  [ floatArithmeticTests
  , mixedIntFloatTests
  , floatComparisonTests
  , floatEdgeCaseTests
  ]

-- | Tests for pure float arithmetic operations
floatArithmeticTests :: TestTree
floatArithmeticTests = testGroup "Float arithmetic"
  [ withSessionTest "float addition" $ \s -> do
      result <- eval s "1.5 + 2.5"
      assertFloat result 4.0

  , withSessionTest "float addition with different scales" $ \s -> do
      result <- eval s "0.1 + 0.2"
      -- Note: floating point arithmetic, so use approximate comparison
      assertFloatApprox result 0.3 0.0001

  , withSessionTest "float subtraction" $ \s -> do
      result <- eval s "5.5 - 2.3"
      assertFloatApprox result 3.2 0.0001

  , withSessionTest "float subtraction negative result" $ \s -> do
      result <- eval s "1.5 - 3.5"
      assertFloat result (-2.0)

  , withSessionTest "float multiplication" $ \s -> do
      result <- eval s "2.5 * 4.0"
      assertFloat result 10.0

  , withSessionTest "float multiplication with small numbers" $ \s -> do
      result <- eval s "0.5 * 0.5"
      assertFloat result 0.25

  , withSessionTest "float division" $ \s -> do
      result <- eval s "10.0 / 4.0"
      assertFloat result 2.5

  , withSessionTest "float division with remainder" $ \s -> do
      result <- eval s "7.5 / 2.0"
      assertFloat result 3.75

  , withSessionTest "float negation" $ \s -> do
      result <- eval s "-3.14"
      assertFloat result (-3.14)

  , withSessionTest "float double negation" $ \s -> do
      result <- eval s "- -2.5"
      assertFloat result 2.5

  , withSessionTest "float negation of expression" $ \s -> do
      result <- eval s "-(1.5 + 2.5)"
      assertFloat result (-4.0)

  , withSessionTest "complex float expression" $ \s -> do
      result <- eval s "(1.5 + 2.5) * 2.0 - 1.0"
      assertFloat result 7.0
  ]

-- | Tests for mixed integer/float operations (type coercion)
mixedIntFloatTests :: TestTree
mixedIntFloatTests = testGroup "Mixed int/float operations"
  [ -- Addition: int + float and float + int
    withSessionTest "int + float" $ \s -> do
      result <- eval s "1 + 2.5"
      assertFloat result 3.5

  , withSessionTest "float + int" $ \s -> do
      result <- eval s "2.5 + 1"
      assertFloat result 3.5

  , withSessionTest "int + float zero" $ \s -> do
      result <- eval s "5 + 0.0"
      assertFloat result 5.0

  -- Subtraction: int - float and float - int
  , withSessionTest "int - float" $ \s -> do
      result <- eval s "5 - 1.5"
      assertFloat result 3.5

  , withSessionTest "float - int" $ \s -> do
      result <- eval s "5.5 - 2"
      assertFloat result 3.5

  , withSessionTest "int - float negative result" $ \s -> do
      result <- eval s "1 - 2.5"
      assertFloat result (-1.5)

  -- Multiplication: int * float and float * int
  , withSessionTest "int * float" $ \s -> do
      result <- eval s "3 * 2.5"
      assertFloat result 7.5

  , withSessionTest "float * int" $ \s -> do
      result <- eval s "2.5 * 3"
      assertFloat result 7.5

  , withSessionTest "int * float zero" $ \s -> do
      result <- eval s "5 * 0.0"
      assertFloat result 0.0

  -- Division: int / float and float / int
  , withSessionTest "int / float" $ \s -> do
      result <- eval s "5 / 2.0"
      assertFloat result 2.5

  , withSessionTest "float / int" $ \s -> do
      result <- eval s "5.0 / 2"
      assertFloat result 2.5

  , withSessionTest "int / int producing float context" $ \s -> do
      -- In Nix, 10 / 3 is integer division, but this tests the evaluator
      -- 10 / 4.0 should give 2.5
      result <- eval s "10 / 4.0"
      assertFloat result 2.5

  -- Mixed in complex expressions
  , withSessionTest "mixed expression" $ \s -> do
      result <- eval s "(2 + 3.5) * 2"
      assertFloat result 11.0

  , withSessionTest "mixed with parentheses" $ \s -> do
      result <- eval s "10 / (2 + 2.0)"
      assertFloat result 2.5
  ]

-- | Tests for float comparison operations
floatComparisonTests :: TestTree
floatComparisonTests = testGroup "Float comparison"
  [ -- Less than
    withSessionTest "float less than true" $ \s -> do
      result <- eval s "1.5 < 2.5"
      assertBoolVal result True

  , withSessionTest "float less than false" $ \s -> do
      result <- eval s "2.5 < 1.5"
      assertBoolVal result False

  , withSessionTest "float less than equal" $ \s -> do
      result <- eval s "2.5 < 2.5"
      assertBoolVal result False

  -- Greater than
  , withSessionTest "float greater than true" $ \s -> do
      result <- eval s "2.5 > 1.5"
      assertBoolVal result True

  , withSessionTest "float greater than false" $ \s -> do
      result <- eval s "1.5 > 2.5"
      assertBoolVal result False

  , withSessionTest "float greater than equal" $ \s -> do
      result <- eval s "2.5 > 2.5"
      assertBoolVal result False

  -- Equality
  , withSessionTest "float equality true" $ \s -> do
      result <- eval s "3.14 == 3.14"
      assertBoolVal result True

  , withSessionTest "float equality false" $ \s -> do
      result <- eval s "3.14 == 2.71"
      assertBoolVal result False

  , withSessionTest "float int equality" $ \s -> do
      -- In Nix, 2.0 == 2 should be true
      result <- eval s "2.0 == 2"
      assertBoolVal result True

  , withSessionTest "int float equality" $ \s -> do
      result <- eval s "2 == 2.0"
      assertBoolVal result True

  -- Inequality
  , withSessionTest "float inequality true" $ \s -> do
      result <- eval s "3.14 != 2.71"
      assertBoolVal result True

  , withSessionTest "float inequality false" $ \s -> do
      result <- eval s "3.14 != 3.14"
      assertBoolVal result False

  -- Less than or equal
  , withSessionTest "float lte less" $ \s -> do
      result <- eval s "1.5 <= 2.5"
      assertBoolVal result True

  , withSessionTest "float lte equal" $ \s -> do
      result <- eval s "2.5 <= 2.5"
      assertBoolVal result True

  , withSessionTest "float lte greater" $ \s -> do
      result <- eval s "3.5 <= 2.5"
      assertBoolVal result False

  -- Greater than or equal
  , withSessionTest "float gte greater" $ \s -> do
      result <- eval s "3.5 >= 2.5"
      assertBoolVal result True

  , withSessionTest "float gte equal" $ \s -> do
      result <- eval s "2.5 >= 2.5"
      assertBoolVal result True

  , withSessionTest "float gte less" $ \s -> do
      result <- eval s "1.5 >= 2.5"
      assertBoolVal result False

  -- Mixed int/float comparisons
  , withSessionTest "int float comparison less" $ \s -> do
      result <- eval s "1 < 1.5"
      assertBoolVal result True

  , withSessionTest "float int comparison greater" $ \s -> do
      result <- eval s "2.5 > 2"
      assertBoolVal result True

  , withSessionTest "int float comparison lte" $ \s -> do
      result <- eval s "2 <= 2.0"
      assertBoolVal result True
  ]

-- | Tests for edge cases including very small floats and scientific notation
floatEdgeCaseTests :: TestTree
floatEdgeCaseTests = testGroup "Float edge cases"
  [ -- Very small floats
    withSessionTest "very small float" $ \s -> do
      result <- eval s "0.000001"
      assertFloatApprox result 0.000001 0.0000001

  , withSessionTest "very small float arithmetic" $ \s -> do
      result <- eval s "0.000001 + 0.000001"
      assertFloatApprox result 0.000002 0.0000001

  , withSessionTest "very small float multiplication" $ \s -> do
      result <- eval s "0.001 * 0.001"
      assertFloatApprox result 0.000001 0.0000001

  -- Scientific notation - large numbers
  , withSessionTest "scientific notation 1e10" $ \s -> do
      result <- eval s "1e10"
      assertFloat result 1e10

  , withSessionTest "scientific notation 1.5e10" $ \s -> do
      result <- eval s "1.5e10"
      assertFloat result 1.5e10

  , withSessionTest "scientific notation uppercase E" $ \s -> do
      result <- eval s "1E10"
      assertFloat result 1e10

  -- Scientific notation - small numbers
  , withSessionTest "scientific notation 1e-3" $ \s -> do
      result <- eval s "1e-3"
      assertFloatApprox result 0.001 0.0001

  , withSessionTest "scientific notation 1.5e-3" $ \s -> do
      result <- eval s "1.5e-3"
      assertFloatApprox result 0.0015 0.0001

  , withSessionTest "scientific notation 5e-10" $ \s -> do
      result <- eval s "5e-10"
      assertFloatApprox result 5e-10 1e-15

  -- Scientific notation arithmetic
  , withSessionTest "scientific notation addition" $ \s -> do
      result <- eval s "1e10 + 1e10"
      assertFloat result 2e10

  , withSessionTest "scientific notation multiplication" $ \s -> do
      result <- eval s "1e5 * 1e5"
      assertFloat result 1e10

  , withSessionTest "scientific notation with regular float" $ \s -> do
      result <- eval s "1e3 + 500.0"
      assertFloat result 1500.0

  -- Zero and negative zero edge cases
  , withSessionTest "float zero" $ \s -> do
      result <- eval s "0.0"
      assertFloat result 0.0

  , withSessionTest "negative float zero" $ \s -> do
      result <- eval s "-0.0"
      -- -0.0 should compare equal to 0.0 in IEEE 754
      assertFloat result 0.0

  , withSessionTest "float zero equality" $ \s -> do
      result <- eval s "0.0 == -0.0"
      assertBoolVal result True

  -- Large floats
  , withSessionTest "large float" $ \s -> do
      result <- eval s "999999999.999999"
      assertFloatApprox result 999999999.999999 0.001

  -- Precision edge cases
  , withSessionTest "float precision" $ \s -> do
      -- Testing that we don't lose precision in common cases
      result <- eval s "1.0 / 3.0 * 3.0"
      assertFloatApprox result 1.0 0.0001

  -- Negative scientific notation
  , withSessionTest "negative scientific notation" $ \s -> do
      result <- eval s "-1e10"
      assertFloat result (-1e10)

  , withSessionTest "negative coefficient scientific" $ \s -> do
      result <- eval s "-1.5e-3"
      assertFloatApprox result (-0.0015) 0.0001

  -- Type checking with floats
  , withSessionTest "isFloat true" $ \s -> do
      result <- eval s "builtins.isFloat 3.14"
      assertBoolVal result True

  , withSessionTest "isFloat false for int" $ \s -> do
      result <- eval s "builtins.isFloat 42"
      assertBoolVal result False

  , withSessionTest "typeOf float" $ \s -> do
      result <- eval s "builtins.typeOf 3.14"
      assertStringVal result "float"
  ]
