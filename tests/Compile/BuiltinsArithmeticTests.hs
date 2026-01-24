-- | Tests for arithmetic builtins in the Nix compiler.
--
-- This module tests the curried forms of arithmetic operations available
-- through builtins, including add, sub, mul, div, floor, ceil, and bitwise
-- operations.
module Compile.BuiltinsArithmeticTests (tests) where

import Relude
import Test.Tasty

import Compile.TestCommon

-- | All arithmetic builtin tests
tests :: TestTree
tests = testGroup "Builtins.Arithmetic"
  [ addTests
  , subTests
  , mulTests
  , divTests
  , floorCeilTests
  , bitwiseTests
  , lessThanTests
  , partialApplicationTests
  , mixedTypeTests
  ]

-- | Tests for builtins.add
addTests :: TestTree
addTests = testGroup "add"
  [ withSessionTest "add curried basic" $ \s -> do
      result <- eval s "builtins.add 2 3"
      assertInt result 5

  , withSessionTest "add curried zeros" $ \s -> do
      result <- eval s "builtins.add 0 0"
      assertInt result 0

  , withSessionTest "add curried negative" $ \s -> do
      result <- eval s "builtins.add (-5) 3"
      assertInt result (-2)

  , withSessionTest "add curried both negative" $ \s -> do
      result <- eval s "builtins.add (-5) (-3)"
      assertInt result (-8)

  , withSessionTest "add curried large numbers" $ \s -> do
      result <- eval s "builtins.add 1000000 2000000"
      assertInt result 3000000

  , withSessionTest "add floats" $ \s -> do
      result <- eval s "builtins.add 1.5 2.5"
      assertFloat result 4.0

  , withSessionTest "add mixed int float" $ \s -> do
      result <- eval s "builtins.add 1 2.5"
      assertFloat result 3.5

  , withSessionTest "add mixed float int" $ \s -> do
      result <- eval s "builtins.add 1.5 2"
      assertFloat result 3.5
  ]

-- | Tests for builtins.sub
subTests :: TestTree
subTests = testGroup "sub"
  [ withSessionTest "sub curried basic" $ \s -> do
      result <- eval s "builtins.sub 5 3"
      assertInt result 2

  , withSessionTest "sub curried to zero" $ \s -> do
      result <- eval s "builtins.sub 5 5"
      assertInt result 0

  , withSessionTest "sub curried negative result" $ \s -> do
      result <- eval s "builtins.sub 3 5"
      assertInt result (-2)

  , withSessionTest "sub curried with negative" $ \s -> do
      result <- eval s "builtins.sub 5 (-3)"
      assertInt result 8

  , withSessionTest "sub floats" $ \s -> do
      result <- eval s "builtins.sub 5.5 2.5"
      assertFloat result 3.0

  , withSessionTest "sub mixed int float" $ \s -> do
      result <- eval s "builtins.sub 5 2.5"
      assertFloat result 2.5
  ]

-- | Tests for builtins.mul
mulTests :: TestTree
mulTests = testGroup "mul"
  [ withSessionTest "mul curried basic" $ \s -> do
      result <- eval s "builtins.mul 3 4"
      assertInt result 12

  , withSessionTest "mul curried by zero" $ \s -> do
      result <- eval s "builtins.mul 100 0"
      assertInt result 0

  , withSessionTest "mul curried by one" $ \s -> do
      result <- eval s "builtins.mul 42 1"
      assertInt result 42

  , withSessionTest "mul curried negative" $ \s -> do
      result <- eval s "builtins.mul (-3) 4"
      assertInt result (-12)

  , withSessionTest "mul curried both negative" $ \s -> do
      result <- eval s "builtins.mul (-3) (-4)"
      assertInt result 12

  , withSessionTest "mul floats" $ \s -> do
      result <- eval s "builtins.mul 2.5 4.0"
      assertFloat result 10.0

  , withSessionTest "mul mixed int float" $ \s -> do
      result <- eval s "builtins.mul 3 2.5"
      assertFloat result 7.5
  ]

-- | Tests for builtins.div
divTests :: TestTree
divTests = testGroup "div"
  [ withSessionTest "div curried exact" $ \s -> do
      result <- eval s "builtins.div 10 2"
      assertInt result 5

  , withSessionTest "div curried truncates" $ \s -> do
      -- Integer division truncates towards zero
      result <- eval s "builtins.div 10 3"
      assertInt result 3

  , withSessionTest "div curried negative truncates towards zero" $ \s -> do
      -- Nix truncates towards zero for negative
      result <- eval s "builtins.div (-10) 3"
      assertInt result (-3)

  , withSessionTest "div curried by one" $ \s -> do
      result <- eval s "builtins.div 42 1"
      assertInt result 42

  , withSessionTest "div floats" $ \s -> do
      result <- eval s "builtins.div 5.0 2.0"
      assertFloat result 2.5

  , withSessionTest "div mixed int float" $ \s -> do
      result <- eval s "builtins.div 5 2.0"
      assertFloat result 2.5

  , withSessionTest "div mixed float int" $ \s -> do
      result <- eval s "builtins.div 5.0 2"
      assertFloat result 2.5

  , withSessionTest "div float precise" $ \s -> do
      result <- eval s "builtins.div 7.0 2.0"
      assertFloat result 3.5
  ]

-- | Tests for builtins.floor and builtins.ceil
floorCeilTests :: TestTree
floorCeilTests = testGroup "floor/ceil"
  [ -- floor tests
    withSessionTest "floor positive" $ \s -> do
      result <- eval s "builtins.floor 3.7"
      assertInt result 3

  , withSessionTest "floor negative" $ \s -> do
      result <- eval s "builtins.floor (-3.7)"
      assertInt result (-4)

  , withSessionTest "floor exact" $ \s -> do
      result <- eval s "builtins.floor 5.0"
      assertInt result 5

  , withSessionTest "floor small fraction" $ \s -> do
      result <- eval s "builtins.floor 2.001"
      assertInt result 2

  , withSessionTest "floor large fraction" $ \s -> do
      result <- eval s "builtins.floor 2.999"
      assertInt result 2

  , withSessionTest "floor zero" $ \s -> do
      result <- eval s "builtins.floor 0.0"
      assertInt result 0

  , withSessionTest "floor negative small fraction" $ \s -> do
      result <- eval s "builtins.floor (-0.001)"
      assertInt result (-1)

  -- ceil tests
  , withSessionTest "ceil positive" $ \s -> do
      result <- eval s "builtins.ceil 3.2"
      assertInt result 4

  , withSessionTest "ceil negative" $ \s -> do
      result <- eval s "builtins.ceil (-3.2)"
      assertInt result (-3)

  , withSessionTest "ceil exact" $ \s -> do
      result <- eval s "builtins.ceil 5.0"
      assertInt result 5

  , withSessionTest "ceil small fraction" $ \s -> do
      result <- eval s "builtins.ceil 2.001"
      assertInt result 3

  , withSessionTest "ceil large fraction" $ \s -> do
      result <- eval s "builtins.ceil 2.999"
      assertInt result 3

  , withSessionTest "ceil zero" $ \s -> do
      result <- eval s "builtins.ceil 0.0"
      assertInt result 0

  , withSessionTest "ceil negative small fraction" $ \s -> do
      result <- eval s "builtins.ceil (-0.001)"
      assertInt result 0
  ]

-- | Tests for bitwise operations
bitwiseTests :: TestTree
bitwiseTests = testGroup "bitwise"
  [ -- bitAnd tests
    withSessionTest "bitAnd basic" $ \s -> do
      result <- eval s "builtins.bitAnd 12 10"
      -- 12 = 1100, 10 = 1010, AND = 1000 = 8
      assertInt result 8

  , withSessionTest "bitAnd all ones" $ \s -> do
      result <- eval s "builtins.bitAnd 15 15"
      assertInt result 15

  , withSessionTest "bitAnd with zero" $ \s -> do
      result <- eval s "builtins.bitAnd 255 0"
      assertInt result 0

  , withSessionTest "bitAnd disjoint bits" $ \s -> do
      result <- eval s "builtins.bitAnd 12 3"
      -- 12 = 1100, 3 = 0011, AND = 0000 = 0
      assertInt result 0

  -- bitOr tests
  , withSessionTest "bitOr basic" $ \s -> do
      result <- eval s "builtins.bitOr 12 10"
      -- 12 = 1100, 10 = 1010, OR = 1110 = 14
      assertInt result 14

  , withSessionTest "bitOr with zero" $ \s -> do
      result <- eval s "builtins.bitOr 42 0"
      assertInt result 42

  , withSessionTest "bitOr disjoint bits" $ \s -> do
      result <- eval s "builtins.bitOr 12 3"
      -- 12 = 1100, 3 = 0011, OR = 1111 = 15
      assertInt result 15

  , withSessionTest "bitOr same value" $ \s -> do
      result <- eval s "builtins.bitOr 7 7"
      assertInt result 7

  -- bitXor tests
  , withSessionTest "bitXor basic" $ \s -> do
      result <- eval s "builtins.bitXor 12 10"
      -- 12 = 1100, 10 = 1010, XOR = 0110 = 6
      assertInt result 6

  , withSessionTest "bitXor same value" $ \s -> do
      result <- eval s "builtins.bitXor 42 42"
      assertInt result 0

  , withSessionTest "bitXor with zero" $ \s -> do
      result <- eval s "builtins.bitXor 42 0"
      assertInt result 42

  , withSessionTest "bitXor disjoint bits" $ \s -> do
      result <- eval s "builtins.bitXor 12 3"
      -- 12 = 1100, 3 = 0011, XOR = 1111 = 15
      assertInt result 15

  , withSessionTest "bitXor self inverse" $ \s -> do
      -- a XOR b XOR b = a
      result <- eval s "builtins.bitXor (builtins.bitXor 42 99) 99"
      assertInt result 42
  ]

-- | Tests for builtins.lessThan
lessThanTests :: TestTree
lessThanTests = testGroup "lessThan"
  [ withSessionTest "lessThan curried true" $ \s -> do
      result <- eval s "builtins.lessThan 1 2"
      assertBoolVal result True

  , withSessionTest "lessThan curried false" $ \s -> do
      result <- eval s "builtins.lessThan 2 1"
      assertBoolVal result False

  , withSessionTest "lessThan curried equal" $ \s -> do
      result <- eval s "builtins.lessThan 2 2"
      assertBoolVal result False

  , withSessionTest "lessThan negative" $ \s -> do
      result <- eval s "builtins.lessThan (-5) (-3)"
      assertBoolVal result True

  , withSessionTest "lessThan floats" $ \s -> do
      result <- eval s "builtins.lessThan 1.5 2.5"
      assertBoolVal result True

  , withSessionTest "lessThan mixed int float" $ \s -> do
      result <- eval s "builtins.lessThan 1 1.5"
      assertBoolVal result True

  , withSessionTest "lessThan strings" $ \s -> do
      result <- eval s "builtins.lessThan \"abc\" \"abd\""
      assertBoolVal result True

  , withSessionTest "lessThan strings equal" $ \s -> do
      result <- eval s "builtins.lessThan \"abc\" \"abc\""
      assertBoolVal result False
  ]

-- | Tests for partial application of curried builtins
-- Note: We only test add partial as a representative case since all builtins
-- use the same currying mechanism. More complex patterns (map, filter, chaining)
-- are kept to test partial application in realistic contexts.
partialApplicationTests :: TestTree
partialApplicationTests = testGroup "partial application"
  [ withSessionTest "add partial" $ \s -> do
      result <- eval s "let add5 = builtins.add 5; in add5 3"
      assertInt result 8

  , withSessionTest "partial in map" $ \s -> do
      result <- eval s "builtins.map (builtins.add 10) [ 1 2 3 ]"
      assertList result
        [ flip assertInt 11
        , flip assertInt 12
        , flip assertInt 13
        ]

  , withSessionTest "partial in filter comparison" $ \s -> do
      result <- eval s "builtins.filter (x: builtins.lessThan x 3) [ 1 2 3 4 5 ]"
      assertList result
        [ flip assertInt 1
        , flip assertInt 2
        ]

  , withSessionTest "chained partial application" $ \s -> do
      -- ((add 1) 2) is the same as add 1 2
      result <- eval s "(builtins.add 1) 2"
      assertInt result 3
  ]

-- | Tests for mixed integer/float operations
mixedTypeTests :: TestTree
mixedTypeTests = testGroup "mixed types"
  [ withSessionTest "add int to float result is float" $ \s -> do
      result <- eval s "builtins.add 1 2.0"
      assertFloat result 3.0

  , withSessionTest "mul int by float result is float" $ \s -> do
      result <- eval s "builtins.mul 3 1.5"
      assertFloat result 4.5

  , withSessionTest "div int by float result is float" $ \s -> do
      result <- eval s "builtins.div 5 2.0"
      assertFloat result 2.5

  , withSessionTest "chain operations mixed types" $ \s -> do
      -- Start with int, add float, result is float
      result <- eval s "builtins.add (builtins.mul 2 3) 0.5"
      assertFloat result 6.5

  , withSessionTest "floor of int operation" $ \s -> do
      -- Float division then floor
      result <- eval s "builtins.floor (builtins.div 5.0 2.0)"
      assertInt result 2

  , withSessionTest "ceil of int operation" $ \s -> do
      result <- eval s "builtins.ceil (builtins.div 5.0 2.0)"
      assertInt result 3

  , withSessionTest "negative float operations" $ \s -> do
      result <- eval s "builtins.add (-1.5) (-2.5)"
      assertFloat result (-4.0)

  , withSessionTest "very small float" $ \s -> do
      result <- eval s "builtins.add 0.0001 0.0001"
      assertFloatApprox result 0.0002 0.00001

  , withSessionTest "very large numbers" $ \s -> do
      result <- eval s "builtins.mul 1000000 1000000"
      assertInt result 1000000000000
  ]
