-- | Tests for pattern-matching functions in the Nix compiler.
--
-- These tests verify that the compiler correctly handles pattern-matching
-- function forms including simple patterns, defaults, variadic arguments,
-- @-bindings, and complex nested patterns.
module Compile.PatternFunctionTests (tests) where

import Relude
import Compile.TestCommon

-- | All pattern function tests
tests :: TestTree
tests = testGroup "Pattern Functions"
  [ simplePatternTests
  , defaultValueTests
  , variadicTests
  , atBindingTests
  , complexPatternTests
  , errorCaseTests
  ]

-- | Tests for simple pattern matching without defaults or variadic
simplePatternTests :: TestTree
simplePatternTests = testGroup "Simple patterns"
  [ withSessionTest "single param" $ \s -> do
      result <- eval s "({ x }: x) { x = 42; }"
      assertInt result 42

  , withSessionTest "two params" $ \s -> do
      result <- eval s "({ x, y }: x + y) { x = 10; y = 20; }"
      assertInt result 30

  , withSessionTest "three params" $ \s -> do
      result <- eval s "({ a, b, c }: a + b + c) { a = 1; b = 2; c = 3; }"
      assertInt result 6

  , withSessionTest "params used multiple times" $ \s -> do
      result <- eval s "({ x }: x + x) { x = 5; }"
      assertInt result 10

  , withSessionTest "unused param" $ \s -> do
      result <- eval s "({ x, y }: x) { x = 1; y = 2; }"
      assertInt result 1

  , withSessionTest "params in expression" $ \s -> do
      result <- eval s "({ x, y }: if x > y then x else y) { x = 10; y = 5; }"
      assertInt result 10

  , withSessionTest "string params" $ \s -> do
      result <- eval s "({ s }: s + \"!\") { s = \"hello\"; }"
      assertStringVal result "hello!"

  , withSessionTest "nested set access in pattern body" $ \s -> do
      result <- eval s "({ x }: x.a) { x = { a = 99; }; }"
      assertInt result 99
  ]

-- | Tests for default values in patterns
defaultValueTests :: TestTree
defaultValueTests = testGroup "Default values"
  [ withSessionTest "default not used" $ \s -> do
      result <- eval s "({ x ? 1 }: x) { x = 42; }"
      assertInt result 42

  , withSessionTest "default used" $ \s -> do
      result <- eval s "({ x ? 1 }: x) { }"
      assertInt result 1

  , withSessionTest "multiple defaults all provided" $ \s -> do
      result <- eval s "({ x ? 1, y ? 2 }: x + y) { x = 10; y = 20; }"
      assertInt result 30

  , withSessionTest "multiple defaults none provided" $ \s -> do
      result <- eval s "({ x ? 1, y ? 2 }: x + y) { }"
      assertInt result 3

  , withSessionTest "multiple defaults partial provided" $ \s -> do
      result <- eval s "({ x ? 1, y ? 2 }: x + y) { x = 10; }"
      assertInt result 12

  , withSessionTest "required and optional mixed" $ \s -> do
      result <- eval s "({ x, y ? 2 }: x + y) { x = 10; }"
      assertInt result 12

  , withSessionTest "required and optional both provided" $ \s -> do
      result <- eval s "({ x, y ? 2 }: x + y) { x = 10; y = 30; }"
      assertInt result 40

  , withSessionTest "default is expression" $ \s -> do
      result <- eval s "({ x ? 2 + 3 }: x) { }"
      assertInt result 5

  , withSessionTest "default is null" $ \s -> do
      result <- eval s "({ x ? null }: x) { }"
      assertNull result

  , withSessionTest "default is empty set" $ \s -> do
      result <- eval s "({ x ? {} }: x) { }"
      assertAttrsSize result 0

  , withSessionTest "default is empty list" $ \s -> do
      result <- eval s "({ x ? [] }: x) { }"
      assertListLength result 0

  , withSessionTest "default refers to another param" $ \s -> do
      -- This is a special Nix feature: defaults can reference other params
      result <- eval s "({ x, y ? x + 1 }: y) { x = 10; }"
      assertInt result 11

  , withSessionTest "default refers to another default" $ \s -> do
      -- When neither is provided, x gets its default, then y uses x
      result <- eval s "({ x ? 5, y ? x * 2 }: y) { }"
      assertInt result 10

  , withSessionTest "chained defaults" $ \s -> do
      result <- eval s "({ a ? 1, b ? a + 1, c ? b + 1 }: c) { }"
      assertInt result 3
  ]

-- | Tests for variadic patterns (...)
variadicTests :: TestTree
variadicTests = testGroup "Variadic patterns"
  [ withSessionTest "variadic accepts extra args" $ \s -> do
      result <- eval s "({ x, ... }: x) { x = 1; y = 2; z = 3; }"
      assertInt result 1

  , withSessionTest "variadic with no extra args" $ \s -> do
      result <- eval s "({ x, ... }: x) { x = 42; }"
      assertInt result 42

  , withSessionTest "variadic with default" $ \s -> do
      result <- eval s "({ x ? 0, ... }: x) { y = 1; z = 2; }"
      assertInt result 0

  , withSessionTest "variadic default and extra" $ \s -> do
      result <- eval s "({ x ? 0, ... }: x) { x = 99; extra = true; }"
      assertInt result 99

  , withSessionTest "variadic multiple named params" $ \s -> do
      result <- eval s "({ a, b, ... }: a + b) { a = 1; b = 2; c = 3; d = 4; }"
      assertInt result 3

  , withSessionTest "variadic accesses extra via atbinding" $ \s -> do
      -- Use @args to access the full argument set including extras
      result <- eval s "({ x, ... }@args: args.y) { x = 1; y = 42; }"
      assertInt result 42

  , withSessionTest "variadic atbinding access multiple extras" $ \s -> do
      result <- eval s "({ x, ... }@args: args.y + args.z) { x = 1; y = 10; z = 20; }"
      assertInt result 30

  , withSessionTest "variadic empty extras" $ \s -> do
      -- Just named params, no extras - should work fine
      result <- eval s "({ x, ... }: x * 2) { x = 5; }"
      assertInt result 10
  ]

-- | Tests for @-binding patterns
atBindingTests :: TestTree
atBindingTests = testGroup "@-binding patterns"
  [ withSessionTest "atbinding before pattern" $ \s -> do
      result <- eval s "(args@{ x }: x) { x = 42; }"
      assertInt result 42

  , withSessionTest "atbinding after pattern" $ \s -> do
      result <- eval s "({ x }@args: x) { x = 42; }"
      assertInt result 42

  , withSessionTest "atbinding access whole set" $ \s -> do
      result <- eval s "({ x }@args: args) { x = 1; }"
      assertAttrsSize result 1

  , withSessionTest "atbinding and pattern together" $ \s -> do
      result <- eval s "(args@{ x }: args.x + x) { x = 10; }"
      assertInt result 20

  , withSessionTest "atbinding return whole set" $ \s -> do
      result <- eval s "({ x }@args: args) { x = 42; }"
      assertAttrs result $ \attrs -> do
        case lookupAttr "x" attrs of
          Just (VInt n) -> n @?= 42
          Just v -> assertFailure $ "Expected int for x, got: " <> show v
          Nothing -> assertFailure "Missing attr x"

  , withSessionTest "atbinding with multiple params" $ \s -> do
      result <- eval s "(args@{ x, y }: args.x + args.y) { x = 10; y = 20; }"
      assertInt result 30

  , withSessionTest "atbinding with defaults" $ \s -> do
      result <- eval s "(args@{ x ? 1 }: args) { }"
      -- The atbinding should capture the empty input set, not the defaulted one
      assertAttrsSize result 0

  , withSessionTest "atbinding variadic captures all" $ \s -> do
      result <- eval s "({ x, ... }@args: args) { x = 1; y = 2; z = 3; }"
      assertAttrsSize result 3

  , withSessionTest "atbinding nested access" $ \s -> do
      result <- eval s "(args@{ x }: args.x * args.x) { x = 7; }"
      assertInt result 49

  , withSessionTest "atbinding in let" $ \s -> do
      result <- eval s "let f = args@{ x }: args.x + 1; in f { x = 10; }"
      assertInt result 11
  ]

-- | Tests for complex pattern scenarios
complexPatternTests :: TestTree
complexPatternTests = testGroup "Complex patterns"
  [ withSessionTest "nested function calls" $ \s -> do
      result <- eval s "let f = { x }: { y }: x + y; in f { x = 1; } { y = 2; }"
      assertInt result 3

  , withSessionTest "curried pattern functions" $ \s -> do
      result <- eval s "let f = { x }: { y }: { z }: x + y + z; in f { x = 1; } { y = 2; } { z = 3; }"
      assertInt result 6

  , withSessionTest "pattern function in rec set" $ \s -> do
      result <- eval s "rec { f = { x }: x + 1; result = f { x = 10; }; }.result"
      assertInt result 11

  , withSessionTest "pattern function returns pattern function" $ \s -> do
      result <- eval s "(({ a }: { b }: a + b) { a = 5; }) { b = 7; }"
      assertInt result 12

  , withSessionTest "pattern function in list" $ \s -> do
      result <- eval s "let fs = [ ({ x }: x + 1) ({ x }: x * 2) ]; in (builtins.elemAt fs 0) { x = 5; }"
      assertInt result 6

  , withSessionTest "pattern function applied to computed set" $ \s -> do
      result <- eval s "({ x }: x) (let n = 42; in { x = n; })"
      assertInt result 42

  , withSessionTest "higher order pattern function" $ \s -> do
      result <- eval s "let apply = { f, x }: f x; in apply { f = n: n + 1; x = 10; }"
      assertInt result 11

  , withSessionTest "pattern destructures computed set" $ \s -> do
      result <- eval s "let mkSet = n: { x = n; y = n * 2; }; in ({ x, y }: x + y) (mkSet 5)"
      assertInt result 15

  , withSessionTest "default using param with more computation" $ \s -> do
      result <- eval s "({ x, y ? x * x + x }: y) { x = 3; }"
      assertInt result 12

  , withSessionTest "chained defaults complex" $ \s -> do
      result <- eval s "({ a ? 2, b ? a * 3, c ? b + a }: c) { }"
      assertInt result 8

  , withSessionTest "variadic with defaults and atbinding" $ \s -> do
      result <- eval s "({ x ? 1, ... }@args: if args ? y then args.y else x) { y = 99; }"
      assertInt result 99

  , withSessionTest "variadic atbinding default fallback" $ \s -> do
      result <- eval s "({ x ? 1, ... }@args: if args ? y then args.y else x) { }"
      assertInt result 1

  , withSessionTest "pattern in with scope" $ \s -> do
      result <- eval s "with { f = { x }: x + 1; }; f { x = 41; }"
      assertInt result 42

  , withSessionTest "nested atbinding" $ \s -> do
      -- Outer function captures args, calls inner with x
      result <- eval s "let f = args@{ x }: ({ y }: args.x + y); in f { x = 10; } { y = 5; }"
      assertInt result 15
  ]

-- | Tests for error cases
errorCaseTests :: TestTree
errorCaseTests = testGroup "Error cases"
  [ withSessionTest "missing required arg" $ \s -> do
      expectError s "({ x }: x) { }"

  , withSessionTest "missing one of multiple required args" $ \s -> do
      expectError s "({ x, y }: x + y) { x = 1; }"

  , withSessionTest "extra arg without variadic" $ \s -> do
      expectError s "({ x }: x) { x = 1; y = 2; }"

  , withSessionTest "multiple extra args without variadic" $ \s -> do
      expectError s "({ x }: x) { x = 1; y = 2; z = 3; }"

  , withSessionTest "completely wrong arg names" $ \s -> do
      expectError s "({ x, y }: x) { a = 1; b = 2; }"

  , withSessionTest "apply pattern func to non-set" $ \s -> do
      expectError s "({ x }: x) 42"

  , withSessionTest "apply pattern func to list" $ \s -> do
      expectError s "({ x }: x) [ 1 2 3 ]"

  , withSessionTest "apply pattern func to null" $ \s -> do
      expectError s "({ x }: x) null"

  , withSessionTest "apply pattern func to string" $ \s -> do
      expectError s "({ x }: x) \"hello\""
  ]
