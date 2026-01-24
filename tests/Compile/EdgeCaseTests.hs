-- | Comprehensive edge case tests for the Nix compiler.
--
-- This module tests boundary conditions, unusual inputs, and corner cases
-- that might expose bugs in parsing, evaluation, or code generation.
-- Categories include empty structures, unicode handling, large values,
-- deep nesting, recursive sets, and string edge cases.
module Compile.EdgeCaseTests (tests) where

import Relude
import Test.Tasty

import Compile.TestCommon

-- | All edge case tests
tests :: TestTree
tests = testGroup "Edge Case Tests"
  [ emptyStructureTests
  , unicodeTests
  , largeValueTests
  , deepNestingTests
  , recursiveSetTests
  , supportedBindingTests
  , stringEdgeCaseTests
  , envPathTests
  , pathInterpolationTests
  ]

-- | Tests for empty structures
emptyStructureTests :: TestTree
emptyStructureTests = testGroup "Empty Structures"
  [ testGroup "Empty string"
      [ withSessionTest "empty string literal" $ \s -> do
          result <- eval s "\"\""
          assertStringVal result ""

      , withSessionTest "empty string length" $ \s -> do
          result <- eval s "builtins.stringLength \"\""
          assertInt result 0
      ]

  , testGroup "Empty list"
      [ withSessionTest "empty list literal" $ \s -> do
          result <- eval s "[]"
          assertListLength result 0

      , withSessionTest "empty list length" $ \s -> do
          result <- eval s "builtins.length []"
          assertInt result 0

      , withSessionTest "empty list head fails" $ \s -> do
          expectError s "builtins.head []"

      , withSessionTest "empty list tail fails" $ \s -> do
          expectError s "builtins.tail []"
      ]

  , testGroup "Empty attrset"
      [ withSessionTest "empty attrset literal" $ \s -> do
          result <- eval s "{}"
          assertAttrsSize result 0

      , withSessionTest "empty attrset attrNames" $ \s -> do
          result <- eval s "builtins.attrNames {}"
          assertListLength result 0
      ]

  , testGroup "Empty string operations"
      [ withSessionTest "substring of empty" $ \s -> do
          result <- eval s "builtins.substring 0 10 \"\""
          assertStringVal result ""

      , withSessionTest "replaceStrings on empty" $ \s -> do
          result <- eval s "builtins.replaceStrings [\"a\"] [\"b\"] \"\""
          assertStringVal result ""

      , withSessionTest "split empty string" $ \s -> do
          result <- eval s "builtins.split \"x\" \"\""
          assertListLength result 1

      , withSessionTest "match empty string" $ \s -> do
          result <- eval s "builtins.match \"\" \"\""
          assertListLength result 0

      , withSessionTest "hash empty string" $ \s -> do
          result <- eval s "builtins.hashString \"sha256\" \"\""
          assertStringVal result "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      ]
  ]

-- | Tests for unicode handling
unicodeTests :: TestTree
unicodeTests = testGroup "Unicode Handling"
  [ testGroup "Unicode in strings"
      [ withSessionTest "unicode characters" $ \s -> do
          result <- eval s "\"hello \228\184\150\231\149\140\""
          assertIsString result

      , withSessionTest "unicode emoji" $ \s -> do
          result <- eval s "\"\240\159\152\128\""
          assertIsString result

      , withSessionTest "unicode equality" $ \s -> do
          result <- eval s "\"\228\184\150\231\149\140\" == \"\228\184\150\231\149\140\""
          assertBoolVal result True

      , withSessionTest "mixed ascii and unicode" $ \s -> do
          result <- eval s "\"hello \228\184\150\231\149\140 world\""
          assertStringContains result "hello"
      ]

  , testGroup "Unicode string length"
      [ withSessionTest "unicode length counts codepoints" $ \s -> do
          -- "abc" is 3 codepoints
          result <- eval s "builtins.stringLength \"abc\""
          assertInt result 3

      , withSessionTest "string with newlines" $ \s -> do
          -- "a\nb" is 3 characters
          result <- eval s "builtins.stringLength \"a\\nb\""
          assertInt result 3
      ]

  , testGroup "Unicode in interpolation"
      [ withSessionTest "unicode in interpolated expression" $ \s -> do
          result <- eval s "let x = \"世界\"; in \"hello ${x}\""
          assertIsString result

      , withSessionTest "unicode variable value" $ \s -> do
          -- 世界 (sekai - world) as variable name
          result <- eval s "let 世界 = 42; in 世界"
          assertInt result 42

      , withSessionTest "japanese variable name" $ \s -> do
          -- 日本語 (nihongo - Japanese language) as variable name
          result <- eval s "let 日本語 = 123; in 日本語"
          assertInt result 123

      , withSessionTest "unicode variable in attrset" $ \s -> do
          -- Unicode attribute name in attrset
          result <- eval s "{ 世界 = 99; }.世界"
          assertInt result 99

      , withSessionTest "unicode variable in function param" $ \s -> do
          -- Unicode function parameter name
          result <- eval s "(世界: 世界 + 1) 10"
          assertInt result 11
      ]
  ]

-- | Tests for large values
largeValueTests :: TestTree
largeValueTests = testGroup "Large Values"
  [ testGroup "Large integers"
      [ withSessionTest "large positive integer" $ \s -> do
          -- Near max int64: 2^62
          result <- eval s "4611686018427387904"
          assertInt result 4611686018427387904

      , withSessionTest "large negative integer" $ \s -> do
          -- Near min int64: -2^62
          result <- eval s "(-4611686018427387904)"
          assertInt result (-4611686018427387904)

      , withSessionTest "int64 max minus 1" $ \s -> do
          result <- eval s "9223372036854775806"
          assertInt result 9223372036854775806

      , withSessionTest "int64 min plus 1" $ \s -> do
          result <- eval s "(-9223372036854775807)"
          assertInt result (-9223372036854775807)

      , withSessionTest "large integer arithmetic" $ \s -> do
          result <- eval s "1000000000000 + 1000000000000"
          assertInt result 2000000000000

      , withSessionTest "large integer multiplication" $ \s -> do
          result <- eval s "1000000 * 1000000"
          assertInt result 1000000000000
      ]

  , testGroup "Large lists"
      [ withSessionTest "100 element list" $ \s -> do
          result <- eval s "builtins.genList (x: x) 100"
          assertListLength result 100

      , withSessionTest "100 element list first" $ \s -> do
          result <- eval s "builtins.elemAt (builtins.genList (x: x) 100) 0"
          assertInt result 0

      , withSessionTest "100 element list last" $ \s -> do
          result <- eval s "builtins.elemAt (builtins.genList (x: x) 100) 99"
          assertInt result 99

      , withSessionTest "1000 element list length" $ \s -> do
          result <- eval s "builtins.length (builtins.genList (x: x) 1000)"
          assertInt result 1000

      , withSessionTest "sum 100 elements" $ \s -> do
          result <- eval s "builtins.foldl' (a: b: a + b) 0 (builtins.genList (x: x) 100)"
          assertInt result 4950  -- sum of 0..99
      ]

  , testGroup "Large attrsets"
      [ withSessionTest "many attributes" $ \s -> do
          result <- eval s "builtins.listToAttrs (builtins.genList (i: { name = \"attr${toString i}\"; value = i; }) 50)"
          assertAttrsSize result 50

      , withSessionTest "access attribute in large set" $ \s -> do
          result <- eval s "(builtins.listToAttrs (builtins.genList (i: { name = \"attr${toString i}\"; value = i; }) 50)).attr25"
          assertInt result 25

      , withSessionTest "attrNames of large set" $ \s -> do
          result <- eval s "builtins.length (builtins.attrNames (builtins.listToAttrs (builtins.genList (i: { name = \"attr${toString i}\"; value = i; }) 100)))"
          assertInt result 100
      ]
  ]

-- | Tests for deeply nested structures
deepNestingTests :: TestTree
deepNestingTests = testGroup "Deep Nesting"
  [ testGroup "Nested attrsets"
      [ withSessionTest "4 levels deep access" $ \s -> do
          result <- eval s "{ a = { b = { c = { d = 42; }; }; }; }.a.b.c.d"
          assertInt result 42

      , withSessionTest "5 levels deep access" $ \s -> do
          result <- eval s "{ a = { b = { c = { d = { e = 100; }; }; }; }; }.a.b.c.d.e"
          assertInt result 100

      , withSessionTest "nested attrset with hasAttr" $ \s -> do
          result <- eval s "{ a = { b = { c = 1; }; }; }.a.b ? c"
          assertBoolVal result True

      , withSessionTest "nested attrset missing deep attr" $ \s -> do
          result <- eval s "{ a = { b = {}; }; }.a.b ? c"
          assertBoolVal result False

      , withSessionTest "hasAttr path with non-attrset intermediate returns false" $ \s -> do
          -- { a = 1; } ? a.b should return false, not throw
          -- because 1 is not an attrset, so a.b doesn't exist
          result <- eval s "{ a = 1; } ? a.b"
          assertBoolVal result False

      , withSessionTest "hasAttr path with non-attrset intermediate (nested)" $ \s -> do
          -- { a = { b = 42; }; } ? a.b.c should return false
          -- because 42 is not an attrset
          result <- eval s "{ a = { b = 42; }; } ? a.b.c"
          assertBoolVal result False

      , withSessionTest "hasAttr path with list intermediate returns false" $ \s -> do
          -- { a = [1 2 3]; } ? a.b should return false
          -- because a list is not an attrset
          result <- eval s "{ a = [1 2 3]; } ? a.b"
          assertBoolVal result False

      , withSessionTest "hasAttr path with string intermediate returns false" $ \s -> do
          -- { a = "hello"; } ? a.b should return false
          result <- eval s "{ a = \"hello\"; } ? a.b"
          assertBoolVal result False

      , withSessionTest "or default on nested access" $ \s -> do
          result <- eval s "{ a = { b = {}; }; }.a.b.c or 99"
          assertInt result 99
      ]

  , testGroup "Nested lists"
      [ withSessionTest "triple nested list" $ \s -> do
          result <- eval s "[[[1]]]"
          assertListLength result 1

      , withSessionTest "access triple nested" $ \s -> do
          result <- eval s "builtins.elemAt (builtins.elemAt (builtins.elemAt [[[42]]] 0) 0) 0"
          assertInt result 42

      , withSessionTest "quad nested list" $ \s -> do
          result <- eval s "builtins.elemAt (builtins.elemAt (builtins.elemAt (builtins.elemAt [[[[7]]]] 0) 0) 0) 0"
          assertInt result 7

      , withSessionTest "nested list length" $ \s -> do
          result <- eval s "builtins.length [[1 2] [3 4] [5 6]]"
          assertInt result 3

      , withSessionTest "nested list inner length" $ \s -> do
          result <- eval s "builtins.length (builtins.elemAt [[1 2 3] [4 5]] 0)"
          assertInt result 3
      ]

  , testGroup "Nested function calls"
      [ withSessionTest "triple nested function application" $ \s -> do
          result <- eval s "let f = x: x + 1; in f (f (f 0))"
          assertInt result 3

      , withSessionTest "quad nested function application" $ \s -> do
          result <- eval s "let f = x: x * 2; in f (f (f (f 1)))"
          assertInt result 16

      , withSessionTest "nested map calls" $ \s -> do
          result <- eval s "builtins.length (map (x: x) (map (x: x) [1 2 3]))"
          assertInt result 3

      , withSessionTest "deeply nested let bindings" $ \s -> do
          result <- eval s "let a = 1; in let b = a + 1; in let c = b + 1; in let d = c + 1; in d"
          assertInt result 4

      , withSessionTest "nested conditionals" $ \s -> do
          result <- eval s "if true then (if true then (if true then 42 else 0) else 0) else 0"
          assertInt result 42
      ]

  , testGroup "Mixed nesting"
      [ withSessionTest "list of attrsets" $ \s -> do
          result <- eval s "(builtins.elemAt [{ a = 1; } { a = 2; }] 1).a"
          assertInt result 2

      , withSessionTest "attrset of lists" $ \s -> do
          result <- eval s "builtins.elemAt { xs = [10 20 30]; }.xs 2"
          assertInt result 30

      , withSessionTest "deeply mixed nesting" $ \s -> do
          result <- eval s "{ a = [{ b = [{ c = 42; }]; }]; }.a"
          assertListLength result 1
      ]
  ]

-- | Tests for recursive sets
recursiveSetTests :: TestTree
recursiveSetTests = testGroup "Recursive Sets"
  [ testGroup "Self reference"
      [ withSessionTest "simple self reference" $ \s -> do
          result <- eval s "rec { x = 1; y = x + 1; }.y"
          assertInt result 2

      , withSessionTest "chained self reference" $ \s -> do
          result <- eval s "rec { a = 1; b = a + 1; c = b + 1; }.c"
          assertInt result 3

      , withSessionTest "self reference with function" $ \s -> do
          result <- eval s "rec { f = x: x + 1; y = f 10; }.y"
          assertInt result 11

      , withSessionTest "multiple self references" $ \s -> do
          result <- eval s "rec { x = 1; y = x; z = x + y; }.z"
          assertInt result 2
      ]

  , testGroup "Mutual reference"
      [ withSessionTest "mutual reference simple" $ \s -> do
          result <- eval s "rec { x = y + 1; y = 1; }.x"
          assertInt result 2

      , withSessionTest "mutual reference chain" $ \s -> do
          result <- eval s "rec { a = b + 1; b = c + 1; c = 1; }.a"
          assertInt result 3

      , withSessionTest "mutual reference with same base" $ \s -> do
          result <- eval s "rec { base = 10; x = base + y; y = base + 5; }.x"
          assertInt result 25
      ]

  , testGroup "Reference in nested structure"
      [ withSessionTest "reference in nested attrset" $ \s -> do
          result <- eval s "rec { x = { a = y; }; y = 42; }.x.a"
          assertInt result 42

      , withSessionTest "reference in nested list" $ \s -> do
          result <- eval s "builtins.elemAt (rec { x = [y]; y = 99; }.x) 0"
          assertInt result 99

      , withSessionTest "double nested reference" $ \s -> do
          result <- eval s "rec { x = { inner = { deep = y; }; }; y = 123; }.x.inner.deep"
          assertInt result 123

      , withSessionTest "rec with attrNames" $ \s -> do
          result <- eval s "builtins.length (builtins.attrNames (rec { x = 1; y = x; }))"
          assertInt result 2
      ]

  , testGroup "Recursive set with inherit"
      [ withSessionTest "inherit in rec" $ \s -> do
          result <- eval s "let x = 10; in rec { inherit x; y = x + 1; }.y"
          assertInt result 11

      , withSessionTest "inherit from in rec" $ \s -> do
          result <- eval s "let outer = { a = 5; }; in rec { inherit (outer) a; b = a * 2; }.b"
          assertInt result 10
      ]

  , testGroup "Rec with functions"
      [ withSessionTest "recursive function via rec" $ \s -> do
          result <- eval s "rec { factorial = n: if n <= 1 then 1 else n * factorial (n - 1); }.factorial 5"
          assertInt result 120

      , withSessionTest "mutually recursive functions" $ \s -> do
          result <- eval s "rec { isEven = n: if n == 0 then true else isOdd (n - 1); isOdd = n: if n == 0 then false else isEven (n - 1); }.isEven 4"
          assertBoolVal result True
      ]
  ]

-- | Tests for binding patterns in attribute sets.
-- These patterns are valid Nix syntax and are now implemented in the compiler.
supportedBindingTests :: TestTree
supportedBindingTests = testGroup "Binding Patterns"
  [ testGroup "Nested attribute paths"
      [ withSessionTest "nested path in non-rec attrset" $ \s -> do
          -- { a.b.c = 1; } builds nested structure
          result <- eval s "{ a.b.c = 1; }.a.b.c"
          assertInt result 1

      , withSessionTest "nested path with two levels" $ \s -> do
          -- { a.b = 1; } builds nested structure
          result <- eval s "{ a.b = 1; }.a.b"
          assertInt result 1

      , withSessionTest "nested path in rec attrset" $ \s -> do
          -- rec { a.b = 1; } builds nested structure in recursive set
          result <- eval s "rec { a.b = 1; }.a.b"
          assertInt result 1

      , withSessionTest "nested path in let binding" $ \s -> do
          -- let a.b = 1; in a.b is now supported
          result <- eval s "let a.b = 1; in a.b"
          assertInt result 1

      , withSessionTest "multiple nested paths same root" $ \s -> do
          -- { a.b = 1; a.c = 2; } groups under same root
          result <- eval s "{ a.b = 1; a.c = 2; }.a.c"
          assertInt result 2

      , withSessionTest "deeply nested path" $ \s -> do
          -- { a.b.c.d.e = 42; } builds deep structure
          result <- eval s "{ a.b.c.d.e = 42; }.a.b.c.d.e"
          assertInt result 42
      ]

  , testGroup "Inherit in recursive contexts"
      [ withSessionTest "inherit in rec attrset" $ \s -> do
          -- inherit inside rec sets pulls from outer scope
          result <- eval s "let x = 1; in rec { inherit x; }.x"
          assertInt result 1

      , withSessionTest "inherit from expr in rec attrset" $ \s -> do
          -- inherit (expr) inside rec sets selects from the expression
          result <- eval s "rec { inherit ({ a = 1; }) a; }.a"
          assertInt result 1

      , withSessionTest "inherit in rec with self reference" $ \s -> do
          -- Inherited value can be referenced by other attrs
          result <- eval s "let x = 10; in rec { inherit x; y = x + 1; }.y"
          assertInt result 11

      , withSessionTest "inherit from with self reference in rec" $ \s -> do
          result <- eval s "let outer = { a = 5; }; in rec { inherit (outer) a; b = a * 2; }.b"
          assertInt result 10
      ]
  ]

-- | Tests for string edge cases
stringEdgeCaseTests :: TestTree
stringEdgeCaseTests = testGroup "String Edge Cases"
  [ testGroup "Escape sequences"
      [ withSessionTest "newline escape" $ \s -> do
          result <- eval s "builtins.stringLength \"a\\nb\""
          assertInt result 3

      , withSessionTest "tab escape" $ \s -> do
          result <- eval s "builtins.stringLength \"a\\tb\""
          assertInt result 3

      , withSessionTest "carriage return escape" $ \s -> do
          result <- eval s "builtins.stringLength \"a\\rb\""
          assertInt result 3

      , withSessionTest "backslash escape" $ \s -> do
          result <- eval s "builtins.stringLength \"a\\\\b\""
          assertInt result 3

      , withSessionTest "quote escape" $ \s -> do
          result <- eval s "builtins.stringLength \"a\\\"b\""
          assertInt result 3

      , withSessionTest "all escapes together" $ \s -> do
          result <- eval s "\"\\n\\t\\r\\\\\\\"\""
          assertStringVal result "\n\t\r\\\""

      , withSessionTest "dollar escape" $ \s -> do
          result <- eval s "\"\\${x}\""
          assertStringVal result "${x}"
      ]

  , testGroup "Multiline strings"
      [ withSessionTest "simple multiline" $ \s -> do
          result <- eval s "''\n  hello\n  world\n''"
          assertIsString result

      , withSessionTest "multiline strips common indent" $ \s -> do
          -- Nix strips the common leading whitespace
          result <- eval s "''\n    line1\n    line2\n  ''"
          assertIsString result

      , withSessionTest "multiline with interpolation" $ \s -> do
          result <- eval s "let x = \"value\"; in ''\n  hello ${x}\n''"
          assertStringContains result "hello"

      , withSessionTest "multiline empty" $ \s -> do
          result <- eval s "''''"
          assertStringVal result ""

      , withSessionTest "multiline single line" $ \s -> do
          result <- eval s "''hello''"
          assertStringVal result "hello"

      , withSessionTest "multiline escape sequences" $ \s -> do
          -- In multiline, '' followed by $ or ' has special meaning
          result <- eval s "''dollar: ''$''"
          assertStringContains result "dollar: $"
      ]

  , testGroup "String interpolation edge cases"
      [ withSessionTest "empty interpolation result" $ \s -> do
          result <- eval s "let x = \"\"; in \"hello${x}world\""
          assertStringVal result "helloworld"

      , withSessionTest "nested interpolation" $ \s -> do
          result <- eval s "let a = \"inner\"; b = \"has ${a}\"; in \"outer ${b}\""
          assertStringVal result "outer has inner"

      , withSessionTest "interpolation with number" $ \s -> do
          result <- eval s "\"value: ${toString 42}\""
          assertStringVal result "value: 42"

      , withSessionTest "multiple interpolations" $ \s -> do
          result <- eval s "let a = \"x\"; b = \"y\"; c = \"z\"; in \"${a}${b}${c}\""
          assertStringVal result "xyz"

      , withSessionTest "interpolation at start" $ \s -> do
          result <- eval s "let x = \"start\"; in \"${x} end\""
          assertStringVal result "start end"

      , withSessionTest "interpolation at end" $ \s -> do
          result <- eval s "let x = \"end\"; in \"start ${x}\""
          assertStringVal result "start end"

      , withSessionTest "only interpolation" $ \s -> do
          result <- eval s "let x = \"content\"; in \"${x}\""
          assertStringVal result "content"

      , withSessionTest "adjacent interpolations" $ \s -> do
          result <- eval s "let a = \"1\"; b = \"2\"; in \"${a}${b}\""
          assertStringVal result "12"
      ]

  , testGroup "String comparison edge cases"
      [ withSessionTest "empty strings equal" $ \s -> do
          result <- eval s "\"\" == \"\""
          assertBoolVal result True

      , withSessionTest "empty less than any" $ \s -> do
          result <- eval s "\"\" < \"a\""
          assertBoolVal result True

      , withSessionTest "string with spaces" $ \s -> do
          result <- eval s "\"a b\" == \"a b\""
          assertBoolVal result True

      , withSessionTest "string with only spaces" $ \s -> do
          result <- eval s "\"   \" == \"   \""
          assertBoolVal result True

      , withSessionTest "case sensitivity" $ \s -> do
          result <- eval s "\"ABC\" == \"abc\""
          assertBoolVal result False
      ]

  , testGroup "String builtin edge cases"
      [ withSessionTest "substring negative start clamped" $ \s -> do
          -- Nix clamps negative start to 0
          result <- eval s "builtins.substring 0 3 \"hello\""
          assertStringVal result "hel"

      , withSessionTest "split on empty pattern" $ \s -> do
          -- Empty pattern matches between every character
          result <- eval s "builtins.split \"\" \"ab\""
          assertIsListNonEmpty result

      , withSessionTest "replaceStrings empty to list" $ \s -> do
          -- Empty string in from list matches between every character
          result <- eval s "builtins.replaceStrings [\"\"] [\"X\"] \"ab\""
          assertStringVal result "XaXbX"

      , withSessionTest "concatStringsSep empty list" $ \s -> do
          result <- eval s "builtins.concatStringsSep \",\" []"
          assertStringVal result ""

      , withSessionTest "concatStringsSep single" $ \s -> do
          result <- eval s "builtins.concatStringsSep \",\" [\"only\"]"
          assertStringVal result "only"
      ]
  ]

-- | Tests for environment path resolution (<nixpkgs> style paths)
envPathTests :: TestTree
envPathTests = testGroup "Environment Paths"
  [ testGroup "NIX_PATH resolution"
      [ withSessionTest "env path without NIX_PATH fails" $ \s -> do
          -- When NIX_PATH is not configured, <nixpkgs> should fail with EnvPathNotFound
          expectError s "<nixpkgs>"

      , withSessionTest "env path with subpath fails without NIX_PATH" $ \s -> do
          -- <nixpkgs/lib> should also fail
          expectError s "<nixpkgs/lib>"

      , withSessionTest "env path in expression fails without NIX_PATH" $ \s -> do
          -- Environment path used in an expression
          expectError s "let lib = <nixpkgs/lib>; in lib"
      ]
  ]

-- | Tests for path interpolation
-- Path interpolation like ./foo/${bar} should produce a path, not a string.
pathInterpolationTests :: TestTree
pathInterpolationTests = testGroup "Path Interpolation"
  [ testGroup "Basic path interpolation produces path type"
      [ withSessionTest "simple path interpolation returns path" $ \s -> do
          -- ./foo/${"bar"} should produce a path, not a string
          result <- eval s "builtins.typeOf ./foo/${\"bar\"}"
          assertStringVal result "path"

      , withSessionTest "path with variable interpolation returns path" $ \s -> do
          -- let x = "baz"; in ./foo/${x} should produce a path
          result <- eval s "builtins.typeOf (let x = \"baz\"; in ./foo/${x})"
          assertStringVal result "path"

      , withSessionTest "path with multiple interpolations returns path" $ \s -> do
          result <- eval s "builtins.typeOf (let a = \"x\"; b = \"y\"; in ./foo/${a}/${b})"
          assertStringVal result "path"

      , withSessionTest "path interpolation value check" $ \s -> do
          -- Check the actual path value
          result <- eval s "./foo/${\"bar\"}"
          assertIsPath result
      ]

  , testGroup "Path interpolation vs string interpolation"
      [ withSessionTest "string interpolation produces string" $ \s -> do
          -- Regular string interpolation should produce string
          result <- eval s "builtins.typeOf \"foo${\"bar\"}\""
          assertStringVal result "string"

      , withSessionTest "path literal produces path" $ \s -> do
          -- Simple path literal should produce path
          result <- eval s "builtins.typeOf ./foo"
          assertStringVal result "path"
      ]
  ]

-- * Additional assertion helpers

-- | Assert a value is a non-empty list
assertIsListNonEmpty :: NixValue -> Assertion
assertIsListNonEmpty v = case v of
  VList vec -> assertBool "Expected non-empty list" (not $ null vec)
  _ -> assertFailure $ "Expected list, got: " <> show v
