-- | Tests for attrset builtins in the Nix compiler.
--
-- These tests verify that attrset builtins are correctly compiled and
-- evaluated by the runtime.
module Compile.BuiltinsAttrSetTests (tests) where

import Relude
import Compile.TestCommon

-- | All attrset builtin tests
tests :: TestTree
tests = testGroup "Builtins.AttrSet"
  [ attrNamesTests
  , attrValuesTests
  , hasAttrTests
  , getAttrTests
  , removeAttrsTests
  , mapAttrsTests
  , intersectAttrsTests
  , catAttrsTests
  , functionArgsTests
  , zipAttrsWithTests
  , listToAttrsTests
  , genAttrsTests
  ]

-- | Tests for builtins.attrNames
attrNamesTests :: TestTree
attrNamesTests = testGroup "attrNames"
  [ withSessionTest "returns sorted names" $ \s -> do
      result <- eval s "builtins.attrNames { b = 1; a = 2; c = 3; }"
      -- attrNames returns sorted names
      assertList result [ \v -> assertStringVal v "a"
                        , \v -> assertStringVal v "b"
                        , \v -> assertStringVal v "c" ]

  , withSessionTest "empty set" $ \s -> do
      result <- eval s "builtins.attrNames { }"
      assertListLength result 0

  , withSessionTest "single attribute" $ \s -> do
      result <- eval s "builtins.attrNames { foo = 1; }"
      assertList result [ \v -> assertStringVal v "foo" ]

  , withSessionTest "nested set (only top-level names)" $ \s -> do
      result <- eval s "builtins.attrNames { a = { x = 1; }; b = 2; }"
      assertList result [ \v -> assertStringVal v "a"
                        , \v -> assertStringVal v "b" ]

  , withSessionTest "names with special characters" $ \s -> do
      result <- eval s "builtins.attrNames { \"foo-bar\" = 1; \"baz_qux\" = 2; }"
      assertList result [ \v -> assertStringVal v "baz_qux"
                        , \v -> assertStringVal v "foo-bar" ]
  ]

-- | Tests for builtins.attrValues
attrValuesTests :: TestTree
attrValuesTests = testGroup "attrValues"
  [ withSessionTest "returns values in name-sorted order" $ \s -> do
      -- Values should be in the same order as attrNames would give
      result <- eval s "builtins.attrValues { b = 2; a = 1; c = 3; }"
      assertList result [ \v -> assertInt v 1  -- a
                        , \v -> assertInt v 2  -- b
                        , \v -> assertInt v 3  -- c
                        ]

  , withSessionTest "empty set" $ \s -> do
      result <- eval s "builtins.attrValues { }"
      assertListLength result 0

  , withSessionTest "single attribute" $ \s -> do
      result <- eval s "builtins.attrValues { x = 42; }"
      assertList result [ \v -> assertInt v 42 ]

  , withSessionTest "mixed value types" $ \s -> do
      result <- eval s "builtins.attrValues { a = 1; b = \"two\"; c = true; }"
      assertList result [ \v -> assertInt v 1
                        , \v -> assertStringVal v "two"
                        , \v -> assertBoolVal v True ]
  ]

-- | Tests for builtins.hasAttr (function form, not ? operator)
hasAttrTests :: TestTree
hasAttrTests = testGroup "hasAttr"
  [ withSessionTest "attribute exists" $ \s -> do
      result <- eval s "builtins.hasAttr \"a\" { a = 1; b = 2; }"
      assertBoolVal result True

  , withSessionTest "attribute missing" $ \s -> do
      result <- eval s "builtins.hasAttr \"c\" { a = 1; b = 2; }"
      assertBoolVal result False

  , withSessionTest "empty set" $ \s -> do
      result <- eval s "builtins.hasAttr \"x\" { }"
      assertBoolVal result False

  , withSessionTest "nested set (only checks top level)" $ \s -> do
      result <- eval s "builtins.hasAttr \"x\" { a = { x = 1; }; }"
      assertBoolVal result False

  , withSessionTest "dynamic name" $ \s -> do
      result <- eval s "let name = \"foo\"; in builtins.hasAttr name { foo = 1; }"
      assertBoolVal result True

  , withSessionTest "quoted attribute name" $ \s -> do
      result <- eval s "builtins.hasAttr \"foo-bar\" { \"foo-bar\" = 1; }"
      assertBoolVal result True
  ]

-- | Tests for builtins.getAttr
getAttrTests :: TestTree
getAttrTests = testGroup "getAttr"
  [ withSessionTest "get existing attribute" $ \s -> do
      result <- eval s "builtins.getAttr \"a\" { a = 42; b = 1; }"
      assertInt result 42

  , withSessionTest "dynamic name" $ \s -> do
      result <- eval s "let name = \"x\"; in builtins.getAttr name { x = 100; }"
      assertInt result 100

  , withSessionTest "nested value" $ \s -> do
      result <- eval s "builtins.getAttr \"a\" { a = { b = 1; }; }"
      assertAttrsSize result 1

  , withSessionTest "missing attribute throws" $ \s -> do
      expectError s "builtins.getAttr \"missing\" { a = 1; }"

  , withSessionTest "quoted attribute name" $ \s -> do
      result <- eval s "builtins.getAttr \"foo-bar\" { \"foo-bar\" = 99; }"
      assertInt result 99
  ]

-- | Tests for builtins.removeAttrs
removeAttrsTests :: TestTree
removeAttrsTests = testGroup "removeAttrs"
  [ withSessionTest "remove single attr" $ \s -> do
      result <- eval s "builtins.removeAttrs { a = 1; b = 2; c = 3; } [ \"b\" ]"
      assertAttrsSize result 2

  , withSessionTest "remove multiple attrs" $ \s -> do
      result <- eval s "builtins.removeAttrs { a = 1; b = 2; c = 3; } [ \"a\" \"c\" ]"
      assertAttrs result $ \as -> do
        case lookupAttr "b" as of
          Just v -> assertInt v 2
          Nothing -> assertFailure "Expected 'b' to remain"
        case lookupAttr "a" as of
          Just _ -> assertFailure "Expected 'a' to be removed"
          Nothing -> pure ()

  , withSessionTest "remove non-existent attr" $ \s -> do
      result <- eval s "builtins.removeAttrs { a = 1; } [ \"x\" ]"
      assertAttrsSize result 1

  , withSessionTest "remove all attrs" $ \s -> do
      result <- eval s "builtins.removeAttrs { a = 1; b = 2; } [ \"a\" \"b\" ]"
      assertAttrsSize result 0

  , withSessionTest "empty removal list" $ \s -> do
      result <- eval s "builtins.removeAttrs { a = 1; b = 2; } [ ]"
      assertAttrsSize result 2

  , withSessionTest "from empty set" $ \s -> do
      result <- eval s "builtins.removeAttrs { } [ \"a\" ]"
      assertAttrsSize result 0
  ]

-- | Tests for builtins.mapAttrs
mapAttrsTests :: TestTree
mapAttrsTests = testGroup "mapAttrs"
  [ withSessionTest "transform values" $ \s -> do
      result <- eval s "builtins.mapAttrs (name: value: value * 2) { a = 1; b = 2; }"
      assertAttrs result $ \as -> do
        case lookupAttr "a" as of
          Just v -> assertInt v 2
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertInt v 4
          Nothing -> assertFailure "Expected 'b'"

  , withSessionTest "use name in transformation" $ \s -> do
      result <- eval s "builtins.mapAttrs (name: value: name) { foo = 1; bar = 2; }"
      assertAttrs result $ \as -> do
        case lookupAttr "foo" as of
          Just v -> assertStringVal v "foo"
          Nothing -> assertFailure "Expected 'foo'"
        case lookupAttr "bar" as of
          Just v -> assertStringVal v "bar"
          Nothing -> assertFailure "Expected 'bar'"

  , withSessionTest "empty set" $ \s -> do
      result <- eval s "builtins.mapAttrs (n: v: v) { }"
      assertAttrsSize result 0

  , withSessionTest "change value types" $ \s -> do
      result <- eval s "builtins.mapAttrs (n: v: builtins.toString v) { a = 1; b = 2; }"
      assertAttrs result $ \as -> do
        case lookupAttr "a" as of
          Just v -> assertStringVal v "1"
          Nothing -> assertFailure "Expected 'a'"

  , withSessionTest "nested application" $ \s -> do
      result <- eval s "builtins.mapAttrs (n: v: v + 1) (builtins.mapAttrs (n: v: v * 2) { x = 3; })"
      assertAttrs result $ \as -> do
        case lookupAttr "x" as of
          Just v -> assertInt v 7  -- (3 * 2) + 1
          Nothing -> assertFailure "Expected 'x'"
  ]

-- | Tests for builtins.intersectAttrs
intersectAttrsTests :: TestTree
intersectAttrsTests = testGroup "intersectAttrs"
  [ withSessionTest "basic intersection" $ \s -> do
      result <- eval s "builtins.intersectAttrs { a = 1; b = 2; } { a = 10; c = 30; }"
      assertAttrs result $ \as -> do
        attrsSize as @?= 1
        case lookupAttr "a" as of
          Just v -> assertInt v 10  -- Value from second set
          Nothing -> assertFailure "Expected 'a'"

  , withSessionTest "no overlap" $ \s -> do
      result <- eval s "builtins.intersectAttrs { a = 1; } { b = 2; }"
      assertAttrsSize result 0

  , withSessionTest "full overlap" $ \s -> do
      result <- eval s "builtins.intersectAttrs { a = 1; b = 2; } { a = 10; b = 20; }"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
        case lookupAttr "a" as of
          Just v -> assertInt v 10
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertInt v 20
          Nothing -> assertFailure "Expected 'b'"

  , withSessionTest "empty first set" $ \s -> do
      result <- eval s "builtins.intersectAttrs { } { a = 1; }"
      assertAttrsSize result 0

  , withSessionTest "empty second set" $ \s -> do
      result <- eval s "builtins.intersectAttrs { a = 1; } { }"
      assertAttrsSize result 0

  , withSessionTest "values from second set" $ \s -> do
      -- Verify that the VALUES come from the second set
      result <- eval s "builtins.intersectAttrs { x = \"first\"; } { x = \"second\"; }"
      assertAttrs result $ \as -> do
        case lookupAttr "x" as of
          Just v -> assertStringVal v "second"
          Nothing -> assertFailure "Expected 'x'"
  ]

-- | Tests for builtins.catAttrs
catAttrsTests :: TestTree
catAttrsTests = testGroup "catAttrs"
  [ withSessionTest "extract attribute from list of sets" $ \s -> do
      result <- eval s "builtins.catAttrs \"a\" [ { a = 1; } { a = 2; } { a = 3; } ]"
      assertList result [ \v -> assertInt v 1
                        , \v -> assertInt v 2
                        , \v -> assertInt v 3 ]

  , withSessionTest "skip sets without attribute" $ \s -> do
      result <- eval s "builtins.catAttrs \"a\" [ { a = 1; } { b = 2; } { a = 3; } ]"
      assertList result [ \v -> assertInt v 1
                        , \v -> assertInt v 3 ]

  , withSessionTest "empty list" $ \s -> do
      result <- eval s "builtins.catAttrs \"x\" [ ]"
      assertListLength result 0

  , withSessionTest "no sets have attribute" $ \s -> do
      result <- eval s "builtins.catAttrs \"x\" [ { a = 1; } { b = 2; } ]"
      assertListLength result 0

  , withSessionTest "all sets have attribute" $ \s -> do
      result <- eval s "builtins.catAttrs \"val\" [ { val = \"a\"; } { val = \"b\"; } ]"
      assertList result [ \v -> assertStringVal v "a"
                        , \v -> assertStringVal v "b" ]

  , withSessionTest "mixed value types" $ \s -> do
      result <- eval s "builtins.catAttrs \"x\" [ { x = 1; } { x = \"two\"; } { x = true; } ]"
      assertList result [ \v -> assertInt v 1
                        , \v -> assertStringVal v "two"
                        , \v -> assertBoolVal v True ]
  ]

-- | Tests for builtins.functionArgs
functionArgsTests :: TestTree
functionArgsTests = testGroup "functionArgs"
  [ withSessionTest "simple function" $ \s -> do
      result <- eval s "builtins.functionArgs ({ a, b }: a + b)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
        case lookupAttr "a" as of
          Just v -> assertBoolVal v False  -- No default
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertBoolVal v False  -- No default
          Nothing -> assertFailure "Expected 'b'"

  , withSessionTest "with defaults" $ \s -> do
      result <- eval s "builtins.functionArgs ({ a ? 1, b }: a + b)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
        case lookupAttr "a" as of
          Just v -> assertBoolVal v True  -- Has default
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertBoolVal v False  -- No default
          Nothing -> assertFailure "Expected 'b'"

  , withSessionTest "all with defaults" $ \s -> do
      result <- eval s "builtins.functionArgs ({ x ? 1, y ? 2, z ? 3 }: x)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 3
        case lookupAttr "x" as of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "Expected 'x'"
        case lookupAttr "y" as of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "Expected 'y'"
        case lookupAttr "z" as of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "Expected 'z'"

  , withSessionTest "with ellipsis" $ \s -> do
      result <- eval s "builtins.functionArgs ({ a, ... }: a)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 1
        case lookupAttr "a" as of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "Expected 'a'"

  , withSessionTest "simple lambda returns empty" $ \s -> do
      result <- eval s "builtins.functionArgs (x: x)"
      assertAttrsSize result 0

  , withSessionTest "named pattern" $ \s -> do
      result <- eval s "builtins.functionArgs (args@{ a, b }: args)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
  ]

-- | Tests for builtins.zipAttrsWith
zipAttrsWithTests :: TestTree
zipAttrsWithTests = testGroup "zipAttrsWith"
  [ withSessionTest "combine values with same name" $ \s -> do
      result <- eval s "builtins.zipAttrsWith (name: values: builtins.head values) [ { a = 1; } { a = 2; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "a" as of
          Just v -> assertInt v 1
          Nothing -> assertFailure "Expected 'a'"

  , withSessionTest "sum values" $ \s -> do
      result <- eval s "builtins.zipAttrsWith (name: values: builtins.foldl' (a: b: a + b) 0 values) [ { x = 1; } { x = 2; } { x = 3; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "x" as of
          Just v -> assertInt v 6
          Nothing -> assertFailure "Expected 'x'"

  , withSessionTest "different attributes" $ \s -> do
      result <- eval s "builtins.zipAttrsWith (n: vs: builtins.head vs) [ { a = 1; } { b = 2; } ]"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
        case lookupAttr "a" as of
          Just v -> assertInt v 1
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertInt v 2
          Nothing -> assertFailure "Expected 'b'"

  , withSessionTest "use name in function" $ \s -> do
      result <- eval s "builtins.zipAttrsWith (name: values: name) [ { foo = 1; } { foo = 2; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "foo" as of
          Just v -> assertStringVal v "foo"
          Nothing -> assertFailure "Expected 'foo'"

  , withSessionTest "empty list" $ \s -> do
      result <- eval s "builtins.zipAttrsWith (n: vs: vs) [ ]"
      assertAttrsSize result 0

  , withSessionTest "count occurrences" $ \s -> do
      result <- eval s "builtins.zipAttrsWith (n: vs: builtins.length vs) [ { a = 1; } { a = 2; b = 3; } { a = 4; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "a" as of
          Just v -> assertInt v 3  -- 'a' appears 3 times
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertInt v 1  -- 'b' appears 1 time
          Nothing -> assertFailure "Expected 'b'"
  ]

-- | Tests for builtins.listToAttrs
listToAttrsTests :: TestTree
listToAttrsTests = testGroup "listToAttrs"
  [ withSessionTest "basic conversion" $ \s -> do
      result <- eval s "builtins.listToAttrs [ { name = \"a\"; value = 1; } { name = \"b\"; value = 2; } ]"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
        case lookupAttr "a" as of
          Just v -> assertInt v 1
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertInt v 2
          Nothing -> assertFailure "Expected 'b'"

  , withSessionTest "empty list" $ \s -> do
      result <- eval s "builtins.listToAttrs [ ]"
      assertAttrsSize result 0

  , withSessionTest "single element" $ \s -> do
      result <- eval s "builtins.listToAttrs [ { name = \"x\"; value = 42; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "x" as of
          Just v -> assertInt v 42
          Nothing -> assertFailure "Expected 'x'"

  , withSessionTest "duplicate names (first wins)" $ \s -> do
      result <- eval s "builtins.listToAttrs [ { name = \"x\"; value = 1; } { name = \"x\"; value = 2; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "x" as of
          Just v -> assertInt v 1  -- First occurrence wins
          Nothing -> assertFailure "Expected 'x'"

  , withSessionTest "string values" $ \s -> do
      result <- eval s "builtins.listToAttrs [ { name = \"greeting\"; value = \"hello\"; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "greeting" as of
          Just v -> assertStringVal v "hello"
          Nothing -> assertFailure "Expected 'greeting'"

  , withSessionTest "nested value" $ \s -> do
      result <- eval s "builtins.listToAttrs [ { name = \"nested\"; value = { inner = 1; }; } ]"
      assertAttrs result $ \as -> do
        case lookupAttr "nested" as of
          Just v -> assertAttrsSize v 1
          Nothing -> assertFailure "Expected 'nested'"
  ]

-- | Tests for builtins.genAttrs
genAttrsTests :: TestTree
genAttrsTests = testGroup "genAttrs"
  [ withSessionTest "basic generation" $ \s -> do
      result <- eval s "builtins.genAttrs [ \"a\" \"b\" \"c\" ] (name: name)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 3
        case lookupAttr "a" as of
          Just v -> assertStringVal v "a"
          Nothing -> assertFailure "Expected 'a'"
        case lookupAttr "b" as of
          Just v -> assertStringVal v "b"
          Nothing -> assertFailure "Expected 'b'"
        case lookupAttr "c" as of
          Just v -> assertStringVal v "c"
          Nothing -> assertFailure "Expected 'c'"

  , withSessionTest "compute values" $ \s -> do
      result <- eval s "builtins.genAttrs [ \"x\" \"y\" ] (n: builtins.stringLength n)"
      assertAttrs result $ \as -> do
        case lookupAttr "x" as of
          Just v -> assertInt v 1
          Nothing -> assertFailure "Expected 'x'"
        case lookupAttr "y" as of
          Just v -> assertInt v 1
          Nothing -> assertFailure "Expected 'y'"

  , withSessionTest "empty list" $ \s -> do
      result <- eval s "builtins.genAttrs [ ] (n: n)"
      assertAttrsSize result 0

  , withSessionTest "single name" $ \s -> do
      result <- eval s "builtins.genAttrs [ \"solo\" ] (n: 42)"
      assertAttrs result $ \as -> do
        case lookupAttr "solo" as of
          Just v -> assertInt v 42
          Nothing -> assertFailure "Expected 'solo'"

  , withSessionTest "complex value function" $ \s -> do
      result <- eval s "builtins.genAttrs [ \"first\" \"second\" ] (n: { inherit n; len = builtins.stringLength n; })"
      assertAttrs result $ \as -> do
        case lookupAttr "first" as of
          Just v -> assertAttrsSize v 2
          Nothing -> assertFailure "Expected 'first'"

  , withSessionTest "special characters in names" $ \s -> do
      result <- eval s "builtins.genAttrs [ \"foo-bar\" \"baz_qux\" ] (n: n)"
      assertAttrs result $ \as -> do
        attrsSize as @?= 2
        case lookupAttr "foo-bar" as of
          Just v -> assertStringVal v "foo-bar"
          Nothing -> assertFailure "Expected 'foo-bar'"
        case lookupAttr "baz_qux" as of
          Just v -> assertStringVal v "baz_qux"
          Nothing -> assertFailure "Expected 'baz_qux'"
  ]
