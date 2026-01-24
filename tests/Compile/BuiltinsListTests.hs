-- | Comprehensive tests for Nix list builtins in the compiler.
--
-- This module tests the compiled implementations of list-related builtins
-- including head, tail, filter, map, foldl', concatLists, genList, sort,
-- all, any, partition, elem, listToAttrs, concatMap, groupBy, reverse,
-- and length.
module Compile.BuiltinsListTests (tests) where

import Relude
import Compile.TestCommon

-- | All list builtin tests
tests :: TestTree
tests = testGroup "List Builtins"
  [ testGroup "length"
      [ withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.length []"
          assertInt result 0
      , withSessionTest "single element" $ \s -> do
          result <- eval s "builtins.length [ 1 ]"
          assertInt result 1
      , withSessionTest "multiple elements" $ \s -> do
          result <- eval s "builtins.length [ 1 2 3 4 5 ]"
          assertInt result 5
      , withSessionTest "nested lists count outer" $ \s -> do
          result <- eval s "builtins.length [ [ 1 2 ] [ 3 4 ] ]"
          assertInt result 2
      ]

  , testGroup "head"
      [ withSessionTest "single element" $ \s -> do
          result <- eval s "builtins.head [ 42 ]"
          assertInt result 42
      , withSessionTest "multiple elements" $ \s -> do
          result <- eval s "builtins.head [ 1 2 3 ]"
          assertInt result 1
      , withSessionTest "head of strings" $ \s -> do
          result <- eval s "builtins.head [ \"first\" \"second\" ]"
          assertStringVal result "first"
      , withSessionTest "head of nested list" $ \s -> do
          result <- eval s "builtins.head [ [ 1 2 ] [ 3 4 ] ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            ]
      , withSessionTest "empty list error" $ \s -> do
          expectError s "builtins.head []"
      ]

  , testGroup "tail"
      [ withSessionTest "single element" $ \s -> do
          result <- eval s "builtins.tail [ 1 ]"
          assertListLength result 0
      , withSessionTest "multiple elements" $ \s -> do
          result <- eval s "builtins.tail [ 1 2 3 ]"
          assertList result
            [ \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      , withSessionTest "preserves types" $ \s -> do
          result <- eval s "builtins.tail [ \"a\" \"b\" \"c\" ]"
          assertList result
            [ \v -> assertStringVal v "b"
            , \v -> assertStringVal v "c"
            ]
      , withSessionTest "empty list error" $ \s -> do
          expectError s "builtins.tail []"
      ]

  , testGroup "filter"
      [ withSessionTest "filter none" $ \s -> do
          result <- eval s "builtins.filter (x: false) [ 1 2 3 ]"
          assertListLength result 0
      , withSessionTest "filter all" $ \s -> do
          result <- eval s "builtins.filter (x: true) [ 1 2 3 ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      , withSessionTest "filter even numbers" $ \s -> do
          result <- eval s "builtins.filter (x: x / 2 * 2 == x) [ 1 2 3 4 5 6 ]"
          assertList result
            [ \v -> assertInt v 2
            , \v -> assertInt v 4
            , \v -> assertInt v 6
            ]
      , withSessionTest "filter empty list" $ \s -> do
          result <- eval s "builtins.filter (x: true) []"
          assertListLength result 0
      , withSessionTest "filter with string predicate" $ \s -> do
          result <- eval s "builtins.filter (s: builtins.stringLength s > 2) [ \"a\" \"abc\" \"ab\" \"abcd\" ]"
          assertList result
            [ \v -> assertStringVal v "abc"
            , \v -> assertStringVal v "abcd"
            ]
      ]

  , testGroup "map"
      [ withSessionTest "map identity" $ \s -> do
          result <- eval s "builtins.map (x: x) [ 1 2 3 ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      , withSessionTest "map increment" $ \s -> do
          result <- eval s "builtins.map (x: x + 1) [ 1 2 3 ]"
          assertList result
            [ \v -> assertInt v 2
            , \v -> assertInt v 3
            , \v -> assertInt v 4
            ]
      , withSessionTest "map to different type" $ \s -> do
          result <- eval s "builtins.map (x: x > 2) [ 1 2 3 4 ]"
          assertList result
            [ \v -> assertBoolVal v False
            , \v -> assertBoolVal v False
            , \v -> assertBoolVal v True
            , \v -> assertBoolVal v True
            ]
      , withSessionTest "map empty list" $ \s -> do
          result <- eval s "builtins.map (x: x + 1) []"
          assertListLength result 0
      , withSessionTest "map nested" $ \s -> do
          result <- eval s "builtins.map (l: builtins.head l) [ [ 1 2 ] [ 3 4 ] ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 3
            ]
      ]

  , testGroup "foldl'"
      [ withSessionTest "sum" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: acc + x) 0 [ 1 2 3 4 5 ]"
          assertInt result 15
      , withSessionTest "product" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: acc * x) 1 [ 1 2 3 4 5 ]"
          assertInt result 120
      , withSessionTest "empty list returns initial" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: acc + x) 42 []"
          assertInt result 42
      , withSessionTest "string concatenation" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: acc + x) \"\" [ \"a\" \"b\" \"c\" ]"
          assertStringVal result "abc"
      , withSessionTest "build list in reverse" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: [ x ] ++ acc) [] [ 1 2 3 ]"
          assertList result
            [ \v -> assertInt v 3
            , \v -> assertInt v 2
            , \v -> assertInt v 1
            ]
      , withSessionTest "count elements" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: acc + 1) 0 [ \"a\" \"b\" \"c\" \"d\" ]"
          assertInt result 4
      ]

  , testGroup "concatLists"
      [ withSessionTest "empty list of lists" $ \s -> do
          result <- eval s "builtins.concatLists []"
          assertListLength result 0
      , withSessionTest "single list" $ \s -> do
          result <- eval s "builtins.concatLists [ [ 1 2 3 ] ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      , withSessionTest "multiple lists" $ \s -> do
          result <- eval s "builtins.concatLists [ [ 1 2 ] [ 3 4 ] [ 5 ] ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            , \v -> assertInt v 4
            , \v -> assertInt v 5
            ]
      , withSessionTest "with empty lists" $ \s -> do
          result <- eval s "builtins.concatLists [ [] [ 1 ] [] [ 2 3 ] [] ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      , withSessionTest "all empty" $ \s -> do
          result <- eval s "builtins.concatLists [ [] [] [] ]"
          assertListLength result 0
      ]

  , testGroup "genList"
      [ withSessionTest "zero length" $ \s -> do
          result <- eval s "builtins.genList (x: x) 0"
          assertListLength result 0
      , withSessionTest "identity generator" $ \s -> do
          result <- eval s "builtins.genList (x: x) 5"
          assertList result
            [ \v -> assertInt v 0
            , \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            , \v -> assertInt v 4
            ]
      , withSessionTest "squares" $ \s -> do
          result <- eval s "builtins.genList (x: x * x) 4"
          assertList result
            [ \v -> assertInt v 0
            , \v -> assertInt v 1
            , \v -> assertInt v 4
            , \v -> assertInt v 9
            ]
      , withSessionTest "constant generator" $ \s -> do
          result <- eval s "builtins.genList (x: 42) 3"
          assertList result
            [ \v -> assertInt v 42
            , \v -> assertInt v 42
            , \v -> assertInt v 42
            ]
      , withSessionTest "string generator" $ \s -> do
          result <- eval s "builtins.genList (x: \"item\") 2"
          assertList result
            [ \v -> assertStringVal v "item"
            , \v -> assertStringVal v "item"
            ]
      ]

  , testGroup "sort"
      [ withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.sort (a: b: a < b) []"
          assertListLength result 0
      , withSessionTest "single element" $ \s -> do
          result <- eval s "builtins.sort (a: b: a < b) [ 1 ]"
          assertList result
            [ \v -> assertInt v 1
            ]
      , withSessionTest "ascending integers" $ \s -> do
          result <- eval s "builtins.sort (a: b: a < b) [ 3 1 4 1 5 9 2 6 ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            , \v -> assertInt v 4
            , \v -> assertInt v 5
            , \v -> assertInt v 6
            , \v -> assertInt v 9
            ]
      , withSessionTest "descending integers" $ \s -> do
          result <- eval s "builtins.sort (a: b: a > b) [ 3 1 4 1 5 ]"
          assertList result
            [ \v -> assertInt v 5
            , \v -> assertInt v 4
            , \v -> assertInt v 3
            , \v -> assertInt v 1
            , \v -> assertInt v 1
            ]
      , withSessionTest "sort strings" $ \s -> do
          result <- eval s "builtins.sort (a: b: a < b) [ \"banana\" \"apple\" \"cherry\" ]"
          assertList result
            [ \v -> assertStringVal v "apple"
            , \v -> assertStringVal v "banana"
            , \v -> assertStringVal v "cherry"
            ]
      , withSessionTest "already sorted" $ \s -> do
          result <- eval s "builtins.sort (a: b: a < b) [ 1 2 3 4 5 ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            , \v -> assertInt v 4
            , \v -> assertInt v 5
            ]
      ]

  , testGroup "all"
      [ withSessionTest "all true" $ \s -> do
          result <- eval s "builtins.all (x: x > 0) [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "one false" $ \s -> do
          result <- eval s "builtins.all (x: x > 0) [ 1 0 3 ]"
          assertBoolVal result False
      , withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.all (x: x > 0) []"
          assertBoolVal result True  -- vacuous truth
      , withSessionTest "all with strings" $ \s -> do
          result <- eval s "builtins.all (s: builtins.stringLength s > 0) [ \"a\" \"bc\" \"def\" ]"
          assertBoolVal result True
      , withSessionTest "all false" $ \s -> do
          result <- eval s "builtins.all (x: x > 10) [ 1 2 3 ]"
          assertBoolVal result False
      ]

  , testGroup "any"
      [ withSessionTest "one true" $ \s -> do
          result <- eval s "builtins.any (x: x > 2) [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "none true" $ \s -> do
          result <- eval s "builtins.any (x: x > 10) [ 1 2 3 ]"
          assertBoolVal result False
      , withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.any (x: x > 0) []"
          assertBoolVal result False
      , withSessionTest "all true" $ \s -> do
          result <- eval s "builtins.any (x: x > 0) [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "first element matches" $ \s -> do
          result <- eval s "builtins.any (x: x == 1) [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "last element matches" $ \s -> do
          result <- eval s "builtins.any (x: x == 3) [ 1 2 3 ]"
          assertBoolVal result True
      ]

  , testGroup "partition"
      [ withSessionTest "split even and odd" $ \s -> do
          result <- eval s "builtins.partition (x: x / 2 * 2 == x) [ 1 2 3 4 5 6 ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "right" attrs of
              Just (VList v) -> do
                assertList (VList v)
                  [ \x -> assertInt x 2
                  , \x -> assertInt x 4
                  , \x -> assertInt x 6
                  ]
              Just other -> assertFailure $ "Expected list for 'right', got: " <> show other
              Nothing -> assertFailure "Missing 'right' attribute"
            case lookupAttr "wrong" attrs of
              Just (VList v) -> do
                assertList (VList v)
                  [ \x -> assertInt x 1
                  , \x -> assertInt x 3
                  , \x -> assertInt x 5
                  ]
              Just other -> assertFailure $ "Expected list for 'wrong', got: " <> show other
              Nothing -> assertFailure "Missing 'wrong' attribute"
      , withSessionTest "all match" $ \s -> do
          result <- eval s "builtins.partition (x: true) [ 1 2 3 ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "right" attrs of
              Just v -> assertListLength v 3
              Nothing -> assertFailure "Missing 'right' attribute"
            case lookupAttr "wrong" attrs of
              Just v -> assertListLength v 0
              Nothing -> assertFailure "Missing 'wrong' attribute"
      , withSessionTest "none match" $ \s -> do
          result <- eval s "builtins.partition (x: false) [ 1 2 3 ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "right" attrs of
              Just v -> assertListLength v 0
              Nothing -> assertFailure "Missing 'right' attribute"
            case lookupAttr "wrong" attrs of
              Just v -> assertListLength v 3
              Nothing -> assertFailure "Missing 'wrong' attribute"
      , withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.partition (x: true) []"
          assertAttrs result $ \attrs -> do
            case lookupAttr "right" attrs of
              Just v -> assertListLength v 0
              Nothing -> assertFailure "Missing 'right' attribute"
            case lookupAttr "wrong" attrs of
              Just v -> assertListLength v 0
              Nothing -> assertFailure "Missing 'wrong' attribute"
      ]

  , testGroup "elem"
      [ withSessionTest "found at start" $ \s -> do
          result <- eval s "builtins.elem 1 [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "found in middle" $ \s -> do
          result <- eval s "builtins.elem 2 [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "found at end" $ \s -> do
          result <- eval s "builtins.elem 3 [ 1 2 3 ]"
          assertBoolVal result True
      , withSessionTest "not found" $ \s -> do
          result <- eval s "builtins.elem 4 [ 1 2 3 ]"
          assertBoolVal result False
      , withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.elem 1 []"
          assertBoolVal result False
      , withSessionTest "string element" $ \s -> do
          result <- eval s "builtins.elem \"b\" [ \"a\" \"b\" \"c\" ]"
          assertBoolVal result True
      , withSessionTest "string not found" $ \s -> do
          result <- eval s "builtins.elem \"d\" [ \"a\" \"b\" \"c\" ]"
          assertBoolVal result False
      , withSessionTest "null in list" $ \s -> do
          result <- eval s "builtins.elem null [ 1 null 3 ]"
          assertBoolVal result True
      , withSessionTest "bool in list" $ \s -> do
          result <- eval s "builtins.elem true [ false true false ]"
          assertBoolVal result True
      ]

  , testGroup "listToAttrs"
      [ withSessionTest "single element" $ \s -> do
          result <- eval s "builtins.listToAttrs [ { name = \"x\"; value = 1; } ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "x" attrs of
              Just v -> assertInt v 1
              Nothing -> assertFailure "Missing 'x' attribute"
      , withSessionTest "multiple elements" $ \s -> do
          result <- eval s "builtins.listToAttrs [ { name = \"a\"; value = 1; } { name = \"b\"; value = 2; } ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "a" attrs of
              Just v -> assertInt v 1
              Nothing -> assertFailure "Missing 'a' attribute"
            case lookupAttr "b" attrs of
              Just v -> assertInt v 2
              Nothing -> assertFailure "Missing 'b' attribute"
      , withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.listToAttrs []"
          assertAttrsSize result 0
      , withSessionTest "duplicate names (first wins)" $ \s -> do
          -- In Nix, first occurrence wins for listToAttrs
          result <- eval s "builtins.listToAttrs [ { name = \"x\"; value = 1; } { name = \"x\"; value = 2; } ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "x" attrs of
              Just v -> assertInt v 1  -- First value wins
              Nothing -> assertFailure "Missing 'x' attribute"
      , withSessionTest "mixed value types" $ \s -> do
          result <- eval s "builtins.listToAttrs [ { name = \"int\"; value = 42; } { name = \"str\"; value = \"hello\"; } ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "int" attrs of
              Just v -> assertInt v 42
              Nothing -> assertFailure "Missing 'int' attribute"
            case lookupAttr "str" attrs of
              Just v -> assertStringVal v "hello"
              Nothing -> assertFailure "Missing 'str' attribute"
      ]

  , testGroup "concatMap"
      [ withSessionTest "identity concat" $ \s -> do
          result <- eval s "builtins.concatMap (x: [ x ]) [ 1 2 3 ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      , withSessionTest "duplicate each" $ \s -> do
          result <- eval s "builtins.concatMap (x: [ x x ]) [ 1 2 ]"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 2
            ]
      , withSessionTest "filter via empty" $ \s -> do
          result <- eval s "builtins.concatMap (x: if x > 2 then [ x ] else []) [ 1 2 3 4 ]"
          assertList result
            [ \v -> assertInt v 3
            , \v -> assertInt v 4
            ]
      , withSessionTest "empty input" $ \s -> do
          result <- eval s "builtins.concatMap (x: [ x ]) []"
          assertListLength result 0
      , withSessionTest "all empty results" $ \s -> do
          result <- eval s "builtins.concatMap (x: []) [ 1 2 3 ]"
          assertListLength result 0
      , withSessionTest "varying lengths" $ \s -> do
          result <- eval s "builtins.concatMap (x: builtins.genList (i: x) x) [ 1 2 3 ]"
          -- 1 generates [1], 2 generates [2 2], 3 generates [3 3 3]
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            , \v -> assertInt v 3
            , \v -> assertInt v 3
            ]
      ]

  , testGroup "groupBy"
      [ withSessionTest "group by value" $ \s -> do
          result <- eval s "builtins.groupBy (x: x) [ \"a\" \"b\" \"a\" ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "a" attrs of
              Just v -> assertList v
                [ \x -> assertStringVal x "a"
                , \x -> assertStringVal x "a"
                ]
              Nothing -> assertFailure "Missing 'a' attribute"
            case lookupAttr "b" attrs of
              Just v -> assertList v
                [ \x -> assertStringVal x "b"
                ]
              Nothing -> assertFailure "Missing 'b' attribute"
      , withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.groupBy (x: x) []"
          assertAttrsSize result 0
      , withSessionTest "single group" $ \s -> do
          result <- eval s "builtins.groupBy (x: \"same\") [ 1 2 3 ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "same" attrs of
              Just v -> assertListLength v 3
              Nothing -> assertFailure "Missing 'same' attribute"
      , withSessionTest "group by length" $ \s -> do
          result <- eval s "builtins.groupBy (s: builtins.toString (builtins.stringLength s)) [ \"a\" \"bb\" \"c\" \"dd\" ]"
          assertAttrs result $ \attrs -> do
            case lookupAttr "1" attrs of
              Just v -> assertList v
                [ \x -> assertStringVal x "a"
                , \x -> assertStringVal x "c"
                ]
              Nothing -> assertFailure "Missing '1' attribute"
            case lookupAttr "2" attrs of
              Just v -> assertList v
                [ \x -> assertStringVal x "bb"
                , \x -> assertStringVal x "dd"
                ]
              Nothing -> assertFailure "Missing '2' attribute"
      ]

  , testGroup "reverse"
      [ withSessionTest "empty list" $ \s -> do
          result <- eval s "builtins.reverse []"
          assertListLength result 0
      , withSessionTest "single element" $ \s -> do
          result <- eval s "builtins.reverse [ 1 ]"
          assertList result
            [ \v -> assertInt v 1
            ]
      , withSessionTest "multiple elements" $ \s -> do
          result <- eval s "builtins.reverse [ 1 2 3 4 5 ]"
          assertList result
            [ \v -> assertInt v 5
            , \v -> assertInt v 4
            , \v -> assertInt v 3
            , \v -> assertInt v 2
            , \v -> assertInt v 1
            ]
      , withSessionTest "reverse strings" $ \s -> do
          result <- eval s "builtins.reverse [ \"a\" \"b\" \"c\" ]"
          assertList result
            [ \v -> assertStringVal v "c"
            , \v -> assertStringVal v "b"
            , \v -> assertStringVal v "a"
            ]
      , withSessionTest "double reverse is identity" $ \s -> do
          result <- eval s "builtins.reverse (builtins.reverse [ 1 2 3 ])"
          assertList result
            [ \v -> assertInt v 1
            , \v -> assertInt v 2
            , \v -> assertInt v 3
            ]
      ]

  , testGroup "elemAt"
      [ withSessionTest "first element" $ \s -> do
          result <- eval s "builtins.elemAt [ 10 20 30 ] 0"
          assertInt result 10
      , withSessionTest "middle element" $ \s -> do
          result <- eval s "builtins.elemAt [ 10 20 30 ] 1"
          assertInt result 20
      , withSessionTest "last element" $ \s -> do
          result <- eval s "builtins.elemAt [ 10 20 30 ] 2"
          assertInt result 30
      , withSessionTest "negative index error" $ \s -> do
          expectError s "builtins.elemAt [ 1 2 3 ] (-1)"
      , withSessionTest "index out of bounds error" $ \s -> do
          expectError s "builtins.elemAt [ 1 2 3 ] 3"
      , withSessionTest "empty list error" $ \s -> do
          expectError s "builtins.elemAt [] 0"
      ]

  , testGroup "combinations"
      [ withSessionTest "map then filter" $ \s -> do
          result <- eval s "builtins.filter (x: x > 2) (builtins.map (x: x * 2) [ 1 2 3 ])"
          -- [1 2 3] -> [2 4 6] -> [4 6]
          assertList result
            [ \v -> assertInt v 4
            , \v -> assertInt v 6
            ]
      , withSessionTest "filter then map" $ \s -> do
          result <- eval s "builtins.map (x: x * 2) (builtins.filter (x: x > 1) [ 1 2 3 ])"
          -- [1 2 3] -> [2 3] -> [4 6]
          assertList result
            [ \v -> assertInt v 4
            , \v -> assertInt v 6
            ]
      , withSessionTest "sort then head" $ \s -> do
          result <- eval s "builtins.head (builtins.sort (a: b: a < b) [ 3 1 2 ])"
          assertInt result 1
      , withSessionTest "genList then foldl'" $ \s -> do
          result <- eval s "builtins.foldl' (acc: x: acc + x) 0 (builtins.genList (x: x + 1) 5)"
          -- genList produces [1 2 3 4 5], sum is 15
          assertInt result 15
      , withSessionTest "concatLists then length" $ \s -> do
          result <- eval s "builtins.length (builtins.concatLists [ [ 1 2 ] [ 3 ] [ 4 5 6 ] ])"
          assertInt result 6
      , withSessionTest "partition then map both" $ \s -> do
          result <- eval s "let p = builtins.partition (x: x > 5) [ 1 6 3 8 2 9 ]; in builtins.length p.right + builtins.length p.wrong"
          -- right: [6 8 9] (3 elements), wrong: [1 3 2] (3 elements)
          assertInt result 6
      ]
  ]
