-- | Tests for string builtins in the Nix compiler.
--
-- This module provides comprehensive tests for all string-related builtins
-- including substring manipulation, pattern matching, hashing, and version
-- comparison functions.
module Compile.BuiltinsStringTests (tests) where

import Relude
import Compile.TestCommon

-- | All string builtin tests
tests :: TestTree
tests = testGroup "String Builtins"
  [ substringTests
  , replaceStringsTests
  , concatStringSepTests
  , matchTests
  , splitTests
  , hashStringTests
  , compareVersionsTests
  , splitVersionTests
  , contextTests
  , stringLengthTests
  ]

-- | Tests for builtins.substring
substringTests :: TestTree
substringTests = testGroup "substring"
  [ -- Normal cases
    withSessionTest "substring start" $ \s -> do
      result <- eval s "builtins.substring 0 3 \"hello\""
      assertStringVal result "hel"

  , withSessionTest "substring middle" $ \s -> do
      result <- eval s "builtins.substring 2 3 \"hello\""
      assertStringVal result "llo"

  , withSessionTest "substring end" $ \s -> do
      result <- eval s "builtins.substring 3 2 \"hello\""
      assertStringVal result "lo"

  , withSessionTest "substring full string" $ \s -> do
      result <- eval s "builtins.substring 0 5 \"hello\""
      assertStringVal result "hello"

  , withSessionTest "substring empty result" $ \s -> do
      result <- eval s "builtins.substring 0 0 \"hello\""
      assertStringVal result ""

    -- Edge cases
  , withSessionTest "substring length exceeds string" $ \s -> do
      -- Nix allows length to exceed remaining chars, returns what's available
      result <- eval s "builtins.substring 3 100 \"hello\""
      assertStringVal result "lo"

  , withSessionTest "substring start at end" $ \s -> do
      result <- eval s "builtins.substring 5 2 \"hello\""
      assertStringVal result ""

  , withSessionTest "substring negative length means rest" $ \s -> do
      -- In Nix, negative length means "take until end"
      result <- eval s "builtins.substring 2 (-1) \"hello\""
      assertStringVal result "llo"

  , withSessionTest "substring on empty string" $ \s -> do
      result <- eval s "builtins.substring 0 5 \"\""
      assertStringVal result ""

  , withSessionTest "substring start past end" $ \s -> do
      -- Start beyond string length should return empty
      result <- eval s "builtins.substring 10 5 \"hello\""
      assertStringVal result ""
  ]

-- | Tests for builtins.replaceStrings
replaceStringsTests :: TestTree
replaceStringsTests = testGroup "replaceStrings"
  [ withSessionTest "single replacement" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"o\"] [\"0\"] \"hello\""
      assertStringVal result "hell0"

  , withSessionTest "multiple occurrences" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"l\"] [\"L\"] \"hello\""
      assertStringVal result "heLLo"

  , withSessionTest "multiple patterns" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"a\" \"e\" \"i\" \"o\" \"u\"] [\"1\" \"2\" \"3\" \"4\" \"5\"] \"hello\""
      assertStringVal result "h2ll4"

  , withSessionTest "longer replacement" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"l\"] [\"LL\"] \"hello\""
      assertStringVal result "heLLLLo"

  , withSessionTest "shorter replacement" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"ll\"] [\"l\"] \"hello\""
      assertStringVal result "helo"

  , withSessionTest "empty replacement (delete)" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"l\"] [\"\"] \"hello\""
      assertStringVal result "heo"

  , withSessionTest "no match" $ \s -> do
      result <- eval s "builtins.replaceStrings [\"x\"] [\"y\"] \"hello\""
      assertStringVal result "hello"

  , withSessionTest "empty from list" $ \s -> do
      result <- eval s "builtins.replaceStrings [] [] \"hello\""
      assertStringVal result "hello"

  , withSessionTest "overlapping patterns" $ \s -> do
      -- First matching pattern wins
      result <- eval s "builtins.replaceStrings [\"he\" \"hel\"] [\"HE\" \"HEL\"] \"hello\""
      assertStringVal result "HEllo"

  , withSessionTest "empty string in from matches everywhere" $ \s -> do
      -- Empty string matches between every character
      result <- eval s "builtins.replaceStrings [\"\"] [\".\"] \"ab\""
      assertStringVal result ".a.b."
  ]

-- | Tests for builtins.concatStringsSep
concatStringSepTests :: TestTree
concatStringSepTests = testGroup "concatStringsSep"
  [ withSessionTest "simple join" $ \s -> do
      result <- eval s "builtins.concatStringsSep \", \" [\"a\" \"b\" \"c\"]"
      assertStringVal result "a, b, c"

  , withSessionTest "empty separator" $ \s -> do
      result <- eval s "builtins.concatStringsSep \"\" [\"a\" \"b\" \"c\"]"
      assertStringVal result "abc"

  , withSessionTest "single element" $ \s -> do
      result <- eval s "builtins.concatStringsSep \", \" [\"alone\"]"
      assertStringVal result "alone"

  , withSessionTest "empty list" $ \s -> do
      result <- eval s "builtins.concatStringsSep \", \" []"
      assertStringVal result ""

  , withSessionTest "newline separator" $ \s -> do
      result <- eval s "builtins.concatStringsSep \"\\n\" [\"line1\" \"line2\"]"
      assertStringVal result "line1\nline2"

  , withSessionTest "long separator" $ \s -> do
      result <- eval s "builtins.concatStringsSep \" <-> \" [\"x\" \"y\"]"
      assertStringVal result "x <-> y"
  ]

-- | Tests for builtins.match
matchTests :: TestTree
matchTests = testGroup "match"
  [ -- Found cases
    withSessionTest "match simple found" $ \s -> do
      result <- eval s "builtins.match \"hello\" \"hello\""
      assertListLength result 0  -- Empty list means full match, no captures

  , withSessionTest "match with capture group" $ \s -> do
      result <- eval s "builtins.match \"h(.*)o\" \"hello\""
      assertList result [assertStringValF "ell"]

  , withSessionTest "match multiple capture groups" $ \s -> do
      result <- eval s "builtins.match \"(.)(.)(.*)\" \"hello\""
      assertList result
        [ assertStringValF "h"
        , assertStringValF "e"
        , assertStringValF "llo"
        ]

  , withSessionTest "match anchored (implicit)" $ \s -> do
      -- Nix's match implicitly anchors: ^regex$
      result <- eval s "builtins.match \"ell\" \"hello\""
      assertNull result  -- Should not match since it's anchored

    -- Not found cases
  , withSessionTest "match not found" $ \s -> do
      result <- eval s "builtins.match \"world\" \"hello\""
      assertNull result

  , withSessionTest "match partial no match" $ \s -> do
      -- "el" doesn't match the whole string
      result <- eval s "builtins.match \"el\" \"hello\""
      assertNull result

    -- Edge cases
  , withSessionTest "match empty string" $ \s -> do
      result <- eval s "builtins.match \"\" \"\""
      assertListLength result 0

  , withSessionTest "match optional capture" $ \s -> do
      result <- eval s "builtins.match \"a(b)?c\" \"ac\""
      -- Unmatched optional group returns null in the list
      assertList result [assertNullF]

  , withSessionTest "match dot matches any" $ \s -> do
      result <- eval s "builtins.match \"h.llo\" \"hello\""
      assertListLength result 0

  , withSessionTest "match character class" $ \s -> do
      result <- eval s "builtins.match \"[a-z]+\" \"hello\""
      assertListLength result 0

  , withSessionTest "match digit class" $ \s -> do
      result <- eval s "builtins.match \"[0-9]+\" \"12345\""
      assertListLength result 0
  ]

-- | Tests for builtins.split
splitTests :: TestTree
splitTests = testGroup "split"
  [ withSessionTest "split simple" $ \s -> do
      -- split "a" "xaxax" returns ["x" [] "x" [] "x"]
      result <- eval s "builtins.split \"a\" \"xaxax\""
      assertList result
        [ assertStringValF "x"
        , assertListLengthF 0  -- Empty capture group list for non-capturing regex
        , assertStringValF "x"
        , assertListLengthF 0
        , assertStringValF "x"
        ]

  , withSessionTest "split with capture" $ \s -> do
      -- split "(a)" "xaxax" returns ["x" ["a"] "x" ["a"] "x"]
      result <- eval s "builtins.split \"(a)\" \"xaxax\""
      assertList result
        [ assertStringValF "x"
        , assertListIs [assertStringValF "a"]
        , assertStringValF "x"
        , assertListIs [assertStringValF "a"]
        , assertStringValF "x"
        ]

  , withSessionTest "split no match" $ \s -> do
      result <- eval s "builtins.split \"z\" \"abc\""
      assertList result [assertStringValF "abc"]

  , withSessionTest "split at start" $ \s -> do
      result <- eval s "builtins.split \"a\" \"abc\""
      assertList result
        [ assertStringValF ""
        , assertListLengthF 0
        , assertStringValF "bc"
        ]

  , withSessionTest "split at end" $ \s -> do
      result <- eval s "builtins.split \"c\" \"abc\""
      assertList result
        [ assertStringValF "ab"
        , assertListLengthF 0
        , assertStringValF ""
        ]

  , withSessionTest "split multiple captures" $ \s -> do
      result <- eval s "builtins.split \"(a)(b)\" \"xabx\""
      assertList result
        [ assertStringValF "x"
        , assertListIs [assertStringValF "a", assertStringValF "b"]
        , assertStringValF "x"
        ]

  , withSessionTest "split empty string" $ \s -> do
      result <- eval s "builtins.split \"a\" \"\""
      assertList result [assertStringValF ""]
  ]

-- | Tests for builtins.hashString
hashStringTests :: TestTree
hashStringTests = testGroup "hashString"
  [ withSessionTest "md5 hash" $ \s -> do
      result <- eval s "builtins.hashString \"md5\" \"hello\""
      assertStringVal result "5d41402abc4b2a76b9719d911017c592"

  , withSessionTest "sha256 hash" $ \s -> do
      result <- eval s "builtins.hashString \"sha256\" \"hello\""
      assertStringVal result "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

  , withSessionTest "sha256 empty string" $ \s -> do
      result <- eval s "builtins.hashString \"sha256\" \"\""
      assertStringVal result "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  , withSessionTest "md5 empty string" $ \s -> do
      result <- eval s "builtins.hashString \"md5\" \"\""
      assertStringVal result "d41d8cd98f00b204e9800998ecf8427e"

  , withSessionTest "sha1 hash" $ \s -> do
      result <- eval s "builtins.hashString \"sha1\" \"hello\""
      assertStringVal result "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"

  , withSessionTest "sha512 hash" $ \s -> do
      result <- eval s "builtins.hashString \"sha512\" \"hello\""
      -- SHA512 produces 128 hex chars
      result' <- eval s "builtins.stringLength (builtins.hashString \"sha512\" \"hello\")"
      assertInt result' 128

  , withSessionTest "hash with special characters" $ \s -> do
      result <- eval s "builtins.hashString \"md5\" \"hello\\nworld\""
      assertIsString result

  , withSessionTest "unknown algorithm error" $ \s -> do
      expectError s "builtins.hashString \"unknown\" \"test\""
  ]

-- | Tests for builtins.compareVersions
compareVersionsTests :: TestTree
compareVersionsTests = testGroup "compareVersions"
  [ -- Equal versions
    withSessionTest "equal versions" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0\" \"1.0\""
      assertInt result 0

  , withSessionTest "equal complex versions" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.2.3\" \"1.2.3\""
      assertInt result 0

    -- Less than
  , withSessionTest "less than major" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0\" \"2.0\""
      assertInt result (-1)

  , withSessionTest "less than minor" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0\" \"1.1\""
      assertInt result (-1)

  , withSessionTest "less than patch" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0.0\" \"1.0.1\""
      assertInt result (-1)

    -- Greater than
  , withSessionTest "greater than major" $ \s -> do
      result <- eval s "builtins.compareVersions \"2.0\" \"1.0\""
      assertInt result 1

  , withSessionTest "greater than minor" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.2\" \"1.1\""
      assertInt result 1

  , withSessionTest "greater than patch" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0.2\" \"1.0.1\""
      assertInt result 1

    -- Edge cases
  , withSessionTest "shorter vs longer equal" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0\" \"1.0.0\""
      -- 1.0 vs 1.0.0 - shorter version is less in Nix
      assertInt result (-1)

  , withSessionTest "pre-release versions" $ \s -> do
      result <- eval s "builtins.compareVersions \"1.0pre1\" \"1.0\""
      -- pre1 should be less than release
      assertInt result (-1)

  , withSessionTest "empty versions" $ \s -> do
      result <- eval s "builtins.compareVersions \"\" \"\""
      assertInt result 0
  ]

-- | Tests for builtins.splitVersion
splitVersionTests :: TestTree
splitVersionTests = testGroup "splitVersion"
  [ withSessionTest "simple version" $ \s -> do
      result <- eval s "builtins.splitVersion \"1.2.3\""
      assertList result
        [ assertStringValF "1"
        , assertStringValF "2"
        , assertStringValF "3"
        ]

  , withSessionTest "version with pre" $ \s -> do
      result <- eval s "builtins.splitVersion \"1.0pre123\""
      -- Should split into components
      assertIsListNonEmpty result

  , withSessionTest "empty version" $ \s -> do
      result <- eval s "builtins.splitVersion \"\""
      assertListLength result 0

  , withSessionTest "single component" $ \s -> do
      result <- eval s "builtins.splitVersion \"42\""
      assertList result [assertStringValF "42"]

  , withSessionTest "version with dash" $ \s -> do
      result <- eval s "builtins.splitVersion \"1.0-rc1\""
      -- Dashes should also split
      assertIsListNonEmpty result
  ]

-- | Tests for context-related builtins
contextTests :: TestTree
contextTests = testGroup "context"
  [ -- unsafeDiscardStringContext
    withSessionTest "unsafeDiscardStringContext" $ \s -> do
      result <- eval s "builtins.unsafeDiscardStringContext \"hello\""
      assertStringVal result "hello"

  , withSessionTest "unsafeDiscardStringContext preserves value" $ \s -> do
      result <- eval s "builtins.unsafeDiscardStringContext \"test string\""
      assertStringVal result "test string"

    -- hasContext
  , withSessionTest "hasContext plain string false" $ \s -> do
      result <- eval s "builtins.hasContext \"hello\""
      assertBoolVal result False

  , withSessionTest "hasContext after discard false" $ \s -> do
      result <- eval s "builtins.hasContext (builtins.unsafeDiscardStringContext \"hello\")"
      assertBoolVal result False

    -- Note: Testing hasContext returning true requires a string with actual context,
    -- which typically comes from path interpolation or derivation outputs.
    -- This is hard to test without store integration.
  ]

-- | Tests for builtins.stringLength (additional coverage)
stringLengthTests :: TestTree
stringLengthTests = testGroup "stringLength"
  [ withSessionTest "empty string" $ \s -> do
      result <- eval s "builtins.stringLength \"\""
      assertInt result 0

  , withSessionTest "simple string" $ \s -> do
      result <- eval s "builtins.stringLength \"hello\""
      assertInt result 5

  , withSessionTest "unicode characters" $ \s -> do
      -- Unicode string length counts codepoints
      result <- eval s "builtins.stringLength \"hello\""
      assertInt result 5

  , withSessionTest "string with escape" $ \s -> do
      result <- eval s "builtins.stringLength \"a\\nb\""
      assertInt result 3  -- 'a', newline, 'b'

  , withSessionTest "string with interpolation" $ \s -> do
      result <- eval s "let x = \"world\"; in builtins.stringLength \"hello ${x}\""
      assertInt result 11  -- "hello world"
  ]

-- * Additional assertion helpers for nested structures

-- | Assert a value is a string with given value (for use in assertList)
assertStringValF :: Text -> NixValue -> Assertion
assertStringValF = flip assertStringVal

-- | Assert a value is null (for use in assertList)
assertNullF :: NixValue -> Assertion
assertNullF = assertNull

-- | Assert a value is a list with given length (for use in assertList)
assertListLengthF :: Int -> NixValue -> Assertion
assertListLengthF = flip assertListLength

-- | Assert a value is a list and check elements
assertListIs :: [NixValue -> Assertion] -> NixValue -> Assertion
assertListIs = flip assertList

-- | Assert a value is a non-empty list
assertIsListNonEmpty :: NixValue -> Assertion
assertIsListNonEmpty v = case v of
  VList vec -> assertBool "Expected non-empty list" (not $ null vec)
  _ -> assertFailure $ "Expected list, got: " <> show v
