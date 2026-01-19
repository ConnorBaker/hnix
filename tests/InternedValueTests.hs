{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for pointer equality of interned values.
--
-- These tests verify that operations which should return interned singleton
-- values (empty list, empty set, true, false, null) actually return the
-- exact same heap object as the cached interned value.
--
-- We use 'System.Mem.StableName' to check pointer equality. If two values
-- have the same StableName, they are the same heap object.
module InternedValueTests (tests) where

import           Nix.Prelude
import           Control.Monad.Catch            ( catch )
import           Data.Time                      ( getCurrentTime )
import           GHC.Err                        ( errorWithoutStackTrace )
import           System.Mem.StableName          ( makeStableName, eqStableName )

import           Nix
import           Nix.Standard
import           Nix.Value.Monad                ( demand )

import           Test.Tasty
import           Test.Tasty.HUnit


-- | Type aliases for the standard evaluation monad (no stats, default config).
type StandardIO = StdM 'False DefaultCfg IO
type StdVal = ValueF 'False StandardIO
type StdThun = ThunkF 'False StandardIO


-- | Run an evaluation action and return the result.
runEval :: StandardIO a -> IO a
runEval action = do
  time <- getCurrentTime
  let opts = defaultOptions time
  runWithBasicEffects opts action

-- | Evaluate a Nix expression to WHNF (weak head normal form).
--
-- Uses 'demand' instead of 'normalForm' to preserve pointer equality.
-- 'normalForm' traverses and reconstructs the entire value tree, which
-- creates new heap objects even for structurally identical values.
-- 'demand' only forces the outermost thunk without reconstruction.
evalExpr :: Text -> StandardIO StdVal
evalExpr src =
  case parseNixText src of
    Left err -> errorWithoutStackTrace $ "Parse error: " <> show err
    Right expr ->
      (demand =<< nixEvalExpr mempty expr)
        `catch` \case
          NixException frames ->
            errorWithoutStackTrace . show
              =<< renderFrames @StdVal @StdThun frames

-- | Test that the result of evaluating an expression is pointer-equal
-- to an interned value.
--
-- The test evaluates the expression and the getter for the interned value
-- within the same evaluation context, then compares their StableNames.
assertPointerEqual
  :: Text                         -- ^ Nix expression to evaluate
  -> StandardIO StdVal            -- ^ Getter for interned value
  -> Text                         -- ^ Description for error message
  -> IO ()
assertPointerEqual expr getInterned desc = do
  (result, interned) <- runEval $ do
    r <- evalExpr expr
    i <- getInterned
    pure (r, i)
  -- Get stable names in IO to ensure heap objects are visible
  snResult   <- makeStableName result
  snInterned <- makeStableName interned
  assertBool
    (toString $ "Expected " <> desc <> " to be pointer-equal to interned value.\n"
      <> "Expression: " <> expr)
    (snResult `eqStableName` snInterned)

-- | Test that the result of evaluating an expression is NOT pointer-equal
-- to an interned value (sanity check).
assertPointerNotEqual
  :: Text
  -> StandardIO StdVal
  -> Text
  -> IO ()
assertPointerNotEqual expr getInterned desc = do
  (result, interned) <- runEval $ do
    r <- evalExpr expr
    i <- getInterned
    pure (r, i)
  snResult   <- makeStableName result
  snInterned <- makeStableName interned
  assertBool
    (toString $ "Expected " <> desc <> " to NOT be pointer-equal to interned value (sanity check).\n"
      <> "Expression: " <> expr)
    (not $ snResult `eqStableName` snInterned)


-- ============================================================================
-- Test groups
-- ============================================================================

tests :: TestTree
tests = testGroup "Interned value pointer equality"
  [ emptyListTests
  , emptySetTests
  , booleanTests
  , nullTests
  , sanityTests
  ]

-- | Tests for operations that should return the interned empty list.
emptyListTests :: TestTree
emptyListTests = testGroup "Empty list fast paths"
  [ testCase "builtins.map over empty list" $
      assertPointerEqual
        "builtins.map (x: x) []"
        askInternedEmptyList
        "map over []"

  , testCase "builtins.filter over empty list" $
      assertPointerEqual
        "builtins.filter (x: true) []"
        askInternedEmptyList
        "filter over []"

  , testCase "builtins.filter returns empty (all filtered)" $
      assertPointerEqual
        "builtins.filter (x: false) [1 2 3]"
        askInternedEmptyList
        "filter returns []"

  , testCase "builtins.tail of singleton" $
      assertPointerEqual
        "builtins.tail [1]"
        askInternedEmptyList
        "tail of [1]"

  , testCase "builtins.sort of empty list" $
      assertPointerEqual
        "builtins.sort (a: b: a < b) []"
        askInternedEmptyList
        "sort of []"

  , testCase "builtins.concatLists of empty list" $
      assertPointerEqual
        "builtins.concatLists []"
        askInternedEmptyList
        "concatLists of []"

  , testCase "builtins.concatLists result is empty" $
      assertPointerEqual
        "builtins.concatLists [[] []]"
        askInternedEmptyList
        "concatLists of [[] []]"

  , testCase "builtins.concatMap over empty list" $
      assertPointerEqual
        "builtins.concatMap (x: [x]) []"
        askInternedEmptyList
        "concatMap over []"

  , testCase "builtins.genList with n=0" $
      assertPointerEqual
        "builtins.genList (x: x) 0"
        askInternedEmptyList
        "genList with 0"

  , testCase "builtins.catAttrs over empty list" $
      assertPointerEqual
        "builtins.catAttrs \"x\" []"
        askInternedEmptyList
        "catAttrs over []"

  , testCase "builtins.catAttrs returns empty" $
      assertPointerEqual
        "builtins.catAttrs \"x\" [{ y = 1; } { z = 2; }]"
        askInternedEmptyList
        "catAttrs returns []"

  , testCase "builtins.partition empty input - right list" $
      assertPointerEqual
        "(builtins.partition (x: true) []).wrong"
        askInternedEmptyList
        "partition [] .wrong"

  , testCase "builtins.partition empty input - left list" $
      -- When input is empty, both .right and .wrong should be interned empty
      assertPointerEqual
        "(builtins.partition (x: true) []).right"
        askInternedEmptyList
        "partition [] .right"
  ]

-- | Tests for operations that should return the interned empty set.
emptySetTests :: TestTree
emptySetTests = testGroup "Empty set fast paths"
  [ testCase "builtins.mapAttrs over empty set" $
      assertPointerEqual
        "builtins.mapAttrs (n: v: v) {}"
        askInternedEmptySet
        "mapAttrs over {}"

  , testCase "builtins.listToAttrs of empty list" $
      assertPointerEqual
        "builtins.listToAttrs []"
        askInternedEmptySet
        "listToAttrs of []"

  , testCase "builtins.groupBy over empty list" $
      assertPointerEqual
        "builtins.groupBy (x: x) []"
        askInternedEmptySet
        "groupBy over []"

  , testCase "builtins.intersectAttrs with empty first arg" $
      assertPointerEqual
        "builtins.intersectAttrs {} { x = 1; }"
        askInternedEmptySet
        "intersectAttrs {} {...}"

  , testCase "builtins.intersectAttrs with empty second arg" $
      assertPointerEqual
        "builtins.intersectAttrs { x = 1; } {}"
        askInternedEmptySet
        "intersectAttrs {...} {}"

  , testCase "builtins.intersectAttrs no common keys" $
      assertPointerEqual
        "builtins.intersectAttrs { a = 1; } { b = 2; }"
        askInternedEmptySet
        "intersectAttrs disjoint"

  , testCase "builtins.zipAttrsWith over empty list" $
      assertPointerEqual
        "builtins.zipAttrsWith (n: vs: vs) []"
        askInternedEmptySet
        "zipAttrsWith over []"
  ]

-- | Tests for boolean operations that should return interned true/false.
booleanTests :: TestTree
booleanTests = testGroup "Boolean fast paths"
  [ testCase "literal true" $
      assertPointerEqual
        "true"
        askInternedTrue
        "true literal"

  , testCase "literal false" $
      assertPointerEqual
        "false"
        askInternedFalse
        "false literal"

  , testCase "builtins.elem found" $
      assertPointerEqual
        "builtins.elem 2 [1 2 3]"
        askInternedTrue
        "elem found"

  , testCase "builtins.elem not found" $
      assertPointerEqual
        "builtins.elem 4 [1 2 3]"
        askInternedFalse
        "elem not found"

  , testCase "builtins.any true" $
      assertPointerEqual
        "builtins.any (x: x > 2) [1 2 3]"
        askInternedTrue
        "any true"

  , testCase "builtins.any false" $
      assertPointerEqual
        "builtins.any (x: x > 10) [1 2 3]"
        askInternedFalse
        "any false"

  , testCase "builtins.all true" $
      assertPointerEqual
        "builtins.all (x: x < 10) [1 2 3]"
        askInternedTrue
        "all true"

  , testCase "builtins.all false" $
      assertPointerEqual
        "builtins.all (x: x > 1) [1 2 3]"
        askInternedFalse
        "all false"
  ]

-- | Tests for null operations.
nullTests :: TestTree
nullTests = testGroup "Null fast paths"
  [ testCase "literal null" $
      assertPointerEqual
        "null"
        askInternedNull
        "null literal"
  ]

-- | Sanity checks that non-empty/non-singleton values are NOT pointer-equal.
sanityTests :: TestTree
sanityTests = testGroup "Sanity checks (should NOT be pointer-equal)"
  [ testCase "[1] is not empty list" $
      assertPointerNotEqual
        "[1]"
        askInternedEmptyList
        "[1]"

  , testCase "{ x = 1; } is not empty set" $
      assertPointerNotEqual
        "{ x = 1; }"
        askInternedEmptySet
        "{ x = 1; }"

  , testCase "1 is not interned null" $
      assertPointerNotEqual
        "1"
        askInternedNull
        "1"
  ]
