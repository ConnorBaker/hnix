-- | Tests for the NExpr to GHC Core compiler.
--
-- These tests verify that the compiler correctly translates Nix expressions
-- and that the runtime evaluates them to the expected values.
module CompileTests (tests) where

import Relude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Exception (SomeException, try, throwIO)

import Nix.Compile.Driver (NixSession, initSession, evalNixText, NixCompileError(..))
import Nix.Compile.Value

-- New extended test modules
import qualified Compile.BuiltinsArithmeticTests
import qualified Compile.BuiltinsAttrSetTests
import qualified Compile.BuiltinsControlFlowTests
import qualified Compile.BuiltinsListTests
import qualified Compile.BuiltinsStringTests
import qualified Compile.BuiltinsTypeTests
import qualified Compile.EdgeCaseTests
import qualified Compile.ErrorTests
import qualified Compile.FloatTests
import qualified Compile.ImportCacheTests
import qualified Compile.OperatorTests
import qualified Compile.PatternFunctionTests

-- | Helper to run a test that needs a NixSession.
-- Returns a test that skips if session initialization fails (e.g., in CI without runtime).
withSessionTest :: String -> (NixSession -> IO ()) -> TestTree
withSessionTest name test = testCase name $ do
  eSession <- initSession
  case eSession of
    Left (RuntimeLoadError _) ->
      -- Skip test if runtime not available (common in CI)
      assertFailure "Skipped: hnix-compile-runtime not available"
    Left err ->
      assertFailure $ "Session init failed: " <> show err
    Right session ->
      test session

-- | Evaluate Nix text and return result, failing the test on error.
eval :: NixSession -> Text -> IO NixValue
eval session text = do
  result <- evalNixText session text
  case result of
    Left err -> assertFailure ("Eval failed: " <> show err) >> error "unreachable"
    Right val -> pure val

-- | Evaluate Nix text, returning Left on error instead of failing the test
evalMayFail :: NixSession -> Text -> IO (Either SomeException NixValue)
evalMayFail session text = do
  try @SomeException $ do
    result <- evalNixText session text
    case result of
      Left err -> throwIO err
      Right val -> pure val

-- | Expect an error from evaluation
expectError :: NixSession -> Text -> Assertion
expectError session text = do
  result <- evalMayFail session text
  case result of
    Left _ -> pure ()
    Right v -> assertFailure $ "Expected error, got: " <> show v

-- | All compiler tests
tests :: TestTree
tests = testGroup "Compiler"
  [ compilerTests
  -- Builtin tests
  , Compile.BuiltinsArithmeticTests.tests
  , Compile.BuiltinsAttrSetTests.tests
  , Compile.BuiltinsControlFlowTests.tests
  , Compile.BuiltinsListTests.tests
  , Compile.BuiltinsStringTests.tests
  , Compile.BuiltinsTypeTests.tests
  -- Feature tests
  , Compile.EdgeCaseTests.tests
  , Compile.ErrorTests.tests
  , Compile.FloatTests.tests
  , Compile.ImportCacheTests.tests
  , Compile.OperatorTests.tests
  , Compile.PatternFunctionTests.tests
  ]

-- | Compiler test groups covering literals, arithmetic, comparison, logical,
-- conditionals, let bindings, functions, attribute sets, and more.
compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ testGroup "Literals"
      [ withSessionTest "integer" $ \session -> do
          result <- eval session "42"
          assertInt result 42
      , withSessionTest "float" $ \session -> do
          result <- eval session "3.14"
          assertFloat result 3.14
      , withSessionTest "bool true" $ \session -> do
          result <- eval session "true"
          assertBoolVal result True
      , withSessionTest "bool false" $ \session -> do
          result <- eval session "false"
          assertBoolVal result False
      , withSessionTest "null" $ \session -> do
          result <- eval session "null"
          assertNull result
      , withSessionTest "string" $ \session -> do
          result <- eval session "\"hello\""
          assertStringVal result "hello"
      ]
  , testGroup "Arithmetic"
      [ withSessionTest "addition" $ \session -> do
          result <- eval session "1 + 2"
          assertInt result 3
      , withSessionTest "subtraction" $ \session -> do
          result <- eval session "5 - 3"
          assertInt result 2
      , withSessionTest "multiplication" $ \session -> do
          result <- eval session "3 * 4"
          assertInt result 12
      , withSessionTest "division" $ \session -> do
          result <- eval session "10 / 3"
          assertInt result 3
      , withSessionTest "negation" $ \session -> do
          result <- eval session "-5"
          assertInt result (-5)
      ]
  , testGroup "Comparison"
      [ withSessionTest "less than" $ \session -> do
          result <- eval session "1 < 2"
          assertBoolVal result True
      , withSessionTest "greater than" $ \session -> do
          result <- eval session "2 > 1"
          assertBoolVal result True
      , withSessionTest "equality" $ \session -> do
          result <- eval session "1 == 1"
          assertBoolVal result True
      , withSessionTest "inequality" $ \session -> do
          result <- eval session "1 != 2"
          assertBoolVal result True
      ]
  , testGroup "Logical"
      [ withSessionTest "and true" $ \session -> do
          result <- eval session "true && true"
          assertBoolVal result True
      , withSessionTest "and false" $ \session -> do
          result <- eval session "true && false"
          assertBoolVal result False
      , withSessionTest "or true" $ \session -> do
          result <- eval session "false || true"
          assertBoolVal result True
      , withSessionTest "not" $ \session -> do
          result <- eval session "!false"
          assertBoolVal result True
      , withSessionTest "implication" $ \session -> do
          result <- eval session "false -> true"
          assertBoolVal result True
      ]
  , testGroup "Conditionals"
      [ withSessionTest "if true" $ \session -> do
          result <- eval session "if true then 1 else 2"
          assertInt result 1
      , withSessionTest "if false" $ \session -> do
          result <- eval session "if false then 1 else 2"
          assertInt result 2
      ]
  , testGroup "Let bindings"
      [ withSessionTest "simple let" $ \session -> do
          result <- eval session "let x = 1; in x"
          assertInt result 1
      , withSessionTest "multiple bindings" $ \session -> do
          result <- eval session "let x = 1; y = 2; in x + y"
          assertInt result 3
      , withSessionTest "recursive let" $ \session -> do
          result <- eval session "let x = 1; y = x + 1; in y"
          assertInt result 2
      ]
  , testGroup "Functions"
      [ withSessionTest "simple lambda" $ \session -> do
          result <- eval session "(x: x + 1) 5"
          assertInt result 6
      , withSessionTest "two arguments" $ \session -> do
          result <- eval session "(x: y: x + y) 2 3"
          assertInt result 5
      ]
  , testGroup "Attribute sets"
      [ withSessionTest "empty set" $ \session -> do
          result <- eval session "{ }"
          assertAttrsSize result 0
      , withSessionTest "single attr" $ \session -> do
          result <- eval session "{ x = 1; }.x"
          assertInt result 1
      , withSessionTest "nested path" $ \session -> do
          result <- eval session "{ a.b.c = 1; }.a.b.c"
          assertInt result 1
      , withSessionTest "recursive set" $ \session -> do
          result <- eval session "rec { x = 1; y = x + 1; }.y"
          assertInt result 2
      ]
  , testGroup "Dynamic keys"
      [ -- Basic dynamic keys (already supported, verify they work)
        withSessionTest "simple dynamic key" $ \session -> do
          result <- eval session "let k = \"x\"; in { ${k} = 1; }.x"
          assertInt result 1
      , withSessionTest "dynamic key literal string" $ \session -> do
          result <- eval session "{ ${\"a\"} = 42; }.a"
          assertInt result 42
      , withSessionTest "dynamic key with expression" $ \session -> do
          result <- eval session "let k = \"foo\" + \"bar\"; in { ${k} = 1; }.foobar"
          assertInt result 1

        -- Dynamic key at end of path (a.${k})
      , withSessionTest "dynamic key at end" $ \session -> do
          result <- eval session "let k = \"b\"; in { a.${k} = 1; }.a.b"
          assertInt result 1
      , withSessionTest "dynamic key at end of longer path" $ \session -> do
          result <- eval session "let k = \"c\"; in { a.b.${k} = 1; }.a.b.c"
          assertInt result 1

        -- Dynamic key in middle (a.${k}.c) - the new feature
      , withSessionTest "dynamic key in middle" $ \session -> do
          result <- eval session "let k = \"b\"; in { a.${k}.c = 1; }.a.b.c"
          assertInt result 1
      , withSessionTest "dynamic key in middle computed" $ \session -> do
          result <- eval session "let x = \"mid\"; in { foo.${x}.bar = 42; }.foo.mid.bar"
          assertInt result 42
      , withSessionTest "dynamic key creates nested structure" $ \session -> do
          -- Verify that { a.${k}.c = 1; } creates { a = { $k = { c = 1; }; }; }
          result <- eval session "let k = \"nested\"; in { a.${k}.c = 1; }.a ? nested"
          assertBoolVal result True

        -- Multiple dynamic keys in path
      , withSessionTest "two dynamic keys" $ \session -> do
          result <- eval session "let k1 = \"a\"; k2 = \"b\"; in { ${k1}.${k2} = 1; }.a.b"
          assertInt result 1
      , withSessionTest "alternating static and dynamic" $ \session -> do
          result <- eval session "let k = \"dyn\"; in { s1.${k}.s2 = 1; }.s1.dyn.s2"
          assertInt result 1

        -- Dynamic keys in recursive sets
        -- DISABLED: GHC 9.12.2 Core simplifier panic (refineFromInScope)
        -- These tests trigger a bug where dynamic binding values containing Var
        -- references to letrec-bound Ids are used outside the Let (Rec ...)
        -- expression, causing GHC's simplifier to panic with:
        --   panic! (the 'impossible' happened)
        --   refineFromInScope InScope {wild_00} a_nJL0
        -- See: GHC #25631, GHC #20639, GHC #22028
      -- , withSessionTest "dynamic key in rec set" $ \session -> do
      --     result <- eval session "rec { ${\"b\"} = a; a = 1; }.b"
      --     assertInt result 1
      -- , withSessionTest "dynamic reference in rec set" $ \session -> do
      --     -- Dynamic bindings can reference static bindings in rec
      --     result <- eval session "rec { x = 10; ${\"y\"} = x + 1; }.y"
      --     assertInt result 11

        -- Dynamic key selection (set.${k})
      , withSessionTest "dynamic selection" $ \session -> do
          result <- eval session "let k = \"a\"; in { a = 1; b = 2; }.${k}"
          assertInt result 1
      , withSessionTest "dynamic hasAttr" $ \session -> do
          result <- eval session "let k = \"a\"; in { a = 1; } ? ${k}"
          assertBoolVal result True
      , withSessionTest "dynamic hasAttr missing" $ \session -> do
          result <- eval session "let k = \"z\"; in { a = 1; } ? ${k}"
          assertBoolVal result False

        -- Dynamic keys with defaults
      , withSessionTest "dynamic select or" $ \session -> do
          result <- eval session "let k = \"a\"; in { a = 1; }.${k} or 0"
          assertInt result 1
      , withSessionTest "dynamic select or missing" $ \session -> do
          result <- eval session "let k = \"z\"; in { a = 1; }.${k} or 99"
          assertInt result 99

        -- Error cases
      , withSessionTest "dynamic key non-string errors" $ \session -> do
          -- Using an integer as key should fail
          result <- evalMayFail session "{ ${123} = 1; }"
          case result of
            Left _ -> pure ()  -- Expected: type error
            Right _ -> assertFailure "Expected type error for non-string key"
      ]
  , testGroup "Inherit"
      [ withSessionTest "inherit in rec" $ \session -> do
          result <- eval session "let x = 1; in rec { inherit x; }.x"
          assertInt result 1
      , withSessionTest "inherit from scope" $ \session -> do
          result <- eval session "let s = { a = 1; }; in { inherit (s) a; }.a"
          assertInt result 1
      ]
  , testGroup "Lists"
      [ withSessionTest "empty list" $ \session -> do
          result <- eval session "[ ]"
          assertListLength result 0
      , withSessionTest "list length" $ \session -> do
          result <- eval session "builtins.length [ 1 2 3 ]"
          assertInt result 3
      , withSessionTest "list elemAt" $ \session -> do
          result <- eval session "builtins.elemAt [ 10 20 30 ] 1"
          assertInt result 20
      ]
  , testGroup "Strings"
      [ withSessionTest "concatenation" $ \session -> do
          result <- eval session "\"hello\" + \" world\""
          assertStringVal result "hello world"
      , withSessionTest "interpolation" $ \session -> do
          result <- eval session "let x = \"world\"; in \"hello ${x}\""
          assertStringVal result "hello world"
      , withSessionTest "stringLength" $ \session -> do
          result <- eval session "builtins.stringLength \"hello\""
          assertInt result 5
      ]
  , testGroup "HasAttr"
      [ withSessionTest "has single" $ \session -> do
          result <- eval session "{ a = 1; } ? a"
          assertBoolVal result True
      , withSessionTest "missing single" $ \session -> do
          result <- eval session "{ a = 1; } ? b"
          assertBoolVal result False
      , withSessionTest "has nested" $ \session -> do
          result <- eval session "{ a.b = 1; } ? a.b"
          assertBoolVal result True
      ]
  , testGroup "Select with default"
      [ withSessionTest "exists" $ \session -> do
          result <- eval session "{ a = 1; }.a or 0"
          assertInt result 1
      , withSessionTest "missing" $ \session -> do
          result <- eval session "{ }.a or 0"
          assertInt result 0
      , withSessionTest "nested missing" $ \session -> do
          result <- eval session "{ a = { }; }.a.b.c or 42"
          assertInt result 42
      ]
  , testGroup "With"
      [ withSessionTest "simple with" $ \session -> do
          result <- eval session "with { x = 1; }; x"
          assertInt result 1
      , withSessionTest "nested with" $ \session -> do
          result <- eval session "with { x = 1; }; with { y = 2; }; x + y"
          assertInt result 3
      , withSessionTest "with shadowing" $ \session -> do
          result <- eval session "with { x = 1; }; with { x = 2; }; x"
          assertInt result 2
      , withSessionTest "lexical beats with" $ \session -> do
          result <- eval session "let x = 1; in with { x = 2; }; x"
          assertInt result 1
      , withSessionTest "with from expression" $ \session -> do
          result <- eval session "with ({ a = 1; } // { b = 2; }); a + b"
          assertInt result 3
      ]
  , testGroup "Variable Shadowing"
      [ withSessionTest "let shadows outer let" $ \session -> do
          result <- eval session "let x = 1; in let x = 2; in x"
          assertInt result 2
      , withSessionTest "lambda param shadows let" $ \session -> do
          result <- eval session "let x = 1; in (x: x) 2"
          assertInt result 2
      , withSessionTest "pattern param shadows let" $ \session -> do
          result <- eval session "let x = 1; in ({ x }: x) { x = 2; }"
          assertInt result 2
      , withSessionTest "rec binding shadows outer" $ \session -> do
          result <- eval session "let x = 1; in rec { x = 2; y = x; }.y"
          assertInt result 2
      ]
  , testGroup "Assert"
      [ withSessionTest "assert with true succeeds" $ \session -> do
          result <- eval session "assert true; 1"
          assertInt result 1
      , withSessionTest "assert with false fails" $ \session -> do
          expectError session "assert false; 1"
      , withSessionTest "assert in let" $ \session -> do
          result <- eval session "let x = assert true; 1; in x"
          assertInt result 1
      , withSessionTest "assert with expression" $ \session -> do
          result <- eval session "assert 1 == 1; 42"
          assertInt result 42
      ]
  , testGroup "Paths"
      [ -- Literal paths
        withSessionTest "literal path" $ \session -> do
          result <- eval session "/foo/bar"
          assertPath result "/foo/bar"
      , withSessionTest "relative path" $ \session -> do
          result <- eval session "./foo"
          assertIsPath result  -- Just verify it's a path
      , withSessionTest "parent path" $ \session -> do
          result <- eval session "../foo"
          assertIsPath result
      , withSessionTest "nested relative path" $ \session -> do
          result <- eval session "./a/b/c"
          assertIsPath result

        -- Path type checks
      , withSessionTest "isPath true" $ \session -> do
          result <- eval session "builtins.isPath /foo"
          assertBoolVal result True
      , withSessionTest "isPath false for string" $ \session -> do
          result <- eval session "builtins.isPath \"foo\""
          assertBoolVal result False
      , withSessionTest "typeOf path" $ \session -> do
          result <- eval session "builtins.typeOf /foo"
          assertStringVal result "path"

        -- Path to string coercion
      , withSessionTest "path to string coercion" $ \session -> do
          result <- eval session "\"${/foo/bar}\""
          assertStringContains result "/foo/bar"
      , withSessionTest "path concatenation" $ \session -> do
          result <- eval session "/foo + \"/bar\""
          -- Path + string (no context) returns path (Nix semantics)
          assertPath result "/foo/bar"

        -- Path builtins
      , withSessionTest "baseNameOf path" $ \session -> do
          result <- eval session "builtins.baseNameOf /foo/bar"
          assertStringVal result "bar"
      , withSessionTest "baseNameOf string" $ \session -> do
          result <- eval session "builtins.baseNameOf \"/foo/bar\""
          assertStringVal result "bar"
      , withSessionTest "dirOf path" $ \session -> do
          result <- eval session "builtins.dirOf /foo/bar"
          -- dirOf returns a path
          assertIsPath result

        -- Path interpolation (NPath constructor)
      , withSessionTest "path interpolation simple" $ \session -> do
          result <- eval session "let x = \"sub\"; in ./foo/${x}"
          assertIsPath result
      , withSessionTest "path interpolation multiple" $ \session -> do
          result <- eval session "let a = \"x\"; b = \"y\"; in ./${a}/${b}"
          assertIsPath result

        -- Environment paths (if runtime supports them)
        -- These may fail if NIX_PATH is not set up
      , withSessionTest "env path resolves" $ \session -> do
          -- This test verifies the compilation works; actual resolution
          -- depends on NIX_PATH being configured
          -- For now, just test that it compiles and runs (may throw)
          result <- evalMayFail session "<nonexistent-test-path>"
          case result of
            Left _ -> pure ()  -- Expected: path not found
            Right v -> assertIsPath v
      ]
  ]

-- | Helper: assert the value is an integer with given value
assertInt :: NixValue -> Int64 -> Assertion
assertInt (VInt n) expected = n @?= expected
assertInt v _ = assertFailure $ "Expected int, got: " <> show v

-- | Helper: assert the value is a float with given value
assertFloat :: NixValue -> Double -> Assertion
assertFloat (VFloat n) expected = n @?= expected
assertFloat v _ = assertFailure $ "Expected float, got: " <> show v

-- | Helper: assert the value is a bool with given value
assertBoolVal :: NixValue -> Bool -> Assertion
assertBoolVal (VBool b) expected = b @?= expected
assertBoolVal v _ = assertFailure $ "Expected bool, got: " <> show v

-- | Helper: assert the value is null
assertNull :: NixValue -> Assertion
assertNull VNull = pure ()
assertNull v = assertFailure $ "Expected null, got: " <> show v

-- | Helper: assert the value is a string with given text
assertStringVal :: NixValue -> Text -> Assertion
assertStringVal (VString t _) expected = t @?= expected
assertStringVal v _ = assertFailure $ "Expected string, got: " <> show v

-- | Helper: assert the value is an attrset with given size
assertAttrsSize :: NixValue -> Int -> Assertion
assertAttrsSize (VAttrs as) expected = attrsSize as @?= expected
assertAttrsSize v _ = assertFailure $ "Expected attrset, got: " <> show v

-- | Helper: assert the value is a list with given length
assertListLength :: NixValue -> Int -> Assertion
assertListLength (VList v) expected = V.length v @?= expected
assertListLength v _ = assertFailure $ "Expected list, got: " <> show v

-- | Helper: assert the value is a path
assertIsPath :: NixValue -> Assertion
assertIsPath (VPath _) = pure ()
assertIsPath v = assertFailure $ "Expected path, got: " <> show v

-- | Helper: assert the value is a path with specific text
assertPath :: NixValue -> Text -> Assertion
assertPath (VPath p) expected = toText p @?= expected
assertPath v _ = assertFailure $ "Expected path, got: " <> show v

-- | Helper: assert the value is a string containing given text
assertStringContains :: NixValue -> Text -> Assertion
assertStringContains (VString t _) substr =
  assertBool ("Expected string containing " <> show substr <> ", got: " <> show t)
             (substr `T.isInfixOf` t)
assertStringContains v _ = assertFailure $ "Expected string, got: " <> show v

-- | Helper: assert the value is a string (any string)
assertIsString :: NixValue -> Assertion
assertIsString (VString _ _) = pure ()
assertIsString v = assertFailure $ "Expected string, got: " <> show v
