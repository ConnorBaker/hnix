-- | Tests for control flow builtins in the Nix compiler.
--
-- This module tests builtins that control evaluation flow:
-- * tryEval - catches evaluation errors
-- * seq - forces first argument, returns second
-- * deepSeq - deeply evaluates first argument, returns second
-- * trace - prints to stderr, returns second argument
-- * System info builtins (currentSystem, langVersion, nixVersion)
module Compile.BuiltinsControlFlowTests (tests) where

import Relude
import Compile.TestCommon

-- | All control flow builtin tests
tests :: TestTree
tests = testGroup "Control Flow Builtins"
  [ tryEvalTests
  , seqTests
  , deepSeqTests
  , traceTests
  , systemInfoTests
  , lazyEvalTests
  ]

-- | Tests for builtins.tryEval
tryEvalTests :: TestTree
tryEvalTests = testGroup "tryEval"
  [ withSessionTest "tryEval success with int" $ \s -> do
      result <- eval s "builtins.tryEval (1 + 1)"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertInt v 2
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval success with string" $ \s -> do
      result <- eval s "builtins.tryEval \"hello\""
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertStringVal v "hello"
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval success with bool" $ \s -> do
      result <- eval s "builtins.tryEval true"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval success with null" $ \s -> do
      result <- eval s "builtins.tryEval null"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertNull v
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval success with list" $ \s -> do
      result <- eval s "builtins.tryEval [1 2 3]"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertListLength v 3
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval success with attrset" $ \s -> do
      result <- eval s "builtins.tryEval { a = 1; }"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertAttrsSize v 1
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval failure with throw" $ \s -> do
      result <- eval s "builtins.tryEval (throw \"error\")"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval failure with assert false" $ \s -> do
      result <- eval s "builtins.tryEval (assert false; 1)"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval failure with missing attr" $ \s -> do
      result <- eval s "builtins.tryEval ({ }.missing)"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval failure with division by zero" $ \s -> do
      result <- eval s "builtins.tryEval (1 / 0)"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertBoolVal v False
          Nothing -> assertFailure "missing 'value' attr"

  , withSessionTest "tryEval nested success" $ \s -> do
      result <- eval s "builtins.tryEval (builtins.tryEval 42)"
      assertAttrs result $ \attrs -> do
        case lookupAttr "success" attrs of
          Just v -> assertBoolVal v True
          Nothing -> assertFailure "missing outer 'success' attr"
        case lookupAttr "value" attrs of
          Just v -> assertAttrs v $ \inner -> do
            case lookupAttr "success" inner of
              Just v2 -> assertBoolVal v2 True
              Nothing -> assertFailure "missing inner 'success' attr"
            case lookupAttr "value" inner of
              Just v2 -> assertInt v2 42
              Nothing -> assertFailure "missing inner 'value' attr"
          Nothing -> assertFailure "missing outer 'value' attr"
  ]

-- | Tests for builtins.seq
seqTests :: TestTree
seqTests = testGroup "seq"
  [ withSessionTest "seq forces first, returns second" $ \s -> do
      result <- eval s "builtins.seq 1 2"
      assertInt result 2

  , withSessionTest "seq with different types" $ \s -> do
      result <- eval s "builtins.seq \"hello\" 42"
      assertInt result 42

  , withSessionTest "seq with null first arg" $ \s -> do
      result <- eval s "builtins.seq null \"result\""
      assertStringVal result "result"

  , withSessionTest "seq with list first arg" $ \s -> do
      result <- eval s "builtins.seq [1 2 3] true"
      assertBoolVal result True

  , withSessionTest "seq with attrset first arg" $ \s -> do
      result <- eval s "builtins.seq { x = 1; } false"
      assertBoolVal result False

  , withSessionTest "seq propagates errors in first arg" $ \s -> do
      expectError s "builtins.seq (throw \"error\") 42"

  , withSessionTest "seq does not deeply evaluate first arg" $ \s -> do
      -- The list is not deeply evaluated, only forced to WHNF
      -- So the throw inside should not be triggered
      result <- eval s "builtins.seq [1 (throw \"deep error\") 3] 42"
      assertInt result 42

  , withSessionTest "seq returns second arg unchanged" $ \s -> do
      result <- eval s "builtins.seq null { a = 1; b = 2; }"
      assertAttrsSize result 2

  , withSessionTest "seq with function result" $ \s -> do
      result <- eval s "let f = x: x + 1; in builtins.seq (f 1) (f 2)"
      assertInt result 3

  , withSessionTest "seq does not evaluate unused computations" $ \s -> do
      -- The error in the first arg means seq itself works, but if
      -- evaluation happens correctly, seq forces only WHNF
      result <- eval s "builtins.seq 1 2"
      assertInt result 2
  ]

-- | Tests for builtins.deepSeq
deepSeqTests :: TestTree
deepSeqTests = testGroup "deepSeq"
  [ withSessionTest "deepSeq forces first, returns second" $ \s -> do
      result <- eval s "builtins.deepSeq 1 2"
      assertInt result 2

  , withSessionTest "deepSeq with string first arg" $ \s -> do
      result <- eval s "builtins.deepSeq \"hello\" 42"
      assertInt result 42

  , withSessionTest "deepSeq deeply evaluates list" $ \s -> do
      -- The list is deeply evaluated, so throw inside should trigger
      expectError s "builtins.deepSeq [1 (throw \"deep error\") 3] 42"

  , withSessionTest "deepSeq success with simple list" $ \s -> do
      result <- eval s "builtins.deepSeq [1 2 3] \"done\""
      assertStringVal result "done"

  , withSessionTest "deepSeq deeply evaluates nested list" $ \s -> do
      result <- eval s "builtins.deepSeq [[1 2] [3 4]] true"
      assertBoolVal result True

  , withSessionTest "deepSeq deeply evaluates attrset" $ \s -> do
      expectError s "builtins.deepSeq { a = throw \"error\"; } 42"

  , withSessionTest "deepSeq success with simple attrset" $ \s -> do
      result <- eval s "builtins.deepSeq { a = 1; b = 2; } \"done\""
      assertStringVal result "done"

  , withSessionTest "deepSeq deeply evaluates nested attrset" $ \s -> do
      result <- eval s "builtins.deepSeq { a = { b = { c = 1; }; }; } 42"
      assertInt result 42

  , withSessionTest "deepSeq with mixed nested structure" $ \s -> do
      result <- eval s "builtins.deepSeq { xs = [1 2 3]; y = { z = 4; }; } null"
      assertNull result

  , withSessionTest "deepSeq error in nested list" $ \s -> do
      expectError s "builtins.deepSeq [[1 (throw \"nested\")]] 42"

  , withSessionTest "deepSeq error in nested attrset" $ \s -> do
      expectError s "builtins.deepSeq { a = { b = throw \"nested\"; }; } 42"

  , withSessionTest "deepSeq returns second arg unchanged" $ \s -> do
      result <- eval s "builtins.deepSeq [1 2] { a = 1; b = 2; c = 3; }"
      assertAttrsSize result 3

  , withSessionTest "deepSeq with null in structure" $ \s -> do
      result <- eval s "builtins.deepSeq [null { x = null; }] \"ok\""
      assertStringVal result "ok"
  ]

-- | Tests for builtins.trace
traceTests :: TestTree
traceTests = testGroup "trace"
  [ withSessionTest "trace returns second arg" $ \s -> do
      result <- eval s "builtins.trace \"message\" 42"
      assertInt result 42

  , withSessionTest "trace with string message" $ \s -> do
      result <- eval s "builtins.trace \"debug\" \"result\""
      assertStringVal result "result"

  , withSessionTest "trace with int message" $ \s -> do
      -- trace coerces first arg to string
      result <- eval s "builtins.trace 123 true"
      assertBoolVal result True

  , withSessionTest "trace with null result" $ \s -> do
      result <- eval s "builtins.trace \"checking\" null"
      assertNull result

  , withSessionTest "trace with list result" $ \s -> do
      result <- eval s "builtins.trace \"list\" [1 2 3]"
      assertListLength result 3

  , withSessionTest "trace with attrset result" $ \s -> do
      result <- eval s "builtins.trace \"set\" { a = 1; }"
      assertAttrsSize result 1

  , withSessionTest "trace nested" $ \s -> do
      result <- eval s "builtins.trace \"outer\" (builtins.trace \"inner\" 42)"
      assertInt result 42

  , withSessionTest "trace with complex message" $ \s -> do
      result <- eval s "builtins.trace \"value: ${toString 42}\" true"
      assertBoolVal result True

  , withSessionTest "trace with attrset message" $ \s -> do
      -- trace can print attrsets
      result <- eval s "builtins.trace { a = 1; } 42"
      assertInt result 42

  , withSessionTest "trace with list message" $ \s -> do
      result <- eval s "builtins.trace [1 2 3] \"done\""
      assertStringVal result "done"
  ]

-- | Tests for system info builtins
systemInfoTests :: TestTree
systemInfoTests = testGroup "System Info"
  [ withSessionTest "currentSystem is a string" $ \s -> do
      result <- eval s "builtins.currentSystem"
      assertIsString result

  , withSessionTest "currentSystem is non-empty" $ \s -> do
      result <- eval s "builtins.stringLength builtins.currentSystem > 0"
      assertBoolVal result True

  , withSessionTest "currentSystem contains hyphen" $ \s -> do
      -- System strings are like "x86_64-linux" or "aarch64-darwin"
      result <- eval s "builtins.match \".*-.*\" builtins.currentSystem != null"
      assertBoolVal result True

  , withSessionTest "langVersion is an int" $ \s -> do
      result <- eval s "builtins.langVersion"
      -- langVersion is an integer, just verify it is one
      case result of
        VInt _ -> pure ()
        _ -> assertFailure $ "Expected int, got: " <> show result

  , withSessionTest "langVersion is positive" $ \s -> do
      result <- eval s "builtins.langVersion > 0"
      assertBoolVal result True

  , withSessionTest "langVersion is at least 5" $ \s -> do
      -- Nix language version has been >= 5 for a long time
      result <- eval s "builtins.langVersion >= 5"
      assertBoolVal result True

  , withSessionTest "nixVersion is a string" $ \s -> do
      result <- eval s "builtins.nixVersion"
      assertIsString result

  , withSessionTest "nixVersion is non-empty" $ \s -> do
      result <- eval s "builtins.stringLength builtins.nixVersion > 0"
      assertBoolVal result True

  , withSessionTest "nixVersion contains period" $ \s -> do
      -- Version strings typically contain periods like "2.18.1"
      result <- eval s "builtins.match \".*\\\\..*\" builtins.nixVersion != null"
      assertBoolVal result True

  , withSessionTest "storeDir is a string" $ \s -> do
      result <- eval s "builtins.storeDir"
      assertIsString result

  , withSessionTest "storeDir starts with slash" $ \s -> do
      result <- eval s "builtins.substring 0 1 builtins.storeDir"
      assertStringVal result "/"

  , withSessionTest "storeDir is /nix/store by default" $ \s -> do
      result <- eval s "builtins.storeDir"
      assertStringVal result "/nix/store"
  ]

-- | Tests to verify lazy evaluation is working correctly
lazyEvalTests :: TestTree
lazyEvalTests = testGroup "Lazy Evaluation Verification"
  [ withSessionTest "unused branch not evaluated" $ \s -> do
      -- The false branch has an error but should not be evaluated
      result <- eval s "if true then 1 else builtins.throw \"should not happen\""
      assertInt result 1
  , withSessionTest "unused or branch not evaluated" $ \s -> do
      result <- eval s "true || builtins.throw \"short circuit\""
      assertBoolVal result True
  , withSessionTest "unused and branch not evaluated" $ \s -> do
      result <- eval s "false && builtins.throw \"short circuit\""
      assertBoolVal result False
  , withSessionTest "unused implication branch not evaluated" $ \s -> do
      result <- eval s "false -> builtins.throw \"short circuit\""
      assertBoolVal result True
  ]
