{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Memory benchmarks for hnix using weigh.
--
-- These benchmarks measure memory allocation for critical code paths
-- to ensure zero-overhead abstractions and efficient data structures.
--
-- Run with:
--
-- > nix develop ".?submodules=1#" --command cabal bench hnix-weigh
module Main (main) where

import Relude

import Control.Monad.Catch (catch)
import Data.Time (getCurrentTime)
import GHC.Err (errorWithoutStackTrace)
import Weigh (Grouped (..), Weight (..), Weigh, func, io, weighResults, wgroup)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Vector as V

import Data.Singletons.Bool (sbool)
import Nix (NixException (..), SBool (..), SBoolI, VarName, defaultOptions,
            nixEvalExpr, normalForm, parseNixText, renderFrames)
import Nix.Config.Singleton (DefaultCfg)
import Nix.Scope (Scope (..), scopeLookup)
import Nix.Standard (StdM, ThunkF, ValueF, runWithBasicEffects)


-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  -- Use weighResults instead of mainWith to get results as data.
  -- This allows custom formatting, working around a bug in weigh's
  -- Markdown output where headers lack a space after '#' (e.g.,
  -- "#Group" instead of "# Group"), producing invalid markdown.
  (results, _config) <- weighResults benchmarks
  putTextLn (formatMarkdown results)


-- ============================================================================
-- Markdown formatting
--
-- weigh's built-in Markdown formatter outputs "#GroupName" instead of
-- "# GroupName", which is invalid markdown (headers require a space
-- after '#'). These functions produce valid markdown.
-- ============================================================================

-- | Format all benchmark results as markdown.
formatMarkdown :: [Grouped (Weight, Maybe String)] -> Text
formatMarkdown groups = unlines (map formatGroup groups)

-- | Format a single benchmark group as a markdown section.
formatGroup :: Grouped (Weight, Maybe String) -> Text
formatGroup (Grouped name entries) = unlines
  [ "## " <> toText name
  , ""
  , tableHeader
  , tableSeparator
  , mconcat (map formatEntry entries)
  ]
formatGroup (Singleton (weight, mErr)) =
  formatWeightRow weight mErr

-- | Format a single entry within a group.
formatEntry :: Grouped (Weight, Maybe String) -> Text
formatEntry (Singleton (weight, mErr)) = formatWeightRow weight mErr
formatEntry (Grouped name _) =
  -- Nested groups are not used, but handle defensively
  "| " <> toText name <> " | (subgroup) | | | |\n"

-- | Format a single weight measurement as a markdown table row.
formatWeightRow :: Weight -> Maybe String -> Text
formatWeightRow w mErr = mconcat
  [ "| ", toText (stripGroupPrefix (weightLabel w))
  , " | ", formatWithCommas (weightAllocatedBytes w)
  , " | ", show (weightGCs w)
  , " | ", formatWithCommas (weightLiveBytes w)
  , " | ", formatWithCommas (weightMaxBytes w)
  , " |", maybe "" (\e -> " error: " <> toText e) mErr
  , "\n"
  ]

tableHeader, tableSeparator :: Text
tableHeader    = "| Case | Allocated | GCs | Live | Max |"
tableSeparator = "|:-----|----------:|----:|-----:|----:|"

-- | Strip the group prefix from a benchmark label.
--
-- weigh stores labels with full paths (e.g., "/Scope construction/singleton").
-- Since groups are already displayed as headers, we strip the prefix to avoid
-- redundancy in table rows.
stripGroupPrefix :: String -> String
stripGroupPrefix s = case List.dropWhile (/= '/') (drop 1 s) of
  '/':rest -> rest
  _        -> s

-- | Format a number with comma separators for readability.
formatWithCommas :: Word64 -> Text
formatWithCommas = toText . reverse . List.intercalate "," . chunksOf 3 . reverse . show
  where
    chunksOf _ [] = []
    chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b


-- ============================================================================
-- Benchmarks
-- ============================================================================

-- | All benchmark definitions.
benchmarks :: Weigh ()
benchmarks = do
  scopeBenchmarks
  vectorBenchmarks
  hashMapBenchmarks
  singletonDispatchBenchmarks
  evaluationBenchmarks

-- | Benchmarks for Scope construction and lookup.
scopeBenchmarks :: Weigh ()
scopeBenchmarks = do
  wgroup "Scope construction" $ do
    func "singleton" mkSingletonScope (fromString "x", 1 :: Int)
    func "10 bindings" (mkScopeN 10) (1 :: Int)
    func "100 bindings" (mkScopeN 100) (1 :: Int)
    func "1000 bindings" (mkScopeN 1000) (1 :: Int)

  wgroup "Scope lookup" $ do
    let scope10 = mkScopeN 10 (1 :: Int)
        scope100 = mkScopeN 100 (1 :: Int)
        scope1000 = mkScopeN 1000 (1 :: Int)
        key = fromString "var_5"

    func "10 bindings (hit)" (scopeLookup key) [scope10]
    func "100 bindings (hit)" (scopeLookup key) [scope100]
    func "1000 bindings (hit)" (scopeLookup key) [scope1000]
    func "10 bindings (miss)" (scopeLookup (fromString "missing")) [scope10]
    func "depth 1" (scopeLookup key) [scope10]
    func "depth 5" (scopeLookup key) (replicate 5 scope10)
    func "depth 10" (scopeLookup key) (replicate 10 scope10)

-- | Benchmarks for Vector (used by NVListF for Nix lists).
vectorBenchmarks :: Weigh ()
vectorBenchmarks = do
  wgroup "Vector construction" $ do
    func "singleton" V.singleton (1 :: Int)
    func "fromList 10" V.fromList ([1..10] :: [Int])
    func "fromList 100" V.fromList ([1..100] :: [Int])
    func "fromList 1000" V.fromList ([1..1000] :: [Int])

  wgroup "Vector operations" $ do
    let v100 = V.fromList [1..100 :: Int]
        v1000 = V.fromList [1..1000 :: Int]
    func "index (100)" (V.! 50) v100
    func "index (1000)" (V.! 500) v1000
    func "length (100)" V.length v100
    func "length (1000)" V.length v1000

-- | Benchmarks for HashMap (used by AttrSet for Nix attribute sets).
hashMapBenchmarks :: Weigh ()
hashMapBenchmarks = do
  wgroup "HashMap construction" $ do
    func "singleton" (uncurry HM.singleton) (fromString "x" :: VarName, 1 :: Int)
    func "fromList 10" HM.fromList (mkPairs 10)
    func "fromList 100" HM.fromList (mkPairs 100)
    func "fromList 1000" HM.fromList (mkPairs 1000)

  wgroup "HashMap lookup" $ do
    let hm10 = HM.fromList (mkPairs 10)
        hm100 = HM.fromList (mkPairs 100)
        hm1000 = HM.fromList (mkPairs 1000)
        key = fromString "var_5"
    func "10 bindings" (HM.lookup key) hm10
    func "100 bindings" (HM.lookup key) hm100
    func "1000 bindings" (HM.lookup key) hm1000

-- | Benchmarks verifying zero-cost singleton type dispatch.
singletonDispatchBenchmarks :: Weigh ()
singletonDispatchBenchmarks =
  wgroup "Singleton dispatch" $ do
    func "SBool @'False" (caseOnSBool @'False) (1 :: Int, 2)
    func "SBool @'True" (caseOnSBool @'True) (1 :: Int, 2)

-- | Benchmarks for Nix expression evaluation.
evaluationBenchmarks :: Weigh ()
evaluationBenchmarks = do
  wgroup "Eval: literals" $ do
    io "integer" evalNix "42"
    io "float" evalNix "3.14159"
    io "string" evalNix "\"hello world\""
    io "path" evalNix "./."
    io "null" evalNix "null"
    io "true" evalNix "true"
    io "false" evalNix "false"

  wgroup "Eval: arithmetic" $ do
    io "add" evalNix "1 + 2"
    io "subtract" evalNix "10 - 3"
    io "multiply" evalNix "6 * 7"
    io "divide" evalNix "builtins.div 100 4"
    io "negate" evalNix "- 42"
    io "compound" evalNix "(1 + 2) * (3 + 4)"

  wgroup "Eval: lists" $ do
    io "empty" evalNix "[]"
    io "singleton" evalNix "[1]"
    io "10 elements" evalNix "[1 2 3 4 5 6 7 8 9 10]"
    io "concat" evalNix "[1 2] ++ [3 4]"
    io "length" evalNix "builtins.length [1 2 3 4 5]"
    io "head" evalNix "builtins.head [1 2 3]"
    io "tail" evalNix "builtins.tail [1 2 3]"
    io "elemAt" evalNix "builtins.elemAt [1 2 3 4 5] 2"

  wgroup "Eval: attrsets" $ do
    io "empty" evalNix "{}"
    io "singleton" evalNix "{ x = 1; }"
    io "5 attrs" evalNix "{ a = 1; b = 2; c = 3; d = 4; e = 5; }"
    io "access" evalNix "{ x = 42; }.x"
    io "nested" evalNix "{ a = { b = { c = 1; }; }; }.a.b.c"
    io "or default" evalNix "{ x = 1; }.y or 42"
    io "hasAttr true" evalNix "{ x = 1; } ? x"
    io "hasAttr false" evalNix "{ x = 1; } ? y"
    io "merge (//)" evalNix "{ a = 1; } // { b = 2; }"

  wgroup "Eval: functions" $ do
    io "identity" evalNix "(x: x) 42"
    io "const" evalNix "(x: y: x) 1 2"
    io "add" evalNix "(a: b: a + b) 3 4"
    io "pattern" evalNix "({ x, y }: x + y) { x = 1; y = 2; }"
    io "pattern default" evalNix "({ x, y ? 10 }: x + y) { x = 1; }"
    io "pattern @" evalNix "({ x, ... }@args: x) { x = 1; y = 2; }"

  wgroup "Eval: let" $ do
    io "simple" evalNix "let x = 1; in x"
    io "5 bindings" evalNix "let a = 1; b = 2; c = 3; d = 4; e = 5; in a + b + c + d + e"
    io "shadowing" evalNix "let x = 1; in let x = 2; in x"
    io "recursive" evalNix "let fac = n: if n <= 1 then 1 else n * fac (n - 1); in fac 5"

  wgroup "Eval: conditionals" $ do
    io "if true" evalNix "if true then 1 else 2"
    io "if false" evalNix "if false then 1 else 2"
    io "comparison" evalNix "if 1 < 2 then \"yes\" else \"no\""
    io "nested" evalNix "if true then (if false then 1 else 2) else 3"

  wgroup "Eval: with" $ do
    io "simple" evalNix "with { x = 1; }; x"
    io "nested" evalNix "with { x = 1; }; with { y = 2; }; x + y"
    io "shadowed by let" evalNix "let x = 1; in with { x = 2; }; x"

  wgroup "Eval: strings" $ do
    io "concat" evalNix "\"hello\" + \" \" + \"world\""
    io "interpolation" evalNix "let x = \"world\"; in \"hello ${x}\""
    io "multiline" evalNix "''\n  line1\n  line2\n''"
    io "stringLength" evalNix "builtins.stringLength \"hello\""
    io "substring" evalNix "builtins.substring 0 5 \"hello world\""

  wgroup "Eval: builtins" $ do
    io "toString" evalNix "builtins.toString 42"
    io "typeOf" evalNix "builtins.typeOf 42"
    io "isInt" evalNix "builtins.isInt 42"
    io "attrNames" evalNix "builtins.attrNames { b = 1; a = 2; c = 3; }"
    io "attrValues" evalNix "builtins.attrValues { a = 1; b = 2; c = 3; }"
    io "map" evalNix "builtins.map (x: x * 2) [1 2 3]"
    io "filter" evalNix "builtins.filter (x: x > 2) [1 2 3 4 5]"
    io "foldl'" evalNix "builtins.foldl' (a: b: a + b) 0 [1 2 3 4 5]"
    io "genList" evalNix "builtins.genList (x: x * x) 5"
    io "listToAttrs" evalNix "builtins.listToAttrs [{ name = \"x\"; value = 1; }]"

  wgroup "Eval: recursion" $ do
    io "factorial 5" evalNix "let fac = n: if n <= 1 then 1 else n * fac (n - 1); in fac 5"
    io "factorial 10" evalNix "let fac = n: if n <= 1 then 1 else n * fac (n - 1); in fac 10"
    io "fibonacci 10" evalNix "let fib = n: if n <= 1 then n else fib (n - 1) + fib (n - 2); in fib 10"
    io "fibonacci 15" evalNix "let fib = n: if n <= 1 then n else fib (n - 1) + fib (n - 2); in fib 15"

  wgroup "Eval: laziness" $ do
    io "unused thunk" evalNix "let unused = builtins.throw \"not evaluated\"; in 42"
    io "short-circuit &&" evalNix "false && builtins.throw \"not evaluated\""
    io "short-circuit ||" evalNix "true || builtins.throw \"not evaluated\""
    io "short-circuit ->" evalNix "false -> builtins.throw \"not evaluated\""

  wgroup "Eval: rec" $ do
    io "simple" evalNix "rec { x = 1; y = x + 1; }.y"
    io "mutual" evalNix "rec { a = b + 1; b = 1; }.a"
    io "inherit" evalNix "let x = 1; in { inherit x; }.x"
    io "inherit from" evalNix "let s = { x = 1; }; in { inherit (s) x; }.x"


-- ============================================================================
-- Helpers
-- ============================================================================

-- | Type aliases for the standard evaluation monad (no stats, default config).
type StandardIO = StdM 'False DefaultCfg IO
type StdVal = ValueF 'False StandardIO
type StdThun = ThunkF 'False StandardIO

-- | Evaluate a Nix expression to normal form.
--
-- Returns @()@ since @NValue@ lacks @NFData@, but weigh measures allocation
-- during the IO action regardless of the return type.
evalNix :: Text -> IO ()
evalNix src = do
  time <- getCurrentTime
  let opts = defaultOptions time
  case parseNixText src of
    Left err -> errorWithoutStackTrace $ "Parse error: " <> show err
    Right expr -> do
      !_ <- runWithBasicEffects opts $
        (normalForm =<< nixEvalExpr mempty expr)
          `catch` \case
            NixException frames ->
              errorWithoutStackTrace . show
                =<< renderFrames @StdVal @StdThun frames
      pure ()

-- | Create a scope with a single binding.
mkSingletonScope :: (VarName, a) -> Scope a
mkSingletonScope (k, v) = Scope (HM.singleton k v)

-- | Create a scope with @n@ bindings named @var_1@ through @var_n@.
mkScopeN :: Int -> a -> Scope a
mkScopeN n v = Scope $ HM.fromList [(fromString ("var_" <> show i), v) | i <- [1..n]]

-- | Create @n@ key-value pairs for HashMap benchmarks.
mkPairs :: Int -> [(VarName, Int)]
mkPairs n = [(fromString ("var_" <> show i), i) | i <- [1..n]]

-- | Dispatch on a singleton boolean. Used to verify zero-cost type-level dispatch.
--
-- NOINLINE prevents compile-time elimination, ensuring we measure runtime dispatch.
caseOnSBool :: forall (b :: Bool) a. SBoolI b => (a, a) -> a
caseOnSBool (t, f) = case sbool @b of
  STrue  -> t
  SFalse -> f
{-# NOINLINE caseOnSBool #-}
