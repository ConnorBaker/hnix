{-# LANGUAGE TemplateHaskell #-}

-- | Inspection tests for hnix evaluator specialization.
--
-- These tests use @inspection-testing@ to verify that GHC's optimizations
-- produce the expected zero-overhead code for the default configuration:
--
-- - Default configuration (@cfg ~ DefaultCfg@)
--
-- == What We Test
--
-- 1. __Singleton dispatch elimination__: When @sbool @'False@ is used in a case
--    expression, GHC should eliminate the @STrue@ branch entirely via
--    case-of-known-constructor optimization.
--
-- 2. __Type class specialization__: No @SBoolI@ dictionaries should remain
--    in the generated code for concrete type applications.
--
-- 3. __Config dispatch__: All config flags (stats, trace) should dispatch
--    without runtime overhead when using DefaultCfg.
--
-- == Running the Tests
--
-- @
-- nix develop ".?submodules=1#" --command cabal test hnix-inspection
-- @
--
-- On failure, inspection-testing shows the GHC Core that violated the property,
-- which is invaluable for debugging optimization issues.
--
-- == Notes on Test Design
--
-- - All test wrapper functions use @NOINLINE@ to prevent GHC from inlining
--   them before inspection-testing can analyze the Core.
--
-- - The @inspect@ TH function registers tests that are checked at compile time.
--   If any test fails, compilation fails with a detailed error message showing
--   the problematic Core.
--
-- - Tests use @==-@ (equivalence modulo renaming) rather than @===@ (exact
--   equality) because GHC may rename variables.
module Main (main) where

import           Relude

-- Import test modules to trigger their compile-time inspection checks.
-- These imports are "redundant" in terms of runtime use, but the act of
-- compiling them triggers the inspection tests via Template Haskell.
import           Inspection.Config      ()
import           Inspection.NixString   ()
import           Inspection.Scope       ()
import           Inspection.Singleton   ()

-- | Run the inspection tests.
--
-- Since inspection-testing checks are performed at compile time, if we reach
-- this point, all tests have passed. This main function just reports success.
main :: IO ()
main = do
  putStrLn "hnix inspection tests"
  putStrLn "====================="
  putStrLn ""
  putStrLn "All inspection tests verified at compile time!"
  putStrLn ""
  putStrLn "Test modules:"
  putStrLn ""
  putStrLn "Configuration and dispatch tests:"
  putStrLn "  1. Config     - singStats/singTrace dispatch"
  putStrLn "  2. Singleton  - sbool @'False/@'True branch elimination"
  putStrLn ""
  putStrLn "Data structure tests:"
  putStrLn "  3. Scope     - scopeLookup efficiency"
  putStrLn "  4. NixString - String construction and extraction"
  putStrLn ""
  putStrLn "All inspection tests passed!"
  putStrLn "(Actual test counts verified at compile time)"
