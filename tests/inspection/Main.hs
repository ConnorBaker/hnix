{-# LANGUAGE TemplateHaskell #-}

-- | Inspection tests for hnix evaluator specialization.
--
-- These tests use @inspection-testing@ to verify that GHC's optimizations
-- produce the expected zero-overhead code for the common evaluation case:
--
-- - Provenance disabled (@prov ~ 'False@)
-- - Default configuration (@cfg ~ DefaultCfg@)
--
-- == What We Test
--
-- 1. __Singleton dispatch elimination__: When @sbool @'False@ is used in a case
--    expression, GHC should eliminate the @STrue@ branch entirely via
--    case-of-known-constructor optimization.
--
-- 2. __Newtype erasure__: The @Cited@, @CitedF@, and @ThunkF@ newtype wrappers
--    should be completely erased in the generated code, leaving no runtime
--    indirection.
--
-- 3. __Type class specialization__: No @SBoolI@ dictionaries should remain
--    in the generated code for concrete type applications.
--
-- 4. __Provenance type elimination__: Types like @NCited@, @Provenance@, and
--    @Identity@ should not appear in the Core for @prov ~ 'False@ code paths.
--
-- 5. __Instance method specialization__: Functor, Applicative, Comonad,
--    Foldable, Traversable, and HasCitations instances should all specialize.
--
-- 6. __Config dispatch__: All config flags (stats, prov, trace) should dispatch
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
import           Inspection.Cited       ()
import           Inspection.Coerce      ()
import           Inspection.Comonad     ()
import           Inspection.Config      ()
import           Inspection.Functor     ()
import           Inspection.HasCitations ()
import           Inspection.Integration ()
import           Inspection.Singleton   ()
import           Inspection.Thunk       ()

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
  putStrLn "Test modules (106 total compile-time tests):"
  putStrLn ""
  putStrLn "1. Cited module (5 tests):"
  putStrLn "   - extractCited/provenanceCited specialization"
  putStrLn "   - No SBoolI dictionaries, NCited, Identity types"
  putStrLn ""
  putStrLn "2. Singleton module (8 tests):"
  putStrLn "   - sbool @'False/@'True branch elimination"
  putStrLn "   - ifSBool helper specialization"
  putStrLn ""
  putStrLn "3. Thunk module (9 tests):"
  putStrLn "   - CitedF extract/fmap specialization"
  putStrLn "   - No provenance types in generated Core"
  putStrLn ""
  putStrLn "4. Integration module (6 tests):"
  putStrLn "   - Full wrapper chain erasure"
  putStrLn "   - Config dispatch specialization"
  putStrLn ""
  putStrLn "5. Comonad module (14 tests):"
  putStrLn "   - extract/duplicate for Cited and CitedF"
  putStrLn "   - No type class dictionaries or provenance types"
  putStrLn ""
  putStrLn "6. Functor module (26 tests):"
  putStrLn "   - Functor, Applicative, Foldable, Traversable instances"
  putStrLn "   - For both Cited and CitedF types"
  putStrLn ""
  putStrLn "7. HasCitations module (18 tests):"
  putStrLn "   - citations1/addProvenance1 for Cited, CitedF, ThunkF"
  putStrLn "   - No SBoolI dictionaries, NCited, Identity types"
  putStrLn ""
  putStrLn "8. Coerce module (7 tests):"
  putStrLn "   - Newtype coercion zero-cost verification"
  putStrLn "   - Roundtrip coercion is identity"
  putStrLn ""
  putStrLn "9. Config module (13 tests):"
  putStrLn "   - singStats/singProv/singTrace dispatch"
  putStrLn "   - Combined flag dispatch (all DefaultCfg flags false)"
  putStrLn ""
  putStrLn "All inspection tests passed!"
