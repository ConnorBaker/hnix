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
import           Inspection.AttrSet     ()
import           Inspection.Cited       ()
import           Inspection.Coerce      ()
import           Inspection.Comonad     ()
import           Inspection.Config      ()
import           Inspection.Convert     ()
import           Inspection.Functor     ()
import           Inspection.HasCitations ()
import           Inspection.Integration ()
import           Inspection.MonadThunk  ()
import           Inspection.NixString   ()
import           Inspection.Protocol    ()
import           Inspection.Scope       ()
import           Inspection.Singleton   ()
import           Inspection.Thunk       ()
import           Inspection.Value       ()

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
  putStrLn "Core Cited/Provenance tests:"
  putStrLn "  1. Cited      - extractCited/provenanceCited specialization"
  putStrLn "  2. Coerce     - Newtype coercion zero-cost verification"
  putStrLn "  3. Comonad    - extract/duplicate for Cited and CitedF"
  putStrLn "  4. Functor    - Functor, Applicative, Foldable, Traversable"
  putStrLn "  5. HasCitations - citations1/addProvenance1 specialization"
  putStrLn ""
  putStrLn "Configuration and dispatch tests:"
  putStrLn "  6. Config     - singStats/singProv/singTrace dispatch"
  putStrLn "  7. Singleton  - sbool @'False/@'True branch elimination"
  putStrLn ""
  putStrLn "Evaluator component tests:"
  putStrLn "  8. Thunk      - CitedF extract/fmap specialization"
  putStrLn "  9. Integration - Full wrapper chain erasure"
  putStrLn "  10. MonadThunk - thunk/force/query operations"
  putStrLn "  11. Value     - NValue construction and extraction"
  putStrLn "  12. Convert   - Coercion and unwrapping operations"
  putStrLn ""
  putStrLn "Data structure tests:"
  putStrLn "  13. Scope     - scopeLookup efficiency"
  putStrLn "  14. NixString - String construction and extraction"
  putStrLn "  15. AttrSet   - Attribute set pattern matching and construction"
  putStrLn "  16. Protocol  - Protocol operations for builtins (list/attrset)"
  putStrLn ""
  putStrLn "All inspection tests passed!"
  putStrLn "(Actual test counts verified at compile time)"
