# HNix Value Representation Optimization Plan

## Executive Summary

This document tracks the implementation of optimizations to reduce memory usage and indirection in the HNix evaluator while preserving the flexibility of recursion schemes and ADI.

**Benchmark command:**
```bash
./result/bin/hnix --store-mode overlay --no-store-read-through --eval \
  --expr '(import /home/connorbaker/nixpkgs/nixos/release.nix {}).closures.gnome.x86_64-linux.drvPath' \
  +RTS -s -RTS
```

---

## Benchmark Log

| Date | Phase | Allocations | Max Heap | GC Time | Total Time | Productivity | Notes |
|------|-------|-------------|----------|---------|------------|--------------|-------|
| Baseline | None | 1.163 TiB | 9.38 GiB | 131.9s | 340.3s | 61.2% | Pre-optimization |
| 2026-01-16 | Phase 1 | 1.164 TiB | 9.38 GiB | 134.7s | 340.5s | 60.4% | Within noise (~2%) |

---

## Phase 1: Quick Wins (COMPLETED)

**Status:** Completed, no measurable impact

### Changes Made
1. **UNPACK pragmas** for hot structures:
   - `NThunkF`: Unpacked `ThunkId` field
   - `NAtom`: Unpacked `Int64` and `Double` fields
   - `NPos`/`NSourcePos`: Unpacked position fields

2. **RULES pragmas** for ADI transform fusion:
   - Added `"adi/id"` rule: `adi id f = cata f`
   - Added `cata` catamorphism function

3. **INLINABLE pragmas** for polymorphic hot functions:
   - `Nix.Eval`: `eval`, `evalWithAttrSet`, `attrSetAlter`, `evalBinds`, `evalSelect`
   - `Nix.Normal`: `normalizeValue`, `normalForm`, `stubCycles`, `removeEffects`, etc.
   - `Nix.Value`: `iterNValue`, `iterNValueM`, etc.
   - `Nix.Utils`: `adi`, `cata`

### Results
No measurable improvement. Analysis:
- UNPACK savings (~7-14 MB) are negligible against 1.06 TiB total
- INLINABLE doesn't help without concrete type visibility at call sites
- RULES pattern `adi id f` rarely occurs in practice

### Files Modified
- `src/Nix/Thunk/Basic.hs`
- `src/Nix/Atoms.hs`
- `src/Nix/Expr/Types.hs`
- `src/Nix/Utils.hs`
- `src/Nix/Eval.hs`
- `src/Nix/Normal.hs`
- `src/Nix/Value.hs`

---

## Phase 2: Type-Level Provenance Elimination (COMPLETED)

**Status:** Fully integrated using explicit `prov :: Bool` type parameter

**Expected Impact:** 15-25% memory reduction when provenance disabled

**Current State:** Following the approach from `phase2-best-path.md`, provenance is now
an explicit type parameter of `StandardTF` and all related types. This eliminates the
GHC type family reduction issues entirely by making `prov` a visible type parameter
rather than extracting it from `cfg` via type families.

### Goal
Use type-level provenance parameter to select the annotation functor at compile time,
eliminating `[Provenance]` list allocation when provenance is disabled.

### Implementation (Completed 2026-01-17)

**Approach:** Explicit `prov :: Bool` type parameter (from `phase2-best-path.md`)

Instead of using type families to extract provenance from `cfg`, we make `prov` an
explicit type parameter of `StandardTF` and all related types. This avoids GHC's
inability to reduce type families under existential quantification.

1. **Provenance-indexed types** (`src/Nix/Standard.hs`):
   ```haskell
   -- Cited functor parameterized by provenance
   newtype CitedF (prov :: Bool) m a =
     CitedF (Cited prov (ThunkF prov m) (CitedF prov m) m a)

   -- Thunk type parameterized by provenance
   newtype ThunkF (prov :: Bool) m =
     ThunkF (CitedF prov m (NThunkF m (ValueF prov m)))

   -- Value type parameterized by provenance
   type ValueF (prov :: Bool) m = NValue (ThunkF prov m) (CitedF prov m) m
   ```

2. **Parameterized StandardTF** (`src/Nix/Standard.hs`):
   ```haskell
   newtype StandardTF (prov :: Bool) (cfg :: EvalCfg) r m a
     = StandardTF
         (ReaderT
           (Context cfg r (ValueF prov r))
           (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
           a
         )

   type StandardT (prov :: Bool) (cfg :: EvalCfg) m = Fix1T (StandardTF prov cfg) m
   type StdM (prov :: Bool) (cfg :: EvalCfg) m = StandardT prov cfg (StdIdT m)
   ```

3. **Unified instances with singleton dispatch** (`src/Nix/Cited/Basic.hs`, `src/Nix/Standard.hs`):
   - Single `MonadThunk (Cited prov u f m t) m v` instance using `sbool @prov`
   - Single `MonadThunk (ThunkF prov m) m (ValueF prov m)` instance
   - Single `MonadValue (ValueF prov m) m` instance
   - Single `Scoped (ValueF prov m) m` instance
   - Single `MonadEffects (ThunkF prov m) (CitedF prov m) m` instance

4. **Top-level branching** (`src/Nix/Standard.hs`):
   ```haskell
   runWithStoreEffectsIOT
     :: forall (cfg :: EvalCfg) a. KnownEvalCfg cfg
     => Options
     -> (forall (prov :: Bool) m. (...) => StdM prov cfg m a)
     -> IO a
   runWithStoreEffectsIOT opts action =
     case singProv @cfg of
       STrue  -> runWithStoreEffectsIOT' @'True  @cfg opts action
       SFalse -> runWithStoreEffectsIOT' @'False @cfg opts action
   ```

### Historical Notes: Instance Resolution Issues (RESOLVED)

During early implementation attempts, general instances caused `<unknown location>` errors
in provenance tracking. The root cause was that general instances like
`HasCitations1 m (NValue t f m) (SomeWrapper t f m)` created ambiguity during GHC's
constraint solving, even when not directly used.

**Solution:** The unified `Cited prov t f m a` approach avoids this entirely by having
a single parameterized type instead of separate wrapper types with potentially
conflicting instances.

### Historical Note: Critical Finding #5 (Type Family Reduction)

Before implementing the explicit `prov` parameter approach, we attempted to use type
families to select types based on `CfgProv cfg`. This was blocked because GHC cannot
reduce type families like `SelValue (CfgProv cfg) m` when `cfg` is existentially bound
by `withEvalCfg`.

**Solution:** The explicit `prov :: Bool` type parameter approach from `phase2-best-path.md`
avoids this issue entirely. Instead of extracting `prov` from `cfg` via type families,
we make `prov` a visible type parameter and branch on it at the top level.

### Completed Work

1. ✅ Root cause of instance resolution issues identified (Critical Finding #4)
2. ✅ Initial Lite instances work (proved the concept)
3. ✅ GHC limitations understood, unified approach designed
4. ✅ Implemented `CitedRep` type family in `Cited/Basic.hs`
5. ✅ Updated `Cited` to take `prov` parameter with singleton dispatch
6. ✅ Updated `StdCited`/`LiteCited` wrappers in `Standard.hs`
7. ✅ Type selection aliases defined
8. ✅ Integration attempted via type families - identified Critical Finding #5
9. ✅ **Implemented explicit `prov` parameter approach** (from `phase2-best-path.md`):
   - `StandardTF` now takes `prov :: Bool` as first type parameter
   - `runWithStoreEffectsIOT` branches on `singProv @cfg`
   - Unified `MonadThunk (Cited prov ...)` instance with singleton dispatch
   - Unified `MonadThunk (ThunkF prov ...)` instance
   - Unified `MonadValue (ValueF prov ...)` instance
   - Updated `Main.hs` type signatures
   - Updated `TestCommon.hs` for tests
10. ✅ All 416/418 tests passing (2 pre-existing failures unrelated to types)
11. ✅ Provenance tracking verified working (error messages show source locations)

### Why the Explicit `prov` Approach Works

The key insight from `phase2-best-path.md` is that making `prov` an explicit type
parameter eliminates the type family reduction problem entirely:

1. **No type families in instance heads**: Instances are for `ThunkF prov m`, not
   `SelThunk (CfgProv cfg) m`, so GHC doesn't need to reduce type families
2. **Polymorphic action works**: The action passed to `runWithStoreEffectsIOT` is
   polymorphic in `prov`, and the top-level branch instantiates it concretely
3. **Single instance set**: Unified instances with `SBoolI prov` constraint work
   for both `'True` and `'False` via singleton dispatch
4. **Zero runtime overhead**: When `prov` is known at compile time (after the
   top-level branch), GHC eliminates dead branches via specialization

### Key Files
- `src/Nix/Cited/Basic.hs` - Unified `Cited prov t f m a` with `CitedRep` type family
- `src/Nix/Standard.hs` - `StdCited`/`LiteCited` wrappers, type families, instances

---

## Phase 3: Church-Encoded Free Monad (PLANNED)

**Expected Impact:** 5-15% improvement for normalization

### Goal
Introduce Church-encoded variant for bind-heavy operations.

---

## Phase 4: Advanced Optimizations (PLANNED)

- UnboxedSums for NValueF (requires GHC 9.2+)
- Defunctionalized closures
- Compact regions for normalized values

---

## Target Metrics

| Metric | Baseline | Target | Status |
|--------|----------|--------|--------|
| Total allocations | 1.06 TiB | <0.95 TiB (-10%) | Pending |
| Max heap size | 9.4 GiB | <8 GiB (-15%) | Pending |
| GC time | 132s | <120s | Pending |
| Productivity | 61.1% | >65% | Pending |
