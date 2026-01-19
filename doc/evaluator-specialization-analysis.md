# HNix Evaluator Specialization Analysis

This document provides a thorough analysis of the HNix evaluator structure, current limitations regarding specialization, and approaches that have been tried to improve compile-time specialization.

## Table of Contents

1. [Evaluator Architecture Overview](#evaluator-architecture-overview)
2. [Type-Level Provenance System](#type-level-provenance-system)
3. [Singleton Dispatch Mechanism](#singleton-dispatch-mechanism)
4. [The Specialization Problem](#the-specialization-problem)
5. [Approaches Tried](#approaches-tried)
6. [Current State and Measurements](#current-state-and-measurements)
7. [Potential Future Directions](#potential-future-directions)

---

## Evaluator Architecture Overview

### Core Type Hierarchy

The HNix evaluator uses a sophisticated type system to support optional provenance tracking at the type level. The key types form a layered hierarchy:

```
┌─────────────────────────────────────────────────────────────┐
│                      User-Facing Types                       │
│  StdM prov cfg m  - The standard evaluation monad           │
│  ValueF prov m    - Nix values with provenance parameter    │
│  ThunkF prov m    - Thunks with provenance parameter        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Cited Layer (Standard.hs)                 │
│  CitedF prov m a  - Newtype wrapping Cited                  │
│  Wraps: Cited prov (ThunkF prov m) (CitedF prov m) m a     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                 Core Cited Type (Cited/Basic.hs)            │
│  Cited prov t f m a                                         │
│  Uses CitedRep type family for representation selection     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Representation Layer                       │
│  When prov ~ 'True:  NCited m v a (provenance + value)     │
│  When prov ~ 'False: Identity a   (just value, zero cost)  │
└─────────────────────────────────────────────────────────────┘
```

### Key Type Definitions

```haskell
-- Type family selecting representation based on provenance flag
type family CitedRep (prov :: Bool) m v a where
  CitedRep 'True  m v a = NCited m v a    -- Full provenance tracking
  CitedRep 'False m v a = Identity a      -- Zero overhead

-- The Cited newtype wraps the selected representation
newtype Cited (prov :: Bool) t f m a =
  Cited { runCited :: CitedRep prov m (NValue t f m) a }

-- NCited stores provenance list alongside the value
data NCited m v a = NCited ![Provenance m v] a
```

### Effect System

The evaluator uses MTL-style type classes for effects:

```haskell
class MonadThunk t m a | t -> m, t -> a where
  thunk    :: m a -> m t           -- Create lazy thunk
  thunkId  :: t -> ThunkId m       -- Get thunk identifier
  query    :: m a -> t -> m a      -- Query without forcing
  force    :: t -> m a             -- Force evaluation
  forceEff :: t -> m a             -- Force with effects
  further  :: t -> m t             -- Transform thunk action

class MonadEval v m where
  evalExprLoc :: NExprLoc -> m v   -- Evaluate expression

class (MonadEval v m, MonadThunk t m v) => MonadNix e t f m
```

---

## Type-Level Provenance System

### Design Goals

The provenance system was designed to:
1. **Track evaluation context** when `--thunks` flag is enabled
2. **Have zero runtime cost** when provenance is disabled
3. **Use a single codebase** for both modes (no code duplication)

### How It Works

The `prov :: Bool` type parameter flows through the entire evaluation stack:

```haskell
-- All major types are parameterized by prov
type ValueF  (prov :: Bool) m = NValue  (ThunkF prov m) (CitedF prov m) m
type ThunkF  (prov :: Bool) m = ...
type CitedF  (prov :: Bool) m = ...
type StdM    (prov :: Bool) (cfg :: EvalCfg) m = ...
```

When `prov ~ 'True`:
- `CitedRep` resolves to `NCited`, which stores `[Provenance m v]`
- Thunk creation captures the current evaluation frame
- Error messages include full provenance chains

When `prov ~ 'False`:
- `CitedRep` resolves to `Identity`, a zero-cost wrapper
- No provenance is stored or tracked
- Newtypes are erased at compile time

---

## Singleton Dispatch Mechanism

### The SBoolI Type Class

Since type families cannot be directly pattern-matched at runtime, HNix uses singleton types to bridge type-level and term-level information:

```haskell
-- Singleton type for Bool
data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

-- Type class providing singleton witness
class SBoolI (b :: Bool) where
  sbool :: SBool b

instance SBoolI 'True  where sbool = STrue
instance SBoolI 'False where sbool = SFalse
```

### Runtime Dispatch Pattern

Code that needs to behave differently based on `prov` uses `sbool` for dispatch:

```haskell
extractCited :: forall prov t f m a. SBoolI prov
             => Cited prov t f m a -> a
extractCited (Cited rep) = case sbool @prov of
  STrue  -> getCited rep      -- rep :: NCited m v a
  SFalse -> runIdentity rep   -- rep :: Identity a
```

### The reifyBoolT Bridge

The `prov` value is determined at runtime from command-line options. The `reifyBoolT` function bridges this runtime value to the type level using continuation-passing style:

```haskell
reifyBoolT :: forall r. Bool
           -> (forall b. (SBoolI b, Typeable b) => Proxy b -> r)
           -> r
reifyBoolT True  k = k (Proxy @'True)
reifyBoolT False k = k (Proxy @'False)
```

Usage in the evaluator entry point:

```haskell
withEvalCfg :: Bool -> Bool -> Bool
            -> (forall prov cfg. ... => Proxy cfg -> r)
            -> r
withEvalCfg stats prov tracing k =
  reifyBoolT stats $ \(_ :: Proxy s) ->
    reifyBoolT prov $ \(_ :: Proxy p) ->
      reifyBoolT tracing $ \(_ :: Proxy t) ->
        k @(MkEvalCfg s p t) Proxy
```

---

## The Specialization Problem

### Why Specialization Fails

GHC's specialization machinery cannot specialize across the `reifyBoolT` boundary because:

1. **Existential Quantification**: The continuation passed to `reifyBoolT` is polymorphic in `prov`. Even though we pass concrete dictionaries at runtime, GHC compiles the continuation polymorphically.

2. **Dictionary Passing**: Every function polymorphic in `prov` receives an `SBoolI` dictionary at runtime, even when the concrete type is known in a specific call path.

3. **No Cross-Module Specialization**: GHC cannot see through the existential to specialize downstream code.

### Visualization of the Problem

```
                    Runtime Decision
                          │
                          ▼
              ┌───────────────────────┐
              │     reifyBoolT        │
              │  prov = True/False    │
              └───────────────────────┘
                          │
            ┌─────────────┴─────────────┐
            │                           │
            ▼                           ▼
    k (Proxy @'True)            k (Proxy @'False)
    with SBoolI 'True           with SBoolI 'False
            │                           │
            └─────────────┬─────────────┘
                          │
                          ▼
              ┌───────────────────────┐
              │  Continuation k is    │
              │  compiled ONCE with   │
              │  polymorphic prov     │
              └───────────────────────┘
                          │
                          ▼
              ┌───────────────────────┐
              │  sbool dispatch at    │
              │  EVERY call site      │
              └───────────────────────┘
```

### GHC Core Evidence

Analysis of the GHC Core output shows dictionary passing throughout:

```haskell
-- From Standard.dump-simpl
$fMonadThunkThunkFmFree_$cforce
  = \ @m_arrn
      @prov_arro           -- prov is still polymorphic
      @cfg_arrp
      $dSBoolI_arrw        -- Dictionary passed at runtime
      ...
```

And runtime `sbool` dispatch:

```haskell
-- From Cited/Basic.dump-simpl
$w$cforce = \ ... $dSBoolI_solO eta_solP ->
  case sbool $dSBoolI_solO of {   -- Runtime dispatch
    STrue co_akKu -> ...
    SFalse co_akL0 -> ...
  }
```

---

## Approaches Tried

### 1. SPECIALIZE Pragmas on Functions

**Approach**: Add SPECIALIZE pragmas to standalone functions:

```haskell
{-# SPECIALIZE extractCited :: Cited 'True t f m a -> a #-}
{-# SPECIALIZE extractCited :: Cited 'False t f m a -> a #-}
```

**Result**: Partially successful. GHC generated specialized versions with USPEC rules:

```haskell
-- From Core output
"USPEC extractCited @'True @_ @_ @_ @_"
      = extractCited_$sextractCited
"USPEC extractCited @'False @_ @_ @_ @_"
      = extractCited_$sextractCited1
```

**Limitation**: These specialized versions are only used when the call site has a statically known `prov` type. Since `prov` comes from `reifyBoolT`, most call sites remain polymorphic.

### 2. Aggressive Specialization GHC Flags

**Approach**: Enable aggressive specialization flags in cabal:

```cabal
ghc-options:
  -fspecialise-aggressively
  -flate-specialise
  -fpolymorphic-specialisation
```

**Result**: **FAILED** - Caused evaluation bugs. The error manifested as:

```
lib.customisation.callPackageWith: Function called without required
argument "xlrd" at <unknown location>, did you mean "xlrd", "xld" or "dcrd"?
```

The flags changed strictness behavior or instance resolution in ways that broke lazy evaluation patterns. This is a known risk with aggressive optimization flags.

**Status**: Reverted.

### 3. Splitting MonadThunk Instance

**Approach**: Replace the unified instance with two separate instances:

```haskell
-- Instead of:
instance (SBoolI prov, ...) => MonadThunk (Cited prov ...) where ...

-- Use:
instance (...) => MonadThunk (Cited 'True ...) where ...
instance (...) => MonadThunk (Cited 'False ...) where ...
```

**Result**: **FAILED** - Compilation errors. The `Standard.hs` module has code polymorphic in `prov` that requires `MonadThunk (Cited prov ...)` to exist for any `prov`:

```haskell
-- Standard.hs requires this to work for any prov
instance (..., SBoolI prov) => MonadThunk (ThunkF prov m) where
  force t = force @(InnerCitedThunk prov m) (coerce t)
  -- ↑ Needs MonadThunk (Cited prov ...) for polymorphic prov
```

Splitting the Cited instance would require also splitting:
- `MonadThunk (ThunkF prov m)` instance
- `MonadEffects` instance
- Various type class instances in Standard.hs
- Potentially more downstream code

**Status**: Abandoned due to cascading changes required.

### 4. SPECIALIZE Instance Pragmas

**Approach**: Use `{-# SPECIALIZE instance #-}` pragmas:

```haskell
{-# SPECIALIZE instance MonadThunk (Cited 'True u f m t) m v #-}
{-# SPECIALIZE instance MonadThunk (Cited 'False u f m t) m v #-}
```

**Result**: **FAILED** - GHC error "Misplaced SPECIALIZE instance pragma". These pragmas have strict placement requirements and didn't work with the instance structure.

**Status**: Abandoned.

---

## Current State and Measurements

### Runtime Dispatch Sites

Analysis of GHC Core output shows **110 total `sbool` dispatch sites** across the codebase:

| Module | Count |
|--------|-------|
| Standard.hs | 35 |
| Cited/Basic.hs | 35 |
| Exec.hs | 34 |
| Config/Singleton.hs | 6 |

### Cost Analysis

Each `sbool` dispatch involves:
1. Load the `SBoolI` dictionary (~1 memory access)
2. Extract the singleton value (~1 indirect call)
3. Case dispatch on the singleton (~1 branch)

Estimated cost: **~5-10 nanoseconds per dispatch**

With 110 dispatch sites in hot paths, and millions of evaluations for large Nixpkgs expressions, this could add **measurable but likely acceptable overhead**.

### What IS Working

1. **Newtype elimination**: `cast` operations in Core show newtypes are erased
2. **Standalone function specialization**: `extractCited`, `provenanceCited` have specialized versions
3. **INLINABLE pragmas**: Enable cross-module inlining where beneficial

---

## Potential Future Directions

### 1. Accept Current State

The `sbool` dispatch overhead may be acceptable given:
- It's a simple case expression on a cached singleton
- The alternative (code duplication) has maintenance costs
- Actual evaluation work dominates runtime

**Recommendation**: Benchmark to quantify actual overhead before investing more effort.

### 2. Top-Level Code Duplication

Create two completely separate evaluation entry points:

```haskell
-- Explicitly specialized top-level functions
evalWithProvenance :: NExprLoc -> StdM 'True cfg m (ValueF 'True m)
evalWithProvenance = ...  -- Uses Cited 'True throughout

evalWithoutProvenance :: NExprLoc -> StdM 'False cfg m (ValueF 'False m)
evalWithoutProvenance = ...  -- Uses Cited 'False throughout
```

**Trade-offs**:
- (+) Eliminates all sbool dispatch
- (-) Significant code duplication
- (-) Maintenance burden doubles
- (-) Potential for divergence between implementations

### 3. GHC Plugin for Specialization

Write a GHC plugin that:
1. Detects the `reifyBoolT` pattern
2. Duplicates downstream code for each branch
3. Specializes appropriately

**Trade-offs**:
- (+) Automatic, no manual duplication
- (-) Complex to implement correctly
- (-) GHC version coupling
- (-) Build time increase

### 4. Runtime Dispatch Without Type Families

Replace type-level provenance with explicit runtime flag:

```haskell
data Cited' m v a
  = CitedProv ![Provenance m v] a
  | CitedLite a

-- Runtime check instead of type-level
force :: Bool -> Cited' m v a -> m v
force withProv (CitedProv ps t)
  | withProv  = handleProvenance ps $ force' t
  | otherwise = force' t
force _ (CitedLite t) = force' t
```

**Trade-offs**:
- (+) Simpler, no type-level complexity
- (-) Always pays for provenance field (even if unused)
- (-) Runtime check instead of compile-time elimination
- (-) Less type safety

### 5. Investigate GHC Improvements

The underlying issue is GHC's inability to specialize across existential boundaries. This could potentially be addressed by:
- Filing a GHC feature request for "call-site specialization"
- Investigating if newer GHC versions handle this better
- Exploring if `-fspecialise-aggressively` issues are fixable

---

## Successful Approach: Explicit Case Dispatch + Type Family Alignment

### The Problem (Two Parts)

The specialization problem had two related causes:

1. **`withEvalCfg` using existential quantification via `reifyBoolT`**: This prevented GHC from seeing concrete types in downstream code.

2. **`runWithStoreEffectsIOT` having a separate polymorphic `prov` parameter**: Even after fixing `withEvalCfg`, the `prov` type was being existentially quantified again in the runner function, preventing `CfgProv cfg` from being recognized as equal to the concrete `prov` value.

### Solution Part 1: Explicit Case Dispatch

Replacing `reifyBoolT` (which uses existential quantification) with explicit case dispatch enables GHC to specialize effectively:

```haskell
-- OLD: Existential quantification hides concrete types
withEvalCfg stats prov tracing k =
  reifyBoolT stats $ \(_ :: Proxy s) ->
    reifyBoolT prov $ \(_ :: Proxy p) ->
      reifyBoolT tracing $ \(_ :: Proxy t) ->
        k (Proxy @('MkEvalCfg s p t))

-- NEW: Explicit enumeration exposes concrete types
withEvalCfg stats prov tracing k = case (stats, prov, tracing) of
  (False, False, False) -> k (Proxy @('MkEvalCfg 'False 'False 'False))
  (False, False, True)  -> k (Proxy @('MkEvalCfg 'False 'False 'True))
  (False, True,  False) -> k (Proxy @('MkEvalCfg 'False 'True  'False))
  (False, True,  True)  -> k (Proxy @('MkEvalCfg 'False 'True  'True))
  (True,  False, False) -> k (Proxy @('MkEvalCfg 'True  'False 'False))
  (True,  False, True)  -> k (Proxy @('MkEvalCfg 'True  'False 'True))
  (True,  True,  False) -> k (Proxy @('MkEvalCfg 'True  'True  'False))
  (True,  True,  True)  -> k (Proxy @('MkEvalCfg 'True  'True  'True))
{-# INLINE withEvalCfg #-}
```

### Solution Part 2: Derive `prov` from `cfg`

The `runWithStoreEffectsIOT` function previously had a separate polymorphic `prov` parameter that was existentially quantified:

```haskell
-- OLD: prov was a separate polymorphic parameter (problematic)
runWithStoreEffectsIOT
  :: forall (cfg :: EvalCfg) a
   . KnownEvalCfg cfg
  => Options
  -> (forall (prov :: Bool) m. (StdBase m, KnownEvalCfg cfg, SBoolI prov, Typeable prov)
      => StdM prov cfg m a)
  -> IO a
```

This prevented GHC from recognizing that `prov` should equal `CfgProv cfg`. The fix was to derive `prov` directly from `cfg`:

```haskell
-- NEW: prov is CfgProv cfg, derived from the configuration
runWithStoreEffectsIOT
  :: forall (cfg :: EvalCfg) a
   . KnownEvalCfg cfg
  => Options
  -> (forall m. StdBase m => StdM (CfgProv cfg) cfg m a)
  -> IO a
```

This also required adding `Typeable (CfgProv cfg)` to the `KnownEvalCfg` constraint:

```haskell
type KnownEvalCfg cfg =
  ( SBoolI (CfgStats cfg)
  , SBoolI (CfgProv cfg)
  , SBoolI (CfgTrace cfg)
  , Typeable cfg
  , Typeable (CfgProv cfg)  -- Added for type family reduction
  )
```

### Results

With both fixes applied:

**Measured Impact in Main.hs (GHC Core analysis):**
- **Before**: 110 `sbool` dispatch sites
- **After**: Only **2** `sbool` dispatch sites

The remaining 2 dispatches are unavoidable runtime decisions for:
1. Stats collection at startup (`mstats <- ifStats @cfg ...`)
2. A trace-related check

All other `sbool` dispatches were specialized away by GHC.

### Why This Works

The key insights are:

1. **Avoid existential quantification** at specialization boundaries - explicit case dispatch lets GHC see concrete types in each branch

2. **Type family alignment** - when `prov` is derived from `cfg` via `CfgProv cfg`, GHC can reduce the type family at compile time when `cfg` is known

3. **INLINE pragmas** on bridge functions enable cross-module specialization

4. The 8 branches (2³ for stats/prov/tracing) are just type applications of the same function - no source-level code duplication. GHC generates specialized code automatically.

### Test Verification

The test suite passes (416/418 tests, with 2 pre-existing failures in `builtins.ceil` and `builtins.floor` unrelated to these changes). Basic evaluation works:

```
$ hnix --eval --expr '1 + 1'
2
```

## Conclusion

The combination of explicit case dispatch and type family alignment successfully enables specialization while maintaining a single codebase. The library remains polymorphic (for flexibility and reusability), while the final executable gets fully specialized code paths.

Key takeaways:
1. **Avoid existential quantification** at specialization boundaries
2. **Use explicit enumeration** when bridging runtime values to type-level
3. **Derive parameterized types from the config** rather than introducing separate polymorphic parameters
4. **INLINE pragmas** on bridge functions enable cross-module specialization
5. The cost is O(2^n) branches for n boolean flags, but no source-level duplication
