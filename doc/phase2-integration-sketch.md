# Phase 2 Integration Sketch (Scoped Fundep-Safe)

**Status:** ATTEMPTED - Blocked by GHC type family limitations (see Notes section)

This document is a concrete implementation sketch for completing Phase 2 by
wiring the unified provenance-aware types into the evaluator, while respecting
`Scoped`'s functional dependency (`m -> a`).

## Goal

Make the **monad choose the value type**, so there is exactly one `Scoped` instance
per monad. This eliminates the Std/Lite overlap problem and allows provenance
selection via `CfgProv cfg` at the type level.

## 1) Selected-type aliases

In `src/Nix/Standard.hs`:

```haskell
type StdSelValue (cfg :: EvalCfg) m = SelValue (CfgProv cfg) m
type StdSelThunk (cfg :: EvalCfg) m = SelThunk (CfgProv cfg) m
type StdSelCited (cfg :: EvalCfg) m = SelCited (CfgProv cfg) m
```

## 2) StandardTF environment uses selected value type

Change `StandardTF` to use `StdSelValue` in the Reader environment:

```haskell
newtype StandardTF (cfg :: EvalCfg) r m a
  = StandardTF
      (ReaderT
        (Context cfg r (StdSelValue cfg r))
        (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
        a
      )
```

Avoid deriving `MonadReader` here if GHC rejects type-family uses. Provide
explicit instances instead:

```haskell
instance MonadReader (Context cfg r (StdSelValue cfg r)) (StandardTF cfg r m) where
  ask = StandardTF ask
  local f (StandardTF m) = StandardTF (local f m)
  reader f = StandardTF (reader f)
```

## 3) Update mkStandardT / runStandardT

Replace `StdValue` with `StdSelValue` in the Reader type:

```haskell
mkStandardT
  :: ReaderT
      (Context cfg (StandardT cfg m) (StdSelValue cfg (StandardT cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
  -> StandardT cfg m a
```

Same for `runStandardT`.

## 4) Single Scoped instance (fundep-safe)

Replace the Std-only instance with a selected-value instance:

```haskell
instance
  ( MonadReader (Context cfg m (StdSelValue cfg m)) m
  , MonadIO m
  )
  => Scoped (StdSelValue cfg m) m where
  askScopes   = askScopesReader
  clearScopes = clearScopesReader @m @(StdSelValue cfg m)
  pushScopes  = pushScopesReader
  setScopes   = setScopesReader
  lookupVar   = lookupVarWithStatsSel
```

`lookupVarWithStatsSel` should be a version of `lookupVarWithStats` generalized
to the selected value type.

## 5) Update MonadEffects instance

Change the Std-only instance head to selected types:

```haskell
instance
  ( ...
  , Scoped (StdSelValue cfg m) m
  , MonadReader (Context cfg m (StdSelValue cfg m)) m
  , MonadDataErrorContext (StdSelThunk cfg m) (StdSelCited cfg m) m
  , MonadThunk (StdSelThunk cfg m) m (StdSelValue cfg m)
  , MonadValue (StdSelValue cfg m) m
  , HasProvCfg cfg
  )
  => MonadEffects (StdSelThunk cfg m) (StdSelCited cfg m) m where
  ...
```

## 6) Update StdValM / StdThunM aliases

```haskell
type StdValM (cfg :: EvalCfg) m = StdSelValue cfg (StdM cfg m)
type StdThunM (cfg :: EvalCfg) m = StdSelThunk cfg (StdM cfg m)
```

This should propagate into `main/Main.hs` and `tests/TestCommon.hs` automatically.

## 7) Audit direct StdValue/StdThunk constraints

Search for `StdValue`/`StdThunk` in constraints and replace where appropriate:

- `src/Nix/Standard.hs`
- `main/Main.hs`
- `tests/TestCommon.hs`
- any `Scoped (StdValue m)` constraints in Std-only code

## 8) Verify and benchmark

- Build with `CfgProv cfg ~ 'True'` and confirm provenance still works.
- Build with `CfgProv cfg ~ 'False'` and benchmark memory/allocs.
- Run nixpkgs evaluation to confirm `<unknown location>` errors are gone.

## Notes

- This keeps a single evaluator path; no CPP or split monads required.
- The key is eliminating multiple `Scoped` instances per monad by making the
  monad select the value type.
- If GHC still dislikes type families in deriving, keep manual instances.

## Implementation Attempt (2026-01-17)

### What was tried

1. Added `StdSelValue`, `StdSelThunk`, `StdSelCited` type aliases that use the
   `SelValue`/`SelThunk`/`SelCited` type families with `CfgProv cfg`

2. Updated `StandardTF` to use `StdSelValue cfg r` in its Reader environment

3. Added manual `MonadReader` instance with type equality constraint:
   ```haskell
   instance (Monad m, v ~ StdSelValue cfg r)
     => MonadReader (Context cfg r v) (StandardTF cfg r m)
   ```

4. Updated `Scoped` and `MonadEffects` instances with type equality constraints

### Why it didn't work

The fundamental issue is that `withEvalCfg` introduces an **existential** `cfg`:

```haskell
withEvalCfg :: Bool -> Bool -> Bool
  -> (forall cfg. KnownEvalCfg cfg => Proxy cfg -> r) -> r
```

When `cfg` is existentially bound, GHC cannot reduce type families like
`SelValue (CfgProv cfg) m` because it doesn't know at compile time whether
`CfgProv cfg` is `'True` or `'False`. Even though `KnownEvalCfg cfg` provides
`SBoolI (CfgProv cfg)`, this only gives a runtime singleton value, not a
compile-time type equality.

The `MonadNix` constraint requires `Scoped (NValue t f m) m`, and when `StandardTF`
uses type families, GHC cannot prove that `StdSelValue cfg m ~ NValue t f m` for
the specific `t` and `f` that `MonadNix` expects.

### Current state

The implementation has been reverted to use concrete `StdValue`/`StdThunk`/`StdCited`
types. The Lite infrastructure (`LiteCited`, `LiteThunk`, `LiteValue`) is complete
and available for future integration via alternative approaches:

1. **Separate evaluation paths** - explicitly use `LiteValue` in a separate runner
2. **Parameterize wrapper types** - make `StdThunk`/`StdCited` take `prov` parameter
3. **Data families** - use injective data families instead of type families

See `doc/value-optimization-plan.md` for full details.
