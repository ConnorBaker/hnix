# Phase 2: Best Path Forward (Ignoring Refactor Cost)

This document proposes the cleanest long‑term integration strategy for Phase 2,
assuming refactor effort is not a constraint. It avoids the `CfgProv cfg`
existential reduction problem entirely and satisfies the `Scoped` functional
dependency by construction.

## Summary

**Best approach:** make provenance an **explicit type parameter of the evaluator
monad** and unify core value/thunk types around that parameter. This removes
`SelValue (CfgProv cfg)` reduction issues and ensures a single `Scoped` instance
per monad.

## Why this is best

- **Eliminates the blocker**: no stuck `SelValue (CfgProv cfg)` reductions under
  `withEvalCfg`.
- **Satisfies `Scoped m -> a`**: the monad type itself fixes the value type.
- **Single evaluator path**: no duplicate evaluation logic; only a small
  entrypoint branch on `prov`.
- **Zero‑overhead**: `SBoolI prov` still allows compile‑time elimination of
  dead branches.

## Step‑by‑Step Plan

### 1) Introduce provenance‑indexed core types

Define a single provenance‑indexed value/thunk type and keep compatibility
aliases for Std/Lite:

```haskell
type Value (prov :: Bool) m =
  NValue (Thunk prov m) (Cited prov (Thunk prov m) (Cited prov) m) m

newtype Thunk (prov :: Bool) m =
  Thunk (Cited prov (Thunk prov m) (Cited prov) m (NThunkF m (Value prov m)))

-- Backward compatibility
 type StdValue  m = Value 'True  m
 type LiteValue m = Value 'False m
 type StdThunk  m = Thunk 'True  m
 type LiteThunk m = Thunk 'False m
```

### 2) Parameterize StandardTF by provenance

Change the evaluator stack to include `prov`:

```haskell
newtype StandardTF (prov :: Bool) (cfg :: EvalCfg) r m a =
  StandardTF (ReaderT (Context cfg r (Value prov r)) (StateT ...) m a)
```

Update `StandardT`, `StdM`, `StdValM`, `StdThunM` to include `prov`.

### 3) Single Scoped instance

```haskell
instance (MonadReader (Context cfg m (Value prov m)) m, MonadIO m)
  => Scoped (Value prov m) m where ...
```

Now `m` uniquely determines the value type via `prov`.

### 4) Update MonadEffects / MonadThunk / MonadValue constraints

Replace concrete Std/Lite types with `Value prov m` / `Thunk prov m`.

### 5) Bridge from withEvalCfg

At the entrypoint, choose `prov` based on the singleton:

```haskell
withEvalCfg stats prov trace $ \(_ :: Proxy cfg) ->
  case singProv @cfg of
    STrue  -> runWithStoreEffectsIOT @'True  @cfg opts action
    SFalse -> runWithStoreEffectsIOT @'False @cfg opts action
```

No type families are needed to select Std vs Lite at this point.

### 6) Remove Sel* and NoCite

Once `prov` is explicit, the `SelCited/SelThunk/SelValue` families and
`NoCite` wrapper are redundant and can be removed.

## Notes

- This is the cleanest architecture but requires a broad refactor.
- The unified `Cited` already supports `prov`; this plan extends that idea to
  the entire evaluator type graph.
- This eliminates all dual‑instance ambiguity and removes the need for
  GHC to reduce `CfgProv cfg` under existential quantification.

