# Compile/Runtime Architecture Review Findings

Date: 2026-01-24
Scope: `compile/` and `compile-runtime/` staged changes, plus a follow-up on unstaged local changes

This write-up expands the earlier review notes with concrete locations, likely failure modes, and suggested directions. I focused on correctness risks first, then semantics/compatibility concerns, then completeness gaps.

## Update: Review of unstaged changes (summary)

The unstaged diffs address several core blockers but also introduce a new type error and leave a few critical items unresolved.

**Addressed (partially or fully):**
- `VarName`/`Text`/`Path` literal construction now goes through runtime helpers (`mkVarNameStr`, `mkText`, `mkPath`), fixing the raw `String` injection in most places.
- `NixEnv` is now typed correctly in the compiler and initialized with `emptyEnv` before evaluation.
- Dynamic scope lookup now uses `lookupWithScopesOrThrow`.
- `nixSelectOr` defaults for param sets are now applied.
- `builtins.sort` now handles equality.
- `NoStrict` pragma added to runtime modules to reduce global strictness.

**Remaining or new issues:**
- `VClosure` params still use `()`; Core remains ill-typed without `RuntimeParams` construction.
- `mkVStringExpr` (in the compiler monad) still injects a raw `String` instead of `Text`.
- Multi-level `?` and select-with-default now use `nixHasAttr`, but the conditional expects a `Bool` while `nixHasAttr` returns `NixValue`. This is a new Core type error.
- Multi-level `?` still throws when an intermediate is not an attrset; Nix expects `false` in that case.
- Laziness mismatch remains significant because strict fields/containers are still in use despite `NoStrict`.

## 1) Type correctness and Core construction (critical)

Several Core builders currently emit raw `String`/`unit` literals where the runtime APIs expect structured types (`VarName`, `Text`, `Path`, `RuntimeParams`). This will either fail Core lint/typechecking or produce values that misbehave at runtime.

### 1.1 VarName literals are built as `String`, not `VarName`

- `compile/Nix/Compile/Expr.hs:563-567`
  - `mkVarNameExpr` builds a literal `String` via `mkLitString`.
  - This is passed to primops like `nixSelect` / `nixHasAttr` which expect `VarName`.
- `compile/Nix/Compile/Expr.hs:120-133`
  - Dynamic lookup uses `refLookupWithScopesId` with that same `String` literal.

**Expected type:** `VarName`
**Actual:** `[Char]` (String)

**Fix direction:** add a runtime helper like `mkVarNameStr :: String -> VarName` (already exists in `compile-runtime/Nix/Compile/Value.hs:544`) and load it in `RuntimeRefs`. Then build Core that calls `mkVarNameStr` on the string literal (or use a fully typed literal path via Core plugin utilities).

**Status after unstaged changes:** Mostly fixed. `mkVarNameExpr` now calls `mkVarNameStr`, and `RuntimeRefs` loads it.

### 1.2 `VString` payloads are built with `String`, not `Text`

- `compile/Nix/Compile/Expr.hs:569-573`
  - `mkVStringExprText` uses `mkLitString (toString t)` and injects it directly.
- `compile/Nix/Compile/Monad.hs:195-201`
  - `mkVStringExpr` uses `mkLitString` and injects it directly.

`VString` constructor expects `Text` in the runtime (`compile-runtime/Nix/Compile/Value.hs:186`).

**Fix direction:** add/load `mkText :: String -> Text` (already exists in `compile-runtime/Nix/Compile/Value.hs:566`) and wrap literals before constructing `VString`.

**Status after unstaged changes:** Partially fixed. `mkVStringExprText` now uses `mkText`, but `mkVStringExpr` (used elsewhere) still injects a raw `String` and remains incorrect (`compile/Nix/Compile/Monad.hs:195-201`).

### 1.3 Paths are built as `String`, not `Path`

- `compile/Nix/Compile/Expr.hs:581-582` uses `mkLitString` for a `Path` argument to `VPath`.

`VPath` expects `Path` from `types/Nix/Types/Path.hs`. This is a newtype over `FilePath` and not type-equal to `String` in Core.

**Fix direction:** add/load a runtime helper `pathFromString :: String -> Path` (or re-export a `fromString` for `Path`) and wrap the literal before `VPath`.

**Status after unstaged changes:** Fixed for `mkPathLit` via new `mkPath` helper, and `RuntimeRefs` now loads it.

### 1.4 `VClosure` param metadata is built as `()` not `RuntimeParams`

- `compile/Nix/Compile/Expr.hs:575-579`
  - `mkParamsExpr` uses `unitDataCon` regardless of parameter shape.

But `VClosure` is `VClosure RuntimeParams (NixValue -> NixValue)` in runtime (`compile-runtime/Nix/Compile/Value.hs:194`). Passing unit will not typecheck.

**Fix direction:** load `RuntimeParams` constructors in `RuntimeRefs` and build them in Core (either minimal or full info).

**Status after unstaged changes:** Still unresolved. `mkParamsExpr` remains `()` and `VClosure` remains ill-typed.

### 1.5 `nixSelectOr` default handling is missing

- `compile/Nix/Compile/Expr.hs:311-315`
  - In param-set defaults, `nixSelectOr` is called with only 2 args and `def'` is ignored.

`nixSelectOr :: NixAttrs -> VarName -> NixValue -> NixValue`. You currently build a partially applied function but then bind it as if it is a `NixValue`. This is a type error in Core and a logic bug (defaults never apply).

**Fix direction:** pass the default value as the third argument in the Core application.

**Status after unstaged changes:** Fixed.

## 2) NixEnv plumbing and dynamic scope (critical)

The dynamic scope path (`with` and unbound variable lookup) expects `NixEnv` values but the compiler currently treats them as `NixValue` and never constructs an `emptyEnv` value. There is also a mismatch between `lookupWithScopes` (returns `Maybe`) vs `lookupWithScopesOrThrow` (returns `NixValue`).

### 2.1 `envId` is typed as `NixValue`, not `NixEnv`

- `compile/Nix/Compile/Driver.hs:262-267`
  - `envId` is built using `refNixValueType` (comment: simplified).
- `compile/Nix/Compile/Expr.hs:396-397`
  - `refNixEnvType` is defined as `refNixValueType`.

This makes all Core that calls `pushWithScope` or `lookupWithScopes*` ill-typed, because those functions take `NixEnv` (runtime type).

**Fix direction:** load the `NixEnv` type and `emptyEnv` Id in `RuntimeRefs`, and use that type when constructing `envId`.

**Status after unstaged changes:** Fixed (`refNixEnvType` loaded and used).

### 2.2 No initialization of `emptyEnv`

- `compile/Nix/Compile/Driver.hs:262-273`
  - `envId` is created but never bound to `emptyEnv`.

The compiled code references `envId` but there is no `Let` binding for its value. This is effectively an unbound core variable.

**Fix direction:** bind `envId` to `emptyEnv` (loaded as an Id in `RuntimeRefs`).

**Status after unstaged changes:** Fixed (body is wrapped in a `let` binding of `emptyEnv`).

### 2.3 Dynamic lookup uses the wrong function

- `compile/Nix/Compile/Expr.hs:120-133`
  - Uses `refLookupWithScopesId` with comment “let it throw on missing”.

But `lookupWithScopes :: VarName -> NixEnv -> Maybe NixValue` (runtime), so this returns `Maybe NixValue`, not `NixValue`. There *is* `lookupWithScopesOrThrow` that returns `NixValue`.

**Fix direction:** load `lookupWithScopesOrThrow` in `RuntimeRefs` and call that from compiled code.

**Status after unstaged changes:** Fixed.

## 3) Strictness and laziness (high, semantic mismatch)

Nix is lazy. The compiled backend intends to reuse GHC laziness, but several design choices make values strict even under GHC. This will cause semantic differences (e.g., exceptions thrown eagerly, infinite recursions that should be lazy, etc.).

### 3.1 `Strict` default extension in the project

- `hnix.cabal`: `flag(strict)` enables `Strict` globally for libs, including runtime.

Under `Strict`, all top-level bindings and fields become strict unless overridden. This conflicts with laziness you likely want for Nix semantics.

### 3.2 `NixValue` fields are strict

- `compile-runtime/Nix/Compile/Value.hs:177-197`
  - Fields are strict (`!`) and some are `UNPACK`ed.

### 3.3 `NixAttrs` uses `HashMap.Strict`

- `compile-runtime/Nix/Compile/Value.hs:237`
  - `HashMap.Strict` forces values at insertion time.

### 3.4 Lists built via `Vector` + `fromList`

- `compile/Nix/Compile/Monad.hs:204-208`
  - Uses `V.fromList` which is strict in its list elements.

**Effect:** Many expressions will evaluate eagerly, making `builtins.tryEval`, `assert`, or `if` behave incorrectly vs Nix. This also undermines your goal of delegating laziness to GHC.

**Fix direction:**
- Consider turning off `Strict` for runtime modules or using `{-# LANGUAGE NoStrict #-}` / `NoStrictData` locally.
- Use a lazy list representation (linked list, lazy vector) or an explicit `Thunk` wrapper if you want to keep vector-like structures.
- Use `HashMap.Lazy` for attrsets if you want to preserve laziness at insertion.

**Status after unstaged changes:** `NoStrict` pragmas were added to runtime modules, which helps, but strict fields (`!`), `UNPACK`s, `HashMap.Strict`, and `Vector` usage remain, so laziness is still materially different from Nix.

## 4) Correctness gaps in Nix semantics (medium)

### 4.1 Complex bindings are silently ignored

- `compile/Nix/Compile/Expr.hs:204-235`
  - Only simple bindings are used in recursive sets/lets.
- `compile/Nix/Compile/Expr.hs:236-247`
  - Nested key paths are ignored entirely (`pure []`).

This silently drops user-specified bindings rather than failing. In Nix this should be an error or a real insertion at a path.

**Fix direction:**
- Either emit a runtime error for unsupported binding types,
- or compile nested paths using `insertAtPathDynamic` or a static-path builder.

### 4.2 Multi-level select/default/hasAttr are unimplemented (or incorrect)

- `compile/Nix/Compile/Expr.hs:426-433`
  - Defaults only work for single key; multi-key paths ignore defaults.
- `compile/Nix/Compile/Expr.hs:440-451`
  - `hasAttr` for multi-level path uses `error`.

**Fix direction:** load `nixSelectPath` / `nixHasAttrPath` and build `NonEmpty VarName` via runtime helpers (already exists `mkNonEmptyVarName` in runtime).

**Status after unstaged changes:** Partially implemented, but currently incorrect:
- `compileSelectPathOr` and `compileHasAttrPath` use `nixHasAttr` directly in `if` conditions. `nixHasAttr` returns `NixValue`, not `Bool`, so the Core is ill-typed unless `expectBool` is inserted.
- `compileHasAttrPath` still uses `expectAttrs` for intermediate path elements, which will throw on non-attrsets. Nix expects `{ a = 1; } ? a.b` to return `false`, not throw.
- `RuntimeRefs` now loads `nixSelectPath`/`nixHasAttrPath`, but they are not used yet. Using those primops (or adding an `isAttrs` primop) would avoid the incorrect behavior.

### 4.3 Path interpolation and env paths compiled to strings

- `compile/Nix/Compile/Expr.hs:520-529` path interpolation uses `nixCoerceToString` (string), not path.
- `compile/Nix/Compile/Expr.hs:492-508` env path `<nixpkgs>` is compiled as literal path, not resolved via `NIX_PATH`.

**Fix direction:** use `stringToPath` or a dedicated primop for interpolated paths; use `nixResolveEnvPath` for `<...>`.

### 4.4 Param set validation (closed patterns) not enforced

The runtime has `nixCheckClosedPattern` but compiler never invokes it. This means closed patterns (`{ a, b }:`) will accept extra keys silently, diverging from Nix.

## 5) Builtins behavior issues (low/medium)

### 5.1 `builtins.sort` comparator never yields EQ

- `compile-runtime/Nix/Compile/Builtins.hs:340-347`
  - Sort uses `LT` for true and `GT` for false only.

If the comparator returns false for both `a<b` and `b<a`, the function always returns `GT` and cannot represent `EQ`. This may break sorting semantics and stability.

**Status after unstaged changes:** Fixed. Comparator now checks both directions and yields `EQ` when neither is true.

### 5.2 `builtins.tryEval` / `trace` rely on strict values

Both are implemented via `evaluate`/`trace` and will force the input in a strict runtime, which differs from Nix behavior if evaluation should be deferred.

## 6) Architectural notes (positive)

- The compiler/runtime split is clean and matches the goal of reusing GHC.
- `RuntimeRefs` caching avoids repeated GHC name lookups and is likely the right approach.
- Using `hscCompileCoreExpr` + bytecode interpreter is a good bridge for experimentation.

## 7) Recommended next steps (ordered)

1. **Fix remaining Core type errors**
   - Build real `RuntimeParams` for `VClosure`.
   - Insert `expectBool` (or change primop return types) before using `nixHasAttr` in `if` conditions.
   - Fix `mkVStringExpr` to use `mkText`.

2. **Re-evaluate multi-level select/hasAttr correctness**
   - Use `nixHasAttrPath`/`nixSelectPath` or an `isAttrs` primop to avoid throwing on non-attrset intermediates.

3. **Decide on strictness/laziness**
   - If Nix laziness is required, adjust runtime modules to be lazy and avoid strict containers.

4. **Add missing semantic support**
   - Multi-level select/hasAttr/defaults via `nixSelectPath` and `nixHasAttrPath`.
   - Nested binding paths; dynamic keys in sets; path interpolation and env paths.

5. **Improve error handling**
   - Replace `error` and silent drops with explicit `NixError`.

6. **Audit builtins behavior**
   - Fix `builtins.sort` comparator treatment of equality.

---

If you want, I can follow up with a patch that wires in the missing runtime refs and fixes Core literals/env binding first (items 1 and 2), since those are the blockers to running any compiled code correctly.
