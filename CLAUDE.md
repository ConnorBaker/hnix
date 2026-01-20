# HNix Codebase Guide for Claude Code

This guide provides essential context for working with HNix - a Haskell implementation of the Nix expression language using advanced functional programming techniques including recursion schemes and abstract definitional interpreters.

## Quick Start Commands

### Essential Development Workflow
```bash
# Standard iterative build (preferred)
nix develop ".?submodules=1#" --command cabal build

# Build without strict (for performance comparison)
nix develop ".?submodules=1#" --command cabal build -f-strict

# Run a single test
nix develop ".?submodules=1#" --command cabal test --test-options="--pattern '/Parser/basic literals/'"

# Interactive REPL for exploration
nix develop ".?submodules=1#" --command cabal repl
> :load Nix.Eval
> :type evalExprLoc

# Quick evaluation test
nix develop ".?submodules=1#" --command cabal run hnix -- --eval --expr '1 + 1'
```

### Output Handling Rules

**IMPORTANT:** Never redirect stdout or stderr with nix or cabal commands, and never pipe them into `head` or `tail`. Instead:

```bash
# BAD - Don't do this
nix develop ".?submodules=1#" --command cabal build 2>&1 | head -50
cabal build > output.txt 2>&1

# GOOD - Run commands directly
nix develop ".?submodules=1#" --command cabal build
# If output is needed, read build logs from dist-newstyle/ or use cabal's --builddir
```

This avoids issues with buffering, signal handling, and incomplete output from terminated processes.

### Testing Commands
```bash
# Standard test suite (use -j1 for reliable results, see note below)
nix develop ".?submodules=1#" --command cabal test --test-options="-j1"

# Standard test suite (parallel, may have intermittent failures)
nix develop ".?submodules=1#" --command cabal test

# All tests including Nixpkgs parsing (slow)
env ALL_TESTS=yes cabal test

# Only Nixpkgs compatibility tests
env NIXPKGS_TESTS=yes cabal test

# Pretty-printer round-trip tests
env PRETTY_TESTS=yes cabal test

# Test with coverage
cabal configure --enable-coverage
cabal test --enable-coverage
```

**Note on Test Flakiness:** The test suite has intermittent failures when run in parallel due to race conditions. Use `--test-options="-j1"` for reliable single-threaded execution.

### Test Wrappers

Additional test scripts for Nix compatibility testing:

```bash
# Parser comparison tests - compares HNix parser output against Nix
# Runs 3 phases: string escapes, Nix test suite, edge cases
nix develop ".?submodules=1#" --command bash tests/parser-compare/run-all.sh

# Individual parser comparison phases
nix develop ".?submodules=1#" --command bash tests/parser-compare/01-string-escapes.sh
nix develop ".?submodules=1#" --command bash tests/parser-compare/02-nix-test-suite.sh
nix develop ".?submodules=1#" --command bash tests/parser-compare/03-edge-cases.sh

# Nixpkgs module system tests (requires nixpkgs checkout)
# Set NIXPKGS_DIR and HNIX_BIN environment variables first
./test-nixpkgs-modules.sh
```

**Parser Comparison Tests (`tests/parser-compare/`):**
- Phase 1: String escape sequences (quotes, dollars, backslashes)
- Phase 2: Nix's official test suite files
- Phase 3: Edge cases (unicode, nested interpolation, indentation, paths)

**Nixpkgs Module Tests (`test-nixpkgs-modules.sh`):**
- Tests HNix against Nixpkgs' module system
- Requires `NIXPKGS_DIR` pointing to a nixpkgs checkout
- Requires `HNIX_BIN` pointing to the built hnix binary

### Debugging & Profiling
```bash
# Memory profiling (upload .prof to speedscope.app)
cabal run --enable-profiling --flags=profiling \
  hnix -- --eval --expr 'builtins.length [1 2 3]' +RTS -hy -l

# Stack trace on error
cabal run hnix -- --trace --eval --expr 'throw "error"' +RTS -xc

# Heap profiling for thunk leaks
cabal run hnix -- --eval --expr 'import <nixpkgs> {}' \
  +RTS -h -i0.1 -RTS && hp2ps -e8in -c hnix.hp

# Reduce complex expressions for minimal repro
hnix --reduce bug.nix --eval --expr 'import ./bug.nix'
```

## Architecture: Working with Recursion Schemes

### Core Expression Types
```haskell
-- The functor (non-recursive structure)
data NExprF r  -- 19 constructors: NConstant, NStr, NSym, NList, NSet, NLiteralPath, NPath, NEnvPath, NApp, NUnary, NBinary, NSelect, NHasAttr, NAbs, NLet, NIf, NWith, NAssert, NSynHole

-- Fixed point gives recursion
type NExpr = Fix NExprF

-- Location annotations via composition
type NExprLoc = Fix (AnnF SrcSpan NExprF)
```

### Using ADI for Custom Behavior

The `adi` function (`src/Nix/Utils.hs`) enables behavior injection:

```haskell
-- Example: Add tracing to evaluation
tracingEval :: NExprLoc -> m (NValue t f m)
tracingEval = adi addTrace baseEval
  where
    addTrace :: Transform NExprLocF (m (NValue t f m))
    addTrace f e = do
      traceM $ "Evaluating: " ++ show (void e)
      result <- f e
      traceM $ "Result: " ++ show result
      pure result
```

Common ADI use cases:
- **Error context**: `evalWithMetaInfo = adi addMetaInfo evalContent`
- **Profiling**: Inject timing measurements at each recursion
- **Memoization**: Cache results of sub-expressions
- **Debugging**: Track evaluation path

### Free Monad Value System

```haskell
type NValue t f m = Free (NValue' t f m) t
-- Pure t = thunk (unevaluated)
-- Free v = evaluated value
```

**Memory implications**:
- Thunks accumulate until forced
- Use `force` explicitly to prevent buildup
- Monitor with `+RTS -s` for thunk statistics

## Working with the Effect System

### Core Type Classes

```haskell
-- Core evaluation typeclass (src/Nix/Eval.hs)
class (Show v, Monad m) => MonadEval v m where
  freeVariable    :: VarName -> m v
  synHole         :: VarName -> m v
  attrMissing     :: NonEmpty VarName -> Maybe v -> m v
  evaledSym       :: VarName -> v -> m v
  evalCurPos      :: m v
  evalConstant    :: NAtom -> m v
  evalString      :: NString (m v) -> m v
  evalLiteralPath :: Path -> m v
  evalEnvPath     :: Path -> m v
  evalUnary       :: NUnaryOp -> v -> m v
  evalBinary      :: NBinaryOp -> v -> m v -> m v
  evalWith        :: m v -> m v -> m v
  evalIf          :: v -> m v -> m v -> m v
  evalAssert      :: v -> m v -> m v
  evalApp         :: v -> m v -> m v
  evalAbs         :: Params (m v) -> (forall a. m v -> (AttrSet (m v) -> m v -> m (a, v)) -> m (a, v)) -> m v
  -- ... additional methods for list, set, select, hasAttr, let, path

class MonadThunkId m => MonadThunk t m a | t -> m, t -> a where
  thunkId  :: t -> ThunkId m        -- Return thunk ID
  thunk    :: m a -> m t            -- Create thunk
  query    :: m a -> t -> m a       -- Non-blocking query
  force    :: t -> m a              -- Force evaluation
  forceEff :: t -> m a              -- Force with effects
  further  :: t -> m t              -- Modify thunk action

-- MonadNix is a type alias, not a class (src/Nix/Exec.hs)
type MonadNix e t f m =
  ( Has e SrcSpan
  , Has e Options
  , Has e (Maybe EvalStats)
  , Scoped (NValue t f m) m
  , Framed e m
  , MonadFix m
  , MonadCatch m
  , MonadThrow m
  , Alternative m
  , MonadEffects t f m
  , MonadCitedThunks t f m
  , MonadValue (NValue t f m) m
  )
```

### Adding New Effects

```haskell
-- Define capability
class Monad m => MonadMyEffect m where
  myOperation :: String -> m Int

-- Add to evaluation monad
newtype MyNix m a = MyNix (ReaderT MyEnv m a)
  deriving (Functor, Applicative, Monad)

instance MonadMyEffect (MyNix m) where
  myOperation s = MyNix $ asks (lookupThing s . myEnvData)
```

### Type-Level Configuration System

HNix uses type-level configuration to enable zero-cost conditional features. The system is defined in `src/Nix/Config/Singleton.hs`.

```haskell
-- Type-level configuration kind
data EvalCfg = MkEvalCfg
  { cfgStats  :: Bool  -- Evaluation statistics
  , cfgProv   :: Bool  -- Provenance tracking
  , cfgTrace  :: Bool  -- Trace evaluation
  }

-- Default configuration (all features disabled)
type DefaultCfg = 'MkEvalCfg 'False 'False 'False

-- Constraint for known configurations
type KnownEvalCfg cfg =
  ( KnownBool (CfgStats cfg)
  , KnownBool (CfgProv cfg)
  , KnownBool (CfgTrace cfg)
  )

-- Bridge runtime options to compile-time config
withEvalCfg
  :: Bool  -- ^ stats
  -> Bool  -- ^ prov
  -> Bool  -- ^ tracing
  -> (forall cfg. KnownEvalCfg cfg => Proxy cfg -> r)  -- ^ default
  -> (forall cfg prov. (KnownEvalCfg cfg, SBoolI prov) => Proxy cfg -> Proxy prov -> r)  -- ^ callback
  -> r
```

**Usage**: At program startup, `withEvalCfg` examines runtime options and selects the appropriate type-level configuration, enabling GHC to specialize code paths and eliminate dead branches.

### Provenance-Indexed Value System

Values and thunks are parameterized by a `prov :: Bool` type that controls provenance tracking at the type level. This is defined in `src/Nix/Standard.hs`.

```haskell
-- Cited functor - wraps values with optional provenance
-- When prov ~ 'True:  stores full provenance list (NCited wrapper)
-- When prov ~ 'False: zero overhead (Identity wrapper)
newtype CitedF (prov :: Bool) m a =
  CitedF (Cited prov (ThunkF prov m) (CitedF prov m) m a)

-- Thunk type parameterized by provenance
newtype ThunkF (prov :: Bool) m =
  ThunkF (CitedF prov m (NThunkF m (ValueF prov m)))

-- Value type parameterized by provenance
type ValueF (prov :: Bool) m = NValue (ThunkF prov m) (CitedF prov m) m

-- Standard evaluation monad with compile-time config and provenance
type StdM (prov :: Bool) (cfg :: EvalCfg) m = StandardT prov cfg (StdIdT m)
```

**Key benefit**: When `prov ~ 'False`, the `CitedF` newtype erases completely at runtime, making provenance tracking truly zero-cost when disabled.

## Extending HNix

### Adding Built-ins

1. Add to `src/Nix/Builtins.hs` in `builtinsList`:
```haskell
-- builtinsList uses helper functions based on arity:
-- add0: No arguments (constants)
-- add:  Single argument
-- add2: Two arguments
-- add3: Three arguments
-- add': Use ToBuiltin typeclass for automatic conversion

builtinsList :: forall e t f m . (MonadNix e t f m, HasProvCfg (CtxCfg e)) => m [Builtin (NValue t f m)]
builtinsList =
  sequenceA
    [ add  Normal "myBuiltin" myBuiltinImpl
    , add2 Normal "myBinary"  myBinaryImpl
    -- ...
    ]

myBuiltinImpl :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
myBuiltinImpl arg = do
  -- Force evaluation if needed
  str <- fromStringNoContext =<< fromValue arg
  -- Perform operation
  pure $ nvStr $ mkNixStringWithoutContext (str <> "!")
```

2. Test in `tests/EvalTests.hs`
3. Document behavior matching Nix semantics

### Modifying Evaluation

```haskell
-- Hook into evaluation via MonadEval instance
instance MonadEval (NValue t f m) MyCustomNix where
  evalExprLoc expr = do
    -- Pre-evaluation hook
    logExpression expr
    -- Delegate to standard evaluation
    result <- standardEvalExprLoc expr
    -- Post-evaluation hook
    recordMetrics expr result
    pure result
```

## Common Pitfalls & Solutions

### Memory Issues

**Problem**: Thunk accumulation causing memory exhaustion
```haskell
-- BAD: Builds huge thunk chain
foldl' (\acc x -> thunk (acc + x)) 0 [1..1000000]

-- GOOD: Forces evaluation incrementally
foldl' (\acc x -> force acc >>= \a -> pure (a + x)) 0 [1..1000000]
```

**Problem**: Lazy fields in strict data
```haskell
-- BAD: ~ makes field lazy despite ! on data
data MyData = MyData { ~myField :: !Int }

-- GOOD: Strict field in strict data
data MyData = MyData { myField :: !Int }
```

### Debugging Infinite Recursion

1. Enable tracing: `--trace` flag
2. Use `--reduce` to minimize test case
3. Add ADI transform to track recursion depth:
```haskell
depthCheck :: Transform NExprLocF (ReaderT Int m (NValue t f m))
depthCheck f e = do
  depth <- ask
  when (depth > 1000) $ error "Recursion limit"
  local (+1) (f e)
```

### Performance Optimization

**Profile first**:
```bash
# Generate flamegraph
cabal v2-run hnix -- --eval --expr 'import <nixpkgs> {}' \
  +RTS -p -RTS
```

**Common optimizations**:
1. Add strictness annotations to accumulators
2. Use `HashMap` instead of association lists
3. Cache frequently computed values
4. Specialize polymorphic functions with `{-# SPECIALIZE #-}`

## Strict Language Extension

### Overview

HNix uses the `Strict` language extension across all modules to prevent space leaks from lazy accumulation. This is controlled by the `strict` cabal flag (enabled by default).

```bash
# Build with strictness (default)
nix develop ".?submodules=1#" --command cabal build

# Build without strictness (for comparison)
nix develop ".?submodules=1#" --command cabal build -f-strict
```

### Why Strict?

Benchmarks show Strict provides **16% less memory** in production and **2.8x less memory** when debugging with `--eval-stats`. See [doc/strict-benchmarks.md](doc/strict-benchmarks.md) for detailed analysis.

### Critical: Avoid `bool`, `maybe`, `either`, `fromMaybe`

With `Strict` enabled, function arguments are evaluated before the function body. This breaks short-circuiting for branching combinators:

```haskell
-- BAD: Both branches evaluated under Strict!
bool expensiveComputation cheapResult condition

-- GOOD: Only matching branch evaluated
if condition then cheapResult else expensiveComputation

-- BAD: Both branches evaluated under Strict!
maybe defaultValue transform mVal

-- GOOD: Only matching branch evaluated
case mVal of
  Nothing -> defaultValue
  Just v  -> transform v

-- BAD: Both branches evaluated under Strict!
either handleError handleSuccess result

-- GOOD: Only matching branch evaluated
case result of
  Left err -> handleError err
  Right ok -> handleSuccess ok
```

### Why This Matters

Under Strict, `bool falseCase trueCase condition` evaluates:
1. `falseCase` (to WHNF)
2. `trueCase` (to WHNF)
3. `condition`
4. Returns the appropriate case

This causes:
- **Wasted computation** - both branches always run
- **Side effects** - IO/monadic effects in both branches execute
- **Space leaks** - thunks from unused branch still allocated

### High-Risk Patterns to Audit

Files with `bool`/`maybe`/`either` usage that may need conversion:

| File | Risk | Pattern |
|------|------|---------|
| `src/Nix/Normal.hs` | HIGH | `bool` with expensive thunk forcing |
| `src/Nix/String/Coerce.hs` | HIGH | `bool` with `addPath` store operation |
| `src/Nix/Render/Frame.hs` | MEDIUM | `bool` with pretty-printing |
| `src/Nix/Render.hs` | MEDIUM | `bool` with file reading |
| `src/Nix/Pretty.hs` | LOW | Multiple `bool` usages |
| `src/Nix/Convert.hs` | LOW | `maybe` usages |

### Safe Patterns

These patterns ARE safe with Strict:

```haskell
-- Pattern matching - naturally lazy in alternatives
case expr of
  Constructor1 -> ...
  Constructor2 -> ...

-- Guards - only matching guard evaluates
foo x
  | condition1 = result1
  | condition2 = result2
  | otherwise  = result3

-- Monadic bind short-circuits on failure
do
  result <- mayFail
  expensiveOperation result  -- Only runs if mayFail succeeds

-- Singleton bool dispatch (compile-time elimination)
ifSBool STrue thenBranch elseBranch  -- elseBranch eliminated at compile time
```

## Module Organization & Dependencies

### Layered Architecture
```
┌─────────────────┐
│   Builtins      │ (100+ built-in functions)
├─────────────────┤
│   Effects       │ (MonadNix, MonadEval constraints)
├─────────────────┤
│   Exec          │ (High-level evaluation)
├─────────────────┤
│   Eval          │ (Core evaluation with ADI)
├─────────────────┤
│   Value/Thunk   │ (Free monad values, lazy evaluation)
├─────────────────┤
│   Expr          │ (NExprF functor, parser, pretty-printer)
└─────────────────┘
```

### Key Files for Common Tasks

- **Adding language features**: Start with `src/Nix/Parser.hs`, add to `NExprF` in `src/Nix/Expr/Types.hs`
- **Modifying evaluation**: `src/Nix/Eval.hs` for core, `src/Nix/Exec.hs` for high-level
- **Debugging issues**: `src/Nix/Reduce.hs` for test reduction, `src/Nix/Cited.hs` for error context
- **Performance work**: `src/Nix/Thunk/Basic.hs` for thunk implementation
- **Built-in functions**: `src/Nix/Builtins.hs` - match Nix semantics exactly

## Testing Philosophy

### Test Categories
- **Language tests** (`tests/NixLanguageTests.hs`): Official Nix test suite
- **Evaluation tests** (`tests/EvalTests.hs`): HNix-specific behavior
- **Parser tests** (`tests/ParserTests.hs`): Round-trip properties
- **Pretty tests** (`tests/PrettyTests.hs`): Pretty-printer correctness

### Nix Language Tests (`data/nix/tests/lang/`)

These are golden tests from Nix's test suite. Test files follow naming conventions:

| Pattern | Description |
|---------|-------------|
| `eval-okay-*.nix` + `.exp` | Expression should evaluate; compare output to `.exp` |
| `eval-fail-*.nix` | Expression should fail to evaluate |
| `parse-okay-*.nix` | Should parse successfully |
| `parse-fail-*.nix` | Should fail to parse |

**Adding new tests:**
1. Copy test files from `~/nix/tests/functional/lang/` to `data/nix/tests/lang/`
2. Include both `.nix` and `.exp` files for eval-okay tests
3. Run `cabal test --test-options="-j1"` to verify

**Test discovery:** `NixLanguageTests.hs` automatically discovers tests by filename pattern.

### Writing Effective Tests
```haskell
-- Property-based test for parser round-trip
prop_parse_pretty :: NExpr -> Property
prop_parse_pretty expr =
  parseNixText (prettyNix expr) === Right expr

-- Golden test for evaluation
goldenEval :: String -> NExpr -> TestTree
goldenEval name expr = goldenVsString name path $ do
  result <- runLazyM defaultOptions $ evalExprLoc expr
  pure $ encodeUtf8 $ prettyNValue result
```

### Inspection Tests (`tests/inspection/`)

Compile-time tests using `inspection-testing` to verify zero-overhead abstractions. Run with:
```bash
nix develop ".?submodules=1#" --command cabal test hnix-inspection
```

**What they verify**:
1. **Singleton dispatch elimination**: When `sbool @'False` is used, GHC eliminates the `STrue` branch entirely
2. **Newtype erasure**: `Cited`, `CitedF`, and `ThunkF` wrappers are completely erased in generated code
3. **Type class specialization**: No `SBoolI` dictionaries remain for concrete type applications
4. **Provenance type elimination**: `NCited`, `Provenance`, and `Identity` types don't appear in Core for `prov ~ 'False` code paths
5. **Instance method specialization**: Functor, Applicative, Comonad, Foldable, Traversable, and HasCitations instances all specialize
6. **Config dispatch**: All config flags (stats, prov, trace) dispatch without runtime overhead for DefaultCfg

**16 test modules** cover: Cited, Singleton, Comonad, Functor, Coerce, Integration, Thunk, Config, HasCitations, Types, Scope, MonadThunk, NixString, Value, Convert, AttrSet

On failure, inspection-testing shows the GHC Core that violated the property.

### Memory Benchmarks (`benchmarks/weigh/`)

Memory allocation benchmarks using the `weigh` library. Run with:
```bash
nix develop ".?submodules=1#" --command cabal bench hnix-weigh
```

**What they measure**:
- Scope operations (lookup, insert, delete)
- Vector vs list allocation
- HashMap operations
- Singleton bool dispatch overhead
- Nix expression evaluation allocation

Results are output as markdown tables showing: allocated bytes, GC count, live bytes, max bytes.

## Important Implementation Notes

### Custom Prelude
Uses `relude` with project utilities in `Nix.Utils`. Key differences:
- `panic` instead of `error` for impossible cases
- `pass` for noop in do-blocks
- Strict `Text` by default

### String Context
Nix strings carry derivation context - critical for store paths:
```haskell
-- Context propagates through operations
makeNixString :: Text -> NixString  -- No context
makeNixStringWithContext :: Text -> Context -> NixString
```

### Store Integration
**Warning**: `derivationStrict` creates real `/nix/store` entries. Use `--dry-run` for testing.

### Position Tracking
Custom `NSourcePos` for performance - strict fields prevent memory leaks during parsing.

## Current Status & Goals

**Primary Goal**: Evaluate all of Nixpkgs
```bash
hnix --eval --expr "import <nixpkgs> {}" --find
```

**Working**: Parser, lazy evaluation, most built-ins, REPL, type inference
**In Progress**: Full Nixpkgs evaluation, performance optimization
**Known Issues**: Tests disabled by default (`doCheck = false`) due to store interaction

## Backpack Infrastructure

HNix uses GHC Backpack for compile-time swapping of data structure implementations with guaranteed monomorphization (no dictionary passing at runtime).

For comprehensive documentation including goals, design rationale, and how to add alternative implementations, see [doc/backpack-architecture.md](doc/backpack-architecture.md).

### Package Structure

```
hnix/
├── hnix-types/                      # Shared fundamental types
│   └── src/Nix/Types/
│       ├── Path.hs                  # Filesystem path type
│       ├── VarName.hs               # Interned variable names
│       ├── SourcePos.hs             # Source position tracking
│       └── Atom.hs                  # Atomic literals
│
├── signatures/                      # Backpack signatures (abstract interfaces)
│   ├── hnix-attrset-sig/            # AttrSet operations signature
│   │   └── Nix/AttrSet/Sig.hsig
│   ├── hnix-list-sig/               # NixList operations signature
│   │   └── Nix/List/Sig.hsig
│   └── hnix-string-sig/             # NixString operations signature
│       └── Nix/String/Sig.hsig
│
├── implementations/                 # Concrete implementations
│   ├── hnix-attrset-hashmap/        # HashMap-backed AttrSet
│   │   └── src/Nix/AttrSet/HashMap.hs
│   ├── hnix-list-vector/            # Vector-backed NixList
│   │   └── src/Nix/List/Vector.hs
│   └── hnix-string-text/            # Text-backed NixString
│       └── src/Nix/String/Text.hs
│
├── hnix-value-core/                 # Core value types (indefinite package)
│   └── src/Nix/Value/Core/
│       ├── Value.hs                 # NValue types
│       ├── Equal.hs                 # Value equality
│       ├── Interned.hs              # Interned constants
│       └── Protocol.hs              # Value protocol types
│
├── hnix-builtins-list/              # List builtins (indefinite package)
│   └── src/Nix/Builtins/List.hs
│
├── hnix-builtins-attrset/           # AttrSet builtins (indefinite package)
│   └── src/Nix/Builtins/AttrSet.hs
│
├── hnix-builtins-string/            # String builtins (indefinite package)
│   └── src/Nix/Builtins/String.hs
│
└── (main hnix package)              # Instantiates signatures via mixins
```

### Building Individual Packages

```bash
# Build shared types
nix develop ".?submodules=1#" --command cabal build hnix-types

# Build signatures
nix develop ".?submodules=1#" --command cabal build hnix-attrset-sig hnix-list-sig hnix-string-sig

# Build implementations
nix develop ".?submodules=1#" --command cabal build hnix-attrset-hashmap hnix-list-vector hnix-string-text

# Build indefinite packages (abstract, require instantiation)
nix develop ".?submodules=1#" --command cabal build hnix-value-core
nix develop ".?submodules=1#" --command cabal build hnix-builtins-list hnix-builtins-attrset hnix-builtins-string

# Build main library (instantiates all signatures)
nix develop ".?submodules=1#" --command cabal build lib:hnix
```

### Why Backpack

| Aspect | Typeclasses | Backpack |
|--------|-------------|----------|
| Specialization | Requires INLINABLE + SPECIALIZE pragmas | **Automatic** at link time |
| Dictionary passing | Can occur if GHC misses specialization | **Never** - concrete types |
| Maintenance burden | Must verify with inspection tests | Write signature once |

### Current Status

**Completed:**
- `hnix-types` package with Path, VarName, NSourcePos, NAtom
- Backpack signatures:
  - `hnix-attrset-sig` - AttrSet interface
  - `hnix-list-sig` - NixList interface
  - `hnix-string-sig` - NixString interface (uses smart constructors, not pattern synonyms)
- Concrete implementations:
  - `hnix-attrset-hashmap` - HashMap-backed AttrSet
  - `hnix-list-vector` - Vector-backed NixList
  - `hnix-string-text` - Text + HashSet StringContext backed NixString
- Indefinite packages:
  - `hnix-value-core` - Core value types
  - `hnix-builtins-list` - List builtins
  - `hnix-builtins-attrset` - AttrSet builtins
  - `hnix-builtins-string` - String builtins
- Main `hnix` package instantiates all signatures via Cabal mixins
- Backpack instantiation verified working (GHC monomorphizes correctly)

**Future Work:**
- Add alternative implementations (Map, Seq, ByteString) for benchmarking
- Add inspection tests to verify monomorphization of new packages

### Migration Guide

When migrating modules to use Backpack:

```haskell
-- Before: Direct HashMap import
import qualified Data.HashMap.Strict as HM
type AttrSet = HashMap VarName

-- After: Import from signature
import Nix.AttrSet.Sig  -- Provides abstract AttrSet type

-- Operations stay the same
lookup, insert, union, mapWithKey, etc.
```

The signature modules export the same API as HashMap/Vector, so migration is mostly mechanical renaming.

## Resources

- [Win for Recursion Schemes](https://newartisans.com/2018/04/win-for-recursion-schemes/) - Essential architectural context
- [Design of HNix](https://github.com/haskell-nix/hnix/wiki/Design-of-the-HNix-code-base)
- [Gitter Chat](https://gitter.im/haskell-nix/Lobby)