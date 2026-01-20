# HNix Backpack Architecture

This document describes the GHC Backpack architecture used in HNix for swappable
data structure implementations with zero runtime overhead.

## Goals

### Primary Goals

1. **Zero-overhead abstraction**: Data structures (lists, attribute sets, strings)
   should be swappable at compile time without any runtime cost. No dictionary
   passing, no indirect calls, no type class dispatch overhead.

2. **Compile-time monomorphization**: GHC should fully monomorphize all operations
   at link time, producing specialized code as if concrete types were used directly.

3. **Alternative implementations**: Enable easy swapping of backing data structures
   for benchmarking and optimization:
   - Lists: Vector (current) vs Seq vs plain lists
   - AttrSets: HashMap (current) vs Map (ordered iteration)
   - Strings: Text (current) vs ByteString vs ShortText

4. **Modular builtins**: Builtin functions should be defined against abstract
   signatures, allowing them to work with any conforming implementation.

### Why Not Type Classes?

Type classes can achieve similar abstraction but have drawbacks:

| Aspect | Type Classes | Backpack |
|--------|-------------|----------|
| Specialization | Requires INLINABLE + SPECIALIZE pragmas | **Automatic** at link time |
| Dictionary passing | Can occur if GHC misses specialization | **Never** - concrete types |
| Verification | Must use inspection-testing to verify | Guaranteed by design |
| Maintenance | Must audit each call site | Define signature once |

With Backpack, once the signature is satisfied, all uses are guaranteed to be
monomorphized. There's no risk of accidentally losing specialization.

## Architecture Overview

All Backpack components are organized as **named sublibraries** within the single
`hnix.cabal` file. This simplifies versioning, reduces maintenance overhead, and
keeps all related code together.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         SUBLIBRARIES IN hnix.cabal                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────────┐                                                        │
│  │   hnix-types    │  Shared types (VarName, Path, SourcePos, Atom)        │
│  └────────┬────────┘                                                        │
│           │                                                                 │
│  ┌────────▼────────┬─────────────────┬─────────────────┐                   │
│  │ hnix-attrset-sig│  hnix-list-sig  │ hnix-string-sig │  SIGNATURES       │
│  │   (.hsig only)  │   (.hsig only)  │   (.hsig only)  │                   │
│  └────────┬────────┴────────┬────────┴────────┬────────┘                   │
│           │                 │                 │                             │
│  ┌────────▼────────┬────────▼────────┬────────▼────────┐                   │
│  │  hnix-attrset   │   hnix-list     │   hnix-string   │  IMPLEMENTATIONS  │
│  │   (hashmap)     │ (vector+intern) │ (text+internal) │                   │
│  └────────┬────────┴────────┬────────┴────────┬────────┘                   │
│           │                 │                 │                             │
│  ┌────────▼─────────────────▼─────────────────▼────────┐                   │
│  │                      hnix-core                       │  INDEFINITE       │
│  │       (requires AttrSet.Sig, List.Sig, String.Sig)   │  (merged)         │
│  │                                                      │                   │
│  │   Nix.Core.AttrSet, List, Scope, Expr.Types          │                   │
│  │   Nix.Core.Value.*, Thunk.*, Protocol, Frames...     │                   │
│  └────────────────────────┬────────────────────────────┘                   │
│                           │                                                 │
│  ┌────────────────────────▼────────────────────────────┐                   │
│  │     hnix-builtins-list    hnix-builtins-attrset     │  INDEFINITE       │
│  └────────────────────────┬────────────────────────────┘                   │
│                           │                                                 │
│  ┌────────────────────────▼────────────────────────────┐                   │
│  │                    library (main)                    │  INSTANTIATES ALL │
│  │              via mixins + reexported-modules         │                   │
│  └─────────────────────────────────────────────────────┘                   │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Directory Structure

```
hnix/
├── hnix.cabal                    # All sublibraries defined here
├── types/                        # hnix-types sublibrary
│   └── Nix/Types/
│       ├── Atom.hs
│       ├── Path.hs
│       ├── SourcePos.hs
│       └── VarName.hs
├── signatures/                   # Signature sublibraries (.hsig files)
│   └── Nix/
│       ├── AttrSet/Sig.hsig
│       ├── List/Sig.hsig
│       └── String/Sig.hsig
├── implementations/              # Implementation sublibraries
│   ├── attrset/                  # hnix-attrset (HashMap)
│   │   └── Nix/AttrSet/HashMap.hs
│   ├── list/                     # hnix-list-internal (Vector)
│   │   └── Nix/List/Vector.hs
│   ├── string/                   # hnix-string-internal (Text)
│   │   └── Nix/String/Text.hs
│   │   └── Nix/String/Text/Context.hs
│   └── string-public/            # hnix-string (NixLike)
│       └── Nix/String/Text/NixLike.hs
├── core/                         # hnix-core (merged core + value)
│   └── Nix/Core/
│       ├── AttrSet.hs
│       ├── List.hs
│       ├── Scope.hs
│       ├── Utils.hs
│       ├── Expr/Types.hs
│       └── Value/
│           ├── Equal.hs
│           ├── Frames.hs
│           ├── Interned.hs
│           ├── Monad.hs
│           ├── Protocol.hs
│           ├── String.hs
│           └── Thunk/Basic.hs
├── builtins/                     # Builtin sublibraries
│   ├── list/                     # hnix-builtins-list
│   │   └── Nix/Builtins/List.hs
│   └── attrset/                  # hnix-builtins-attrset
│       └── Nix/Builtins/AttrSet.hs
└── src/                          # Main library sources
```

## Sublibrary Details

### Signature Sublibraries

Signature sublibraries define abstract interfaces using GHC's `.hsig` files.

#### hnix-list-sig

Defines the abstract list type used for Nix lists:

```haskell
signature Nix.List.Sig where

data NixList a
instance Eq a => Eq (NixList a)
instance Functor NixList
instance Foldable NixList
instance Traversable NixList
-- ...

empty    :: NixList a
length   :: NixList a -> Int           -- O(1) required
elemAt   :: NixList a -> Int -> Maybe a -- O(1) required
head     :: NixList a -> Maybe a
tail     :: NixList a -> Maybe (NixList a)
fromList :: [a] -> NixList a
toList   :: NixList a -> [a]
filterM  :: Monad m => (a -> m Bool) -> NixList a -> m (NixList a)
-- ...
```

#### hnix-attrset-sig

Defines the abstract attribute set type:

```haskell
signature Nix.AttrSet.Sig where

data AttrSet v
instance Eq v => Eq (AttrSet v)
instance Functor AttrSet
instance Foldable AttrSet
-- ...

empty      :: AttrSet v
lookup     :: VarName -> AttrSet v -> Maybe v
insert     :: VarName -> v -> AttrSet v -> AttrSet v
delete     :: VarName -> AttrSet v -> AttrSet v
unionRight :: AttrSet v -> AttrSet v -> AttrSet v
keys       :: AttrSet v -> [VarName]
-- ...
```

#### hnix-string-sig

Defines the abstract string type with context tracking:

```haskell
signature Nix.String.Sig where

data NixString
data StringContext
data ContextFlavor

mkNixStringWithoutContext :: Text -> NixString
mkNixString :: HashSet StringContext -> Text -> NixString
ignoreContext :: NixString -> Text
getStringNoContext :: NixString -> Maybe Text
hasContext :: NixString -> Bool

-- Smart constructors (pattern synonyms not supported in signatures)
mkDirectPath :: ContextFlavor
mkAllOutputs :: ContextFlavor
mkDerivationOutput :: Text -> ContextFlavor

-- Predicates for pattern matching
isDirectPath :: ContextFlavor -> Bool
isAllOutputs :: ContextFlavor -> Bool
isDerivationOutput :: ContextFlavor -> Bool
```

**Note**: Backpack signatures don't support pattern synonyms, so `ContextFlavor`
uses smart constructors (`mkDirectPath`, etc.) and predicates (`isDirectPath`, etc.)
instead.

### Implementation Sublibraries

Implementation sublibraries provide concrete types that satisfy signatures.

#### hnix-attrset (HashMap)

HashMap-backed attribute set implementation:

```haskell
newtype AttrSet v = AttrSet (HashMap VarName v)

lookup k (AttrSet m) = HM.lookup k m   -- O(1) average
insert k v (AttrSet m) = AttrSet (HM.insert k v m)
```

**Why HashMap?** Attribute access is the most common operation in Nix evaluation.
HashMap provides O(1) average case for lookups.

#### hnix-list (Vector)

Vector-backed list implementation (via `hnix-list-internal`):

```haskell
newtype NixList a = NixList (Vector a)

length (NixList v) = V.length v      -- O(1)
elemAt (NixList v) i = v V.!? i      -- O(1)
head (NixList v) = v V.!? 0          -- O(1)
tail (NixList v) = NixList <$> ...   -- O(1), shares memory
```

**Why Vector?** Nix semantics require frequent length checks and random access
(e.g., `builtins.elemAt`, `builtins.length`). Vector provides O(1) for both.

#### hnix-string (Text)

Text-backed string with HashSet context (via `hnix-string-internal`):

```haskell
data NixString = NixStringInternal !(HashSet StringContext) !Text

data StringContext = StringContextInternal !ContextFlavor !VarName

data ContextFlavor
  = DirectPath
  | AllOutputs
  | DerivationOutput Text
```

**Internal library pattern**: `hnix-list` and `hnix-string` use a two-tier structure:
- **Internal** (`hnix-list-internal`, `hnix-string-internal`): Core implementation
  with no unfilled signatures
- **Public** (`hnix-list`, `hnix-string`): Re-exports internal modules, may have
  additional modules that require signature instantiation

### Indefinite Sublibraries

Indefinite sublibraries depend on signatures but don't provide implementations.
They can't be used directly - they must be instantiated via mixins.

#### hnix-core

Core infrastructure (merged from former `hnix-core` and `hnix-value-core`):

- `Nix.Core.AttrSet` - Re-exports from AttrSet.Sig
- `Nix.Core.List` - Re-exports from List.Sig
- `Nix.Core.Scope` - Scope type for variable bindings
- `Nix.Core.Expr.Types` - PositionSet, ParamSet
- `Nix.Core.Value.*` - NValue types, thunks, protocols, frames

#### hnix-builtins-{list,attrset}

Builtin functions implemented against the abstract protocol:

```haskell
-- From hnix-builtins-list
lengthNix :: MonadListBuiltin t f m => NValue t f m -> m (NValue t f m)
lengthNix nv = do
  v <- V.demand nv
  case V.extractList v of
    Just lst -> pure $ V.mkInt $ fromIntegral $ V.listLength lst
    Nothing  -> V.throwTypeError "builtins.length: expected a list"
```

These builtins use the abstract `V.listLength`, `V.extractList`, etc. from
`Nix.Core.Value.Protocol`, which re-exports operations from the signatures.

## How Instantiation Works

The main library uses Cabal mixins to instantiate signatures:

```cabal
library
  build-depends:
    -- Sublibraries
    , hnix:hnix-types
    , hnix:hnix-attrset
    , hnix:hnix-list
    , hnix:hnix-string
    , hnix:hnix-core
    , hnix:hnix-builtins-list
    , hnix:hnix-builtins-attrset

  mixins:
    hnix:hnix-core
      requires
        (Nix.AttrSet.Sig as Nix.AttrSet.HashMap,
         Nix.List.Sig as Nix.List.Vector,
         Nix.String.Sig as Nix.String.Text),
    hnix:hnix-string
      requires
        (Nix.AttrSet.Sig as Nix.AttrSet.HashMap),
    hnix:hnix-builtins-list
      requires
        (Nix.AttrSet.Sig as Nix.AttrSet.HashMap,
         Nix.List.Sig as Nix.List.Vector,
         Nix.String.Sig as Nix.String.Text),
    hnix:hnix-builtins-attrset
      requires
        (Nix.AttrSet.Sig as Nix.AttrSet.HashMap,
         Nix.List.Sig as Nix.List.Vector,
         Nix.String.Sig as Nix.String.Text)
```

When GHC compiles the main library, it:

1. Sees `Nix.AttrSet.Sig as Nix.AttrSet.HashMap`
2. Verifies `Nix.AttrSet.HashMap` satisfies the `Nix.AttrSet.Sig` signature
3. Substitutes all uses of `AttrSet` with `HashMap`
4. Monomorphizes all operations - no indirection remains

## Adding a New Implementation

To add an alternative implementation (e.g., `hnix-list-seq` using `Data.Seq`):

### 1. Create the implementation sublibrary

Add a new sublibrary to `hnix.cabal`:

```cabal
library hnix-list-seq
  import: shared-sublibrary
  visibility: public
  hs-source-dirs: implementations/list-seq
  exposed-modules: Nix.List.Seq
  build-depends:
      base >= 4.12 && < 5
    , containers
    , deepseq
    , hashable
    , semialign
```

### 2. Implement the signature

Create `implementations/list-seq/Nix/List/Seq.hs`:

```haskell
module Nix.List.Seq
  ( NixList
  , empty, length, elemAt, head, tail, fromList, toList
  -- ... all operations from Nix.List.Sig
  ) where

import qualified Data.Sequence as S

newtype NixList a = NixList (S.Seq a)

length (NixList s) = S.length s
elemAt (NixList s) i = S.lookup i s
-- ...
```

### 3. Use in mixins (for testing/benchmarking)

Create a test or benchmark that uses the alternative implementation:

```cabal
test-suite bench-seq-list
  mixins:
    hnix:hnix-core
      requires (Nix.List.Sig as Nix.List.Seq, ...)
```

## Building Sublibraries

All sublibraries are built as part of the main `hnix` package:

```bash
# Build everything (recommended)
nix develop ".?submodules=1#" --command cabal build

# Build a specific sublibrary
nix develop ".?submodules=1#" --command cabal build hnix:hnix-types
nix develop ".?submodules=1#" --command cabal build hnix:hnix-core
nix develop ".?submodules=1#" --command cabal build hnix:hnix-attrset

# Build the main library (instantiates all signatures)
nix develop ".?submodules=1#" --command cabal build lib:hnix
```

Note the `hnix:sublibrary-name` syntax for referencing sublibraries within the
same package.

## Verification

### Inspection Tests

The `tests/inspection/` directory contains compile-time tests that verify
GHC produces the expected specialized code:

```bash
nix develop ".?submodules=1#" --command cabal test hnix-inspection
```

These tests use the `inspection-testing` library to verify:

- No dictionary passing for data structure operations
- Newtype wrappers are erased
- Branch elimination for singleton dispatch
- Full monomorphization of hot paths

### Memory Benchmarks

The `benchmarks/weigh/` directory contains memory allocation benchmarks:

```bash
nix develop ".?submodules=1#" --command cabal bench hnix-weigh
```

### Manual Verification

You can also inspect the generated Core:

```bash
cabal build lib:hnix -fforce-recompile -v \
  --ghc-options="-ddump-simpl -dsuppress-all -dsuppress-uniques"
```

Look for:
- Direct function calls (no `$fFooBar_$c...` dictionary selectors)
- Concrete types (Vector, HashMap, Text) instead of abstract types
- No `GHC.Classes.eq` or similar generic calls

## Signature Evolution Policy

This section provides guidelines for maintaining and evolving the Backpack signatures.

### Adding a New Operation to a Signature

When adding a new operation to an existing signature:

1. **Add to the signature file** (`.hsig`):
   ```haskell
   -- In signatures/Nix/List/Sig.hsig
   myNewOperation :: NixList a -> NixList a
   ```

2. **Implement in all implementations**:
   ```haskell
   -- In implementations/list/Nix/List/Vector.hs
   myNewOperation :: NixList a -> NixList a
   myNewOperation (NixList v) = NixList (V.someOperation v)
   ```

3. **Re-export from Protocol** (for operations used by builtins):
   ```haskell
   -- In core/Nix/Core/Value/Protocol.hs
   listMyNewOperation :: NixList a -> NixList a
   listMyNewOperation = L.myNewOperation
   {-# INLINE listMyNewOperation #-}
   ```

4. **Update Protocol's export list**.

5. **Run inspection tests** to verify monomorphization:
   ```bash
   nix develop ".?submodules=1#" --command cabal test hnix-inspection
   ```

### Adding a New Signature

When adding a completely new abstract type:

1. **Create signature sublibrary** in `hnix.cabal`:
   ```cabal
   library hnix-newthing-sig
     import: shared-sublibrary
     visibility: public
     hs-source-dirs: signatures
     exposed-modules: Nix.NewThing.Sig
     build-depends:
         base >= 4.12 && < 5
       , hnix:hnix-types
   ```

2. **Create `.hsig` file** in `signatures/Nix/NewThing/Sig.hsig`:
   ```haskell
   signature Nix.NewThing.Sig where

   data NewThing a

   empty :: NewThing a
   -- ... other operations

   instance Functor NewThing
   -- ... required instances
   ```

3. **Create implementation sublibrary**.

4. **Add signature to indefinite sublibraries** that need it.

5. **Add mixin mappings** in the main library.

### Removing an Operation from a Signature

1. **Check all usages** in the codebase:
   ```bash
   grep -r "operationName" core/ builtins/ src/
   ```

2. **Remove from implementations first**.

3. **Remove from signature**.

4. **Remove from Protocol** (if re-exported).

5. **Verify build passes**.

### Checklist for Signature Changes

- [ ] Signature updated (`signatures/Nix/*/Sig.hsig`)
- [ ] All implementations updated
- [ ] Protocol updated (if needed)
- [ ] Inspection tests pass (`cabal test hnix-inspection`)
- [ ] Memory benchmarks checked (`cabal bench hnix-weigh`)
- [ ] Documentation updated

### Common Backpack Limitations to Watch For

| Limitation | Workaround |
|------------|------------|
| No pattern synonyms in signatures | Use smart constructors + predicates |
| No type families in signatures | Use associated types or concrete parameters |
| Transitive signature requirements | Re-declare signatures in each indefinite package |
| hsig files have restricted imports | Only import other signatures or concrete types |

## Future Work

### Potential Alternative Implementations

| Sublibrary | Backing Store | Use Case |
|------------|---------------|----------|
| `hnix-list-seq` | `Data.Seq` | Better cons/snoc performance |
| `hnix-attrset-map` | `Data.Map` | Ordered iteration |
| `hnix-string-bytestring` | `ByteString` | FFI interop |
| `hnix-string-shorttext` | `ShortText` | Memory efficiency |

### Remaining Work

1. Benchmark alternative implementations
2. Consider making Path type abstract via Backpack
3. Profile and optimize based on real-world Nixpkgs evaluation

## References

- [GHC User Guide: Backpack](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html#module-signatures)
- [Backpack Paper](https://plv.mpi-sws.org/backpack/)
- [inspection-testing](https://hackage.haskell.org/package/inspection-testing)
