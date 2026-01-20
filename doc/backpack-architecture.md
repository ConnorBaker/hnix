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

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              SIGNATURE LAYER                                │
│         (Abstract interfaces - no concrete implementation)                  │
├───────────────────┬───────────────────┬─────────────────────────────────────┤
│  hnix-list-sig    │  hnix-attrset-sig │  hnix-string-sig                    │
│  Nix.List.Sig     │  Nix.AttrSet.Sig  │  Nix.String.Sig                     │
│                   │                   │                                     │
│  data NixList a   │  data AttrSet v   │  data NixString                     │
│  length, head,    │  lookup, insert,  │  mkNixString, ignoreContext,        │
│  tail, elemAt,    │  delete, union,   │  hasContext, getStringContext,      │
│  fromList, etc.   │  keys, etc.       │  StringContext, ContextFlavor       │
└─────────┬─────────┴─────────┬─────────┴──────────────────┬──────────────────┘
          │                   │                            │
          ▼                   ▼                            ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           IMPLEMENTATION LAYER                              │
│              (Concrete implementations satisfying signatures)               │
├───────────────────┬───────────────────┬─────────────────────────────────────┤
│ hnix-list-vector  │hnix-attrset-      │  hnix-string-text                   │
│                   │    hashmap        │                                     │
│ NixList = Vector  │ AttrSet = HashMap │  NixString = Text + HashSet Context │
│                   │                   │                                     │
│ O(1) length/index │ O(1) avg lookup   │  Context tracking for store paths   │
└─────────┬─────────┴─────────┬─────────┴──────────────────┬──────────────────┘
          │                   │                            │
          ▼                   ▼                            ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          INDEFINITE PACKAGES                                │
│     (Use signatures - not directly usable, require instantiation)           │
├─────────────────────────────────────────────────────────────────────────────┤
│  hnix-value-core          Core NValue types, thunks, evaluation protocol    │
│  hnix-builtins-list       List builtins: length, head, map, filter, etc.    │
│  hnix-builtins-attrset    AttrSet builtins: hasAttr, getAttr, mapAttrs      │
│  hnix-builtins-string     String builtins: hashString, substring, etc.      │
│  hnix-core                Expression types, scope, utilities                │
└─────────────────────────────────────────────────────────────────────────────┘
          │
          │ Cabal mixins instantiate signatures with implementations
          ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              MAIN HNIX PACKAGE                              │
│                                                                             │
│  mixins:                                                                    │
│    hnix-value-core requires (Nix.List.Sig as Nix.List.Vector,               │
│                              Nix.AttrSet.Sig as Nix.AttrSet.HashMap,        │
│                              Nix.String.Sig as Nix.String.Text)             │
│    hnix-builtins-list requires (...)                                        │
│    hnix-builtins-attrset requires (...)                                     │
│    hnix-builtins-string requires (...)                                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Package Details

### Signature Packages

Signature packages define abstract interfaces using GHC's `.hsig` files.

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

### Implementation Packages

Implementation packages provide concrete types that satisfy signatures.

#### hnix-list-vector

Vector-backed list implementation:

```haskell
newtype NixList a = NixList (Vector a)

length (NixList v) = V.length v      -- O(1)
elemAt (NixList v) i = v V.!? i      -- O(1)
head (NixList v) = v V.!? 0          -- O(1)
tail (NixList v) = NixList <$> ...   -- O(1), shares memory
```

**Why Vector?** Nix semantics require frequent length checks and random access
(e.g., `builtins.elemAt`, `builtins.length`). Vector provides O(1) for both.

#### hnix-attrset-hashmap

HashMap-backed attribute set implementation:

```haskell
newtype AttrSet v = AttrSet (HashMap VarName v)

lookup k (AttrSet m) = HM.lookup k m   -- O(1) average
insert k v (AttrSet m) = AttrSet (HM.insert k v m)
```

**Why HashMap?** Attribute access is the most common operation in Nix evaluation.
HashMap provides O(1) average case for lookups.

#### hnix-string-text

Text-backed string with HashSet context:

```haskell
data NixString = NixStringInternal !(HashSet StringContext) !Text

data StringContext = StringContextInternal !ContextFlavor !VarName

data ContextFlavor
  = DirectPath
  | AllOutputs
  | DerivationOutput Text
```

### Indefinite Packages

Indefinite packages depend on signatures but don't provide implementations.
They can't be used directly - they must be instantiated via mixins.

#### hnix-value-core

Core value infrastructure:

- `Nix.Value.Core.Value` - NValue type and pattern synonyms
- `Nix.Value.Core.Thunk` - Thunk types and operations
- `Nix.Value.Core.Protocol` - Abstract interface for builtins
- `Nix.Value.Core.String` - Re-exports string signature
- `Nix.Value.Core.Interned` - Singleton interned values (empty list, true, false)

#### hnix-builtins-{list,attrset,string}

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
`Nix.Value.Core.Protocol`, which re-exports operations from the signatures.

## How Instantiation Works

The main `hnix` package uses Cabal mixins to instantiate signatures:

```cabal
library
  build-depends:
    , hnix-value-core
    , hnix-builtins-list
    , hnix-list-vector
    , hnix-attrset-hashmap
    , hnix-string-text
    -- ...

  mixins:
    hnix-value-core
      requires (Nix.List.Sig as Nix.List.Vector,
                Nix.AttrSet.Sig as Nix.AttrSet.HashMap,
                Nix.String.Sig as Nix.String.Text),
    hnix-builtins-list
      requires (Nix.List.Sig as Nix.List.Vector,
                Nix.String.Sig as Nix.String.Text),
    -- ...
```

When GHC compiles the main package, it:

1. Sees `Nix.List.Sig as Nix.List.Vector`
2. Verifies `Nix.List.Vector` satisfies the `Nix.List.Sig` signature
3. Substitutes all uses of `NixList` with `Vector`
4. Monomorphizes all operations - no indirection remains

## Adding a New Implementation

To add an alternative implementation (e.g., `hnix-list-seq` using `Data.Seq`):

### 1. Create the implementation package

```
implementations/hnix-list-seq/
├── hnix-list-seq.cabal
└── src/Nix/List/Seq.hs
```

### 2. Implement the signature

```haskell
-- src/Nix/List/Seq.hs
module Nix.List.Seq
  ( NixList
  , empty, length, elemAt, head, tail, fromList, toList
  -- ... all operations from Nix.List.Sig
  ) where

import qualified Data.Seq as S

newtype NixList a = NixList (S.Seq a)

length (NixList s) = S.length s
elemAt (NixList s) i = S.lookup i s
-- ...
```

### 3. Add to cabal.project

```cabal
packages:
  implementations/hnix-list-seq
```

### 4. Use in mixins (for testing)

```cabal
-- In a test package or benchmark
mixins:
  hnix-value-core
    requires (Nix.List.Sig as Nix.List.Seq, ...)
```

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

## Future Work

### Potential Alternative Implementations

| Package | Backing Store | Use Case |
|---------|---------------|----------|
| `hnix-list-seq` | `Data.Seq` | Better cons/snoc performance |
| `hnix-attrset-map` | `Data.Map` | Ordered iteration |
| `hnix-string-bytestring` | `ByteString` | FFI interop |
| `hnix-string-shorttext` | `ShortText` | Memory efficiency |

### Remaining Work

1. Add inspection tests for string operations
2. Benchmark alternative implementations
3. Consider making Path type abstract via Backpack
4. Profile and optimize based on real-world Nixpkgs evaluation

## References

- [GHC User Guide: Backpack](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html#module-signatures)
- [Backpack Paper](https://plv.mpi-sws.org/backpack/)
- [inspection-testing](https://hackage.haskell.org/package/inspection-testing)
