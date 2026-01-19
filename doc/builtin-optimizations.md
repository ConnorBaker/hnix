# Builtin Optimizations for Empty/Singleton Collections

This document lists all Nix builtins and notes which ones have optimizations for empty or singleton inputs/outputs.

## Summary

The following optimizations reduce allocations by returning interned (cached) empty values instead of creating fresh empty collections:

- **Interned Empty List**: `askInternedEmptyList` returns a cached `[]`
- **Interned Empty Set**: `askInternedEmptySet` returns a cached `{}`
- **Interned Booleans**: `askInternedTrue`, `askInternedFalse`, `askInternedBool` return cached `true`/`false`
- **Interned Null**: `askInternedNull` returns cached `null`

## Optimized Builtins

### List Operations with Empty Input Fast Paths

| Builtin | Optimization | Description |
|---------|--------------|-------------|
| `map` | Empty input | Returns interned `[]` when input list is empty |
| `filter` | Empty input/output | Returns interned `[]` when input is empty OR when all elements are filtered out |
| `tail` | Singleton input | Returns interned `[]` when input is a singleton list |
| `sort` | Empty input | Returns interned `[]` when input is empty; skips sorting for singletons |
| `concatLists` | Empty input/output | Returns interned `[]` when input is empty OR when result is empty |
| `concatMap` | Empty input/output | Returns interned `[]` when input is empty OR when result is empty |
| `genList` | n=0 | Returns interned `[]` when n is 0 |
| `catAttrs` | Empty input/output | Returns interned `[]` when input is empty OR when no matching attrs found |
| `partition` | Empty input | Returns interned `[]` for both `.right` and `.wrong` when input is empty |
| `attrNames` | Empty input | Returns interned `[]` when input set is empty |
| `attrValues` | Empty input | Returns interned `[]` when input set is empty |

### Set Operations with Empty Input Fast Paths

| Builtin | Optimization | Description |
|---------|--------------|-------------|
| `mapAttrs` | Empty input | Returns interned `{}` when input set is empty |
| `listToAttrs` | Empty input | Returns interned `{}` when input list is empty |
| `groupBy` | Empty input | Returns interned `{}` when input list is empty |
| `intersectAttrs` | Empty input/output | Returns interned `{}` when either input is empty OR when no common keys |
| `zipAttrsWith` | Empty input/output | Returns interned `{}` when input list is empty OR when result is empty |
| `removeAttrs` | Empty result | Returns interned `{}` when all attributes are removed |

### Boolean Operations with Interned Returns

All boolean-returning operations now use interned `true`/`false` values when provenance tracking is disabled:

| Builtin/Operation | Optimization | Description |
|-------------------|--------------|-------------|
| `true` literal | Always interned | Returns interned `true` value |
| `false` literal | Always interned | Returns interned `false` value |
| `null` literal | Always interned | Returns interned `null` value |
| `hasAttr` | Interned boolean | Returns interned `false` for empty set (fast path), otherwise interned boolean result |
| `elem` | Interned boolean | Returns interned `false` for empty list (fast path), otherwise interned boolean result |
| `any` | Interned boolean | Returns interned boolean result |
| `all` | Interned boolean | Returns interned boolean result |
| `hasContext` | Interned boolean | Returns interned boolean result |
| `pathExists` | Interned boolean | Returns interned boolean result |
| `isAttrs` | Interned boolean | Returns interned boolean result |
| `isBool` | Interned boolean | Returns interned boolean result |
| `isFloat` | Interned boolean | Returns interned boolean result |
| `isFunction` | Interned boolean | Returns interned boolean result |
| `isInt` | Interned boolean | Returns interned boolean result |
| `isList` | Interned boolean | Returns interned boolean result |
| `isNull` | Interned boolean | Returns interned boolean result |
| `isPath` | Interned boolean | Returns interned boolean result |
| `isString` | Interned boolean | Returns interned boolean result |
| `!` (not) | Interned boolean | Returns interned boolean result |
| `==`, `!=` | Interned boolean | Returns interned boolean result |
| `<`, `<=`, `>`, `>=` | Interned boolean | Returns interned boolean result |
| `&&`, `||`, `->` | Interned boolean | Returns interned boolean result |

### Binary Operator Optimizations

| Operator | Optimization | Description |
|----------|--------------|-------------|
| `++` (list concat) | Empty operands | Returns interned `[]` when both empty; returns other operand when one is empty |
| `//` (set update) | Empty operands | Returns interned `{}` when both empty; returns other operand when one is empty |
| `+` (string concat) | Empty strings | Returns the other operand when one string is empty AND has no context (context must be preserved for store paths) |

## Non-Optimized Builtins

The following builtins do not have empty/singleton optimizations either because:
1. They always return non-empty results
2. Optimization would add complexity without significant benefit

### Arithmetic (Return numbers)
- `add`, `sub`, `mul`, `div`, `bitAnd`, `bitOr`, `bitXor`, `ceil`, `floor`

### String Operations
- `concatStringsSep`, `stringLength`, `substring`, `replaceStrings`, `match`, `split`

### List Operations (Not applicable)
- `head` - Always returns single element or throws
- `elemAt` - Always returns single element or throws
- `length` - Returns number
- `foldl'` - Depends on accumulator, not applicable

### Set Operations (Not applicable)
- `getAttr` - Returns single value

### I/O and System
- Various fetch*, read*, exec, import operations

## Testing

Pointer equality tests verify that optimized builtins return the exact interned value object (not a fresh copy). See `tests/InternedValueTests.hs`.

Run tests with:
```bash
nix develop ".?submodules=1#" --command cabal test hnix-tests --test-options="-j1 -p Interned"
```

The test suite includes:
- 13 empty list fast path tests
- 7 empty set fast path tests
- 9 boolean interning tests (literals + builtins)
- 1 null interning test
- 3 sanity checks

## Benchmarks

Memory allocation benchmarks for fast paths are in `benchmarks/weigh/Main.hs` under the "Empty list fast paths" and "Empty set fast paths" groups.

Run benchmarks with:
```bash
nix develop ".?submodules=1#" --command cabal bench hnix-weigh
```

## Implementation Details

### Location of Interned Values

- **Definition**: `src/Nix/Value/Interned.hs` - `InternedValues` record
- **Accessors**: `src/Nix/Exec.hs` - `askInternedEmptyList`, `askInternedEmptySet`, `askInternedBool`, `askInternedNull`, etc.
- **Creation**: `src/Nix/Standard.hs` - Created once per evaluation context

### String Constants

- **Empty context**: `src/Nix/String.hs` - `emptyStringContext`, `nixStringEmpty`, `nixStringOne`
- **Position set**: `src/Nix/Expr/Types.hs` - `emptyPositionSet`

### Provenance Interaction

When provenance tracking is enabled (for debugging), fresh values are created with provenance info attached. The interned value optimizations only apply when provenance tracking is disabled (the common case for production use).

The `withProvCtx` helper in `src/Nix/Exec.hs` provides zero-cost dispatch between provenance-enabled and provenance-disabled code paths.
