{-# LANGUAGE NoStrict #-}

-- | Primitive operations for the compiled Nix runtime.
--
-- These functions are called by generated GHC Core code. They implement
-- the semantics of Nix operators and type coercion. Error handling uses
-- Haskell exceptions (NixError) which propagate through the generated code.
--
-- Design principles:
-- * Match Nix semantics exactly (checked arithmetic, type coercion rules)
-- * Throw NixError on type mismatches rather than returning Maybe
-- * INLINE aggressively - these are hot paths
-- * No thunk management - GHC handles that
--
-- This module re-exports primitives from specialized submodules:
-- * "Nix.Compile.Primops.Coerce" - Type coercion and numeric promotion
-- * "Nix.Compile.Primops.Arithmetic" - Numeric operations
-- * "Nix.Compile.Primops.Comparison" - Equality and ordering
-- * "Nix.Compile.Primops.Logical" - Boolean operations
-- * "Nix.Compile.Primops.String" - String operations
-- * "Nix.Compile.Primops.Collection" - List and attribute set operations
-- * "Nix.Compile.Primops.Control" - Function application, control flow, path resolution
module Nix.Compile.Primops
  ( -- * Type coercion (throw on mismatch)
    expectInt
  , expectFloat
  , expectBool
  , expectString
  , expectPath
  , expectList
  , expectAttrs
  , expectFunction
    -- * Type predicates (for safe checking)
  , isAttrsPrimop
    -- * Numeric type promotion
  , toNumeric
    -- * Arithmetic
  , nixAdd
  , nixSub
  , nixMul
  , nixDiv
  , nixNeg
    -- * Comparison
  , nixEq
  , nixEqBool
  , nixNEq
  , nixLt
  , nixLte
  , nixGt
  , nixGte
    -- * Logical
  , nixNot
  , nixAnd
  , nixOr
  , nixImpl
    -- * String operations
  , nixStringConcat
  , nixCoerceToString
  , stringToPath
    -- * List operations
  , nixListConcat
    -- * Attribute set operations
  , nixUpdate
  , nixSelect
  , nixSelectOr
  , nixSelectPath
  , nixHasAttr
  , nixHasAttrPath
    -- * Function application
  , nixApply
    -- * Pattern set validation
  , nixCheckClosedPattern
    -- * Control flow
  , nixAssert
  , nixThrow
  , nixAbort
    -- * Path resolution
  , resolveEnvPath
  , nixResolveEnvPath
  ) where

-- Re-export from specialized submodules
import Nix.Compile.Primops.Coerce
  ( expectInt
  , expectFloat
  , expectBool
  , expectString
  , expectPath
  , expectList
  , expectAttrs
  , expectFunction
  , isAttrsPrimop
  , toNumeric
  )

import Nix.Compile.Primops.Arithmetic
  ( nixAdd
  , nixSub
  , nixMul
  , nixDiv
  , nixNeg
  )

import Nix.Compile.Primops.Comparison
  ( nixEq
  , nixEqBool
  , nixNEq
  , nixLt
  , nixLte
  , nixGt
  , nixGte
  )

import Nix.Compile.Primops.Logical
  ( nixNot
  , nixAnd
  , nixOr
  , nixImpl
  )

import Nix.Compile.Primops.String
  ( nixStringConcat
  , nixCoerceToString
  , stringToPath
  )

import Nix.Compile.Primops.Collection
  ( nixListConcat
  , nixUpdate
  , nixSelect
  , nixSelectOr
  , nixSelectPath
  , nixHasAttr
  , nixHasAttrPath
  )

import Nix.Compile.Primops.Control
  ( nixApply
  , nixCheckClosedPattern
  , nixAssert
  , nixThrow
  , nixAbort
  , resolveEnvPath
  , nixResolveEnvPath
  )
