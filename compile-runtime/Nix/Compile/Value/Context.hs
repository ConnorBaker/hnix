{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoStrict #-}

-- |
-- Module      : Nix.Compile.Value.Context
-- Description : Context tracking for Nix string dependencies
-- Copyright   : (c) 2024 Haskell-Nix Contributors
-- License     : See LICENSE file
--
-- Nix strings carry context that tracks which store paths they depend on.
-- This information is used during derivation building to ensure all
-- dependencies are available.
--
-- Context is represented as a set of @StringContext@ entries, each recording:
-- * The store path or derivation path being depended upon
-- * How the dependency is used (direct path, all outputs, or specific output)
module Nix.Compile.Value.Context
  ( ContextFlavor (..)
  , StringContext (..)
  , NixContext
  , emptyContext
  , pathContext
  , singletonContext
  , allOutputsContext
  , derivationOutputContext
  , unionContext
  , hasContext
  , contextToList
  , contextFromList
  )
where

import Data.Hashable ()
import qualified Data.HashSet as HS
import Relude

-- * Context types

-- | Context flavor for Nix strings.
--
-- Nix strings carry context that tracks which store paths they depend on.
-- This information is used during derivation building to ensure all
-- dependencies are available.
data ContextFlavor
  = DirectPath
    -- ^ Direct store path reference.
    -- The string contains or was derived from this store path directly.
    -- Example: The result of @toString /nix/store/abc...-foo@
  | AllOutputs
    -- ^ All outputs of a derivation.
    -- Used when referencing a derivation without specifying a particular output.
    -- The string depends on all outputs being available.
  | DerivationOutput !Text
    -- ^ Specific derivation output (e.g., "out", "dev", "lib").
    -- Used when a string references a specific output of a multi-output derivation.
  deriving (Eq, Ord, Show, Generic)

instance NFData ContextFlavor

instance Hashable ContextFlavor where
  hashWithSalt s DirectPath = hashWithSalt s (0 :: Int)
  hashWithSalt s AllOutputs = hashWithSalt s (1 :: Int)
  hashWithSalt s (DerivationOutput out) = hashWithSalt s (2 :: Int, out)

-- | String context entry - tracks a single dependency.
--
-- Each entry records:
-- * The store path or derivation path being depended upon
-- * How the dependency is used (direct path, all outputs, or specific output)
data StringContext = StringContext
  { scFlavor :: !ContextFlavor
    -- ^ How this dependency is used
  , scPath   :: !Text
    -- ^ The store path or derivation path
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData StringContext

instance Hashable StringContext where
  hashWithSalt s (StringContext flavor path) = hashWithSalt s (flavor, path)

-- * NixContext

-- | Context is a set of dependencies.
--
-- A Nix string's context is the set of all store paths it depends on,
-- along with information about how each dependency is used.
type NixContext = HashSet StringContext

-- | Empty context (for string literals with no store path dependencies)
emptyContext :: NixContext
emptyContext = HS.empty
{-# NOINLINE emptyContext #-}

-- | Create context for a direct path reference.
-- Used when converting a path to a string.
pathContext :: Text -> NixContext
pathContext p = HS.singleton (StringContext DirectPath p)
{-# INLINE pathContext #-}

-- | Singleton context with a direct path (legacy compatibility).
-- Prefer 'pathContext' for new code.
singletonContext :: Text -> NixContext
singletonContext = pathContext
{-# INLINE singletonContext #-}

-- | Create context for all outputs of a derivation.
allOutputsContext :: Text -> NixContext
allOutputsContext p = HS.singleton (StringContext AllOutputs p)
{-# INLINE allOutputsContext #-}

-- | Create context for a specific derivation output.
derivationOutputContext :: Text -> Text -> NixContext
derivationOutputContext path output =
  HS.singleton (StringContext (DerivationOutput output) path)
{-# INLINE derivationOutputContext #-}

-- | Union of contexts (combines all dependencies).
unionContext :: NixContext -> NixContext -> NixContext
unionContext = HS.union
{-# INLINE unionContext #-}

-- | Check if a context has any dependencies.
hasContext :: NixContext -> Bool
hasContext = not . HS.null
{-# INLINE hasContext #-}

-- | Convert context to a list (for iteration).
contextToList :: NixContext -> [StringContext]
contextToList = HS.toList
{-# INLINE contextToList #-}

-- | Create context from a list.
contextFromList :: [StringContext] -> NixContext
contextFromList = HS.fromList
{-# INLINE contextFromList #-}
