{-# options_ghc -Wno-missing-signatures #-}

-- | Provenance tracking types.
--
-- This module previously provided provenance tracking for values (tracking where
-- values came from during evaluation). Provenance has been removed to simplify
-- the codebase. This module is kept for backwards compatibility but exports only
-- trivial implementations.
module Nix.Cited
  ( -- * Trivial HasCitations classes (no-op implementations)
    HasCitations1(..)
  , HasCitations(..)
  ) where

import           Nix.Prelude

-- | Typeclass for types that can track citations (provenance).
-- Since provenance is removed, this is now a trivial no-op implementation.
class HasCitations1 m v f where
  -- | Get citations from a value (always empty list now).
  citations1 :: f a -> [()]
  citations1 _ = []
  -- | Add provenance to a value (identity function now).
  addProvenance1 :: () -> f a -> f a
  addProvenance1 _ = id

-- | Typeclass for values that can track citations.
-- Since provenance is removed, this is now a trivial no-op implementation.
class HasCitations m v a where
  -- | Get citations from a value (always empty list now).
  citations :: a -> [()]
  citations _ = []
  -- | Add provenance to a value (identity function now).
  addProvenance :: () -> a -> a
  addProvenance _ = id
