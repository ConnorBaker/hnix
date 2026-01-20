-- | Re-export NixList operations from the Backpack signature.
--
-- This module provides a stable API for list operations.
-- Function names match Nix's builtin list operations where applicable.
-- The concrete implementation is selected at link time via Backpack mixins.
--
-- For pure folds, use the Foldable instance (foldl', foldr, etc.)
-- For pure mapping, use the Functor instance (fmap).
-- For pure traversal, use the Traversable instance (traverse).
module Nix.Core.List
  ( -- * Types
    NixList
    -- * Monadic operations for Nix builtins
  , filterM
  , foldM'
  , genListM
  , partitionM
    -- * Core operations
  , empty
  , null
  , length
  , elemAt
  , unsafeElemAt
  , head
  , tail
  , unsafeTail
  , singleton
  , uncons
  , cons
  , snoc
  , append
  , fromList
  , toList
  , reverse
  ) where

import Nix.List.Sig
