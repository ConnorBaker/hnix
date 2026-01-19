-- | Re-export AttrSet operations from the Backpack signature.
--
-- This module provides a stable API for attribute set operations.
-- The concrete implementation is selected at link time via Backpack mixins.
module Nix.Core.AttrSet
  ( -- * Types
    AttrSet
    -- * Core operations
  , empty
  , singleton
  , insert
  , delete
  , lookup
  , member
    -- * Bulk operations
  , union
  , unionWith
  , intersection
  , difference
    -- * Conversion
  , fromList
  , toList
  , keys
  , elems
    -- * Properties
  , null
  , size
    -- * Higher-order operations
  , mapWithKey
  , traverseWithKey
  , foldlWithKey'
  , filterWithKey
  , alterF
    -- * Re-exports for convenience
  , module Nix.Types.VarName
  ) where

import Nix.AttrSet.Sig
import Nix.Types.VarName (VarName, mkVarName, varNameText)
