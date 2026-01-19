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
  , insertWith
  , intersection
  , intersectionWith
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
  , mapMaybe
  , alterF
    -- * Lens operations
  , hashAt
    -- * Re-exports for convenience
  , module Nix.Types.VarName
  ) where

import Relude (Functor, Maybe, flip)
import Nix.AttrSet.Sig
import Nix.Types.VarName (VarName, mkVarName, varNameText)

-- | Lens for accessing a specific key in an AttrSet.
-- Returns Nothing if the key is not present.
-- Setting to Nothing deletes the key, setting to Just v inserts/updates.
hashAt
  :: Functor f
  => VarName
  -> (Maybe v -> f (Maybe v))
  -> AttrSet v
  -> f (AttrSet v)
hashAt = flip alterF
{-# INLINE hashAt #-}
