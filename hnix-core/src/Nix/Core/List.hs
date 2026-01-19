-- | Re-export NixList operations from the Backpack signature.
--
-- This module provides a stable API for list operations.
-- The concrete implementation is selected at link time via Backpack mixins.
module Nix.Core.List
  ( -- * Types
    NixList
    -- * Core operations
  , nlLength
  , nlIndex
  , nlNull
  , nlUncons
  , nlCons
  , nlSnoc
  , nlAppend
    -- * Conversion
  , nlFromList
  , nlToList
    -- * Higher-order operations
  , nlMap
  , nlFilter
  , nlTraverse
  , nlReverse
  , nlEmpty
  , nlFoldl'
  , nlHead
  , nlTail
  ) where

import Nix.List.Sig
