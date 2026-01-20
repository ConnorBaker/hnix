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
  , nlUnsafeIndex
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
  , nlMapM
  , nlFilter
  , nlFilterM
  , nlMapMaybe
  , nlTraverse
  , nlReverse
  , nlEmpty
  , nlFoldl'
  , nlFoldM'
  , nlFoldr
  , nlHead
  , nlTail
  , nlGenerate
  , nlGenerateM
  , nlConcat
  , nlSingleton
  , nlUnsafeTail
  , nlFoldr'
  , nlPartition
  , nlPartitionM
  ) where

import Nix.List.Sig
