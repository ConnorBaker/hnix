{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Core expression types using Backpack signatures.
--
-- This module provides the key types from Nix.Expr.Types but uses
-- the abstract AttrSet from the Backpack signature instead of
-- hardcoding HashMap.
module Nix.Core.Expr.Types
  ( -- * AttrSet type (from signature)
    AttrSet
    -- * Position tracking
  , PositionSet
  , emptyPositionSet
    -- * Parameter sets
  , ParamSet
  , paramSetToSortedList
    -- * Re-exports
  , module Nix.Types.VarName
  , module Nix.Types.SourcePos
  , module Nix.Types.Atom
  ) where

import Relude hiding (empty, fromList, toList, null)
import Nix.AttrSet.Sig
import Nix.Types.VarName
import Nix.Types.SourcePos
import Nix.Types.Atom

-- | Holds file positioning information for abstractions.
-- A type alias for @AttrSet NSourcePos@.
type PositionSet = AttrSet NSourcePos

-- | Shared empty position set constant.
-- Use this instead of @mempty@ when creating @NVSet@ values to avoid
-- repeated allocation of empty AttrSets.
emptyPositionSet :: PositionSet
emptyPositionSet = empty
{-# NOINLINE emptyPositionSet #-}

-- | Parameter set stored as AttrSet for O(1) lookup during evaluation.
-- When ordered output is needed (XML, pretty-printing), convert to list
-- and sort lexicographically by parameter name.
type ParamSet r = AttrSet (Maybe r)

-- | Get parameters in lexicographically sorted order for deterministic output.
paramSetToSortedList :: ParamSet r -> [(VarName, Maybe r)]
paramSetToSortedList = sortOn fst . toList
