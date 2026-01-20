{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    -- * Parameters
  , Params(..)
  , Variadic(..)
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


-- * Variadic

-- | Indicates whether a parameter set is variadic (allows extra arguments).
data Variadic = Closed | Variadic
  deriving (Eq, Ord, Generic, Show, Read, NFData, Hashable)

instance Semigroup Variadic where
  (<>) Closed Closed = Closed
  (<>) _      _      = Variadic

instance Monoid Variadic where
  mempty = Closed


-- * Params

-- | @Params@ represents all the ways the formal parameters to a
-- function can be represented.
data Params r
  = Param VarName
  -- ^ For functions with a single named argument, such as @x: x + 1@.
  --
  -- > Param "x"                                  ~  x
  | ParamSet (Maybe VarName) Variadic (ParamSet r)
  -- ^ Explicit parameters (argument must be a set). Might specify a name to
  -- bind to the set in the function body. The bool indicates whether it is
  -- variadic or not.
  --
  -- > ParamSet  Nothing   False [("x",Nothing)]  ~  { x }
  -- > ParamSet (pure "s") True  [("x", pure y)]  ~  s@{ x ? y, ... }
  deriving (Eq, Ord, Generic, Functor, Foldable, Traversable, Show, Read)

instance NFData r => NFData (Params r)
instance Hashable r => Hashable (Params r)

instance IsString (Params r) where
  fromString = Param . fromString
