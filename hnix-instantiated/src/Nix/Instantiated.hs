{-# LANGUAGE TypeApplications #-}

-- | Re-exports from instantiated hnix-core.
--
-- This module demonstrates that Backpack instantiation works correctly.
-- The AttrSet and NixList types are now concrete (HashMap and Vector).
module Nix.Instantiated
  ( -- * AttrSet operations (backed by HashMap)
    module Nix.Core.AttrSet
    -- * NixList operations (backed by Vector)
  , module Nix.Core.List
    -- * Expression types
  , module Nix.Core.Expr.Types
    -- * Demo functions
  , demoAttrSet
  , demoList
  , demoPositionSet
  ) where

import Relude hiding (empty, fromList, toList, null)
import Nix.Core.AttrSet
import Nix.Core.List
import Nix.Core.Expr.Types

-- | Demo function showing AttrSet operations work.
demoAttrSet :: AttrSet Int
demoAttrSet = fromList
  [ (mkVarName "x", 1)
  , (mkVarName "y", 2)
  , (mkVarName "z", 3)
  ]

-- | Demo function showing NixList operations work.
demoList :: NixList Int
demoList = nlFromList [1, 2, 3, 4, 5]

-- | Demo function showing PositionSet works.
demoPositionSet :: PositionSet
demoPositionSet = emptyPositionSet
