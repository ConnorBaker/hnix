-- | Consolidated re-exports of all Backpack signature types.
--
-- This module provides a single import point for all abstract types used in
-- HNix's Backpack architecture. Instead of importing from multiple signature
-- modules, client code can import everything from here:
--
-- @
-- import Nix.Core.Types (AttrSet, NixList, NixString, StringContext, ContextFlavor)
-- @
--
-- All types are re-exported from their respective signature modules:
--
--   * 'AttrSet' - from "Nix.AttrSet.Sig"
--   * 'NixList' - from "Nix.List.Sig"
--   * 'NixString', 'StringContext', 'ContextFlavor' - from "Nix.String.Sig"
--
-- For operations on these types, see "Nix.Core.Value.Protocol" which provides
-- a unified API with proper INLINE pragmas for guaranteed monomorphization.
module Nix.Core.Types
  ( -- * AttrSet type (from Nix.AttrSet.Sig)
    AttrSet
    -- * List type (from Nix.List.Sig)
  , NixList
    -- * String types (from Nix.String.Sig)
  , NixString
  , StringContext
  , ContextFlavor
    -- * Re-exports from Nix.Types for convenience
  , VarName
  , NAtom(..)
  , Path
  , NSourcePos
  ) where

import           Nix.AttrSet.Sig                ( AttrSet )
import           Nix.List.Sig                   ( NixList )
import           Nix.String.Sig                 ( NixString, StringContext, ContextFlavor )
import           Nix.Types.VarName              ( VarName )
import           Nix.Types.Atom                 ( NAtom(..) )
import           Nix.Types.Path                 ( Path )
import           Nix.Types.SourcePos            ( NSourcePos )
