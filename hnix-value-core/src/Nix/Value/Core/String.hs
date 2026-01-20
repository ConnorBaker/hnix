-- | Re-exports of Nix string types from the Backpack signature.
--
-- This module re-exports the core NixString types from Nix.String.Sig.
-- The concrete implementation is selected at link time via Backpack mixins.
--
-- NOTE: NixLikeContext, WithStringContextT, and related conversion functions
-- are NOT exported from this module. They should be imported directly from
-- the implementation package (e.g., Nix.String.Text.Context and
-- Nix.String.Text.NixLike) as they depend on AttrSet which creates
-- additional signature dependencies.
module Nix.Value.Core.String
  ( -- * Types
    NixString
  , StringContext
  , ContextFlavor
    -- ** ContextFlavor constructors and predicates
  , mkDirectPath
  , mkAllOutputs
  , mkDerivationOutput
  , isDirectPath
  , isAllOutputs
  , isDerivationOutput
  , getDerivationOutputName
    -- ** Accessors
  , getStringContextFlavor
  , getStringContextPath
  , mkStringContext
  , getStringContext
    -- * Construction
  , mkNixStringWithoutContext
  , mkNixString
  , mkNixStringWithSingletonContext
  , mkNixStrDirectPath
  , mkNixStrAllOutputs
    -- * Extraction
  , ignoreContext
  , getStringNoContext
  , hasContext
    -- * Modification
  , modifyNixContents
  , intercalateNixString
    -- * Constants
  , emptyStringContext
  , nixStringEmpty
  , nixStringOne
  )
where

-- Re-export everything from the signature
import Nix.String.Sig
