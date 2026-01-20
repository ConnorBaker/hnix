-- | Nix string type with context tracking.
--
-- This module re-exports the Nix string types from the Backpack implementation.
-- Nix strings carry "context" - information about which store paths or
-- derivations they reference. This context propagates through string operations
-- to ensure proper dependency tracking.
--
-- The concrete implementation uses 'Data.Text.Text' for string content and
-- 'Data.HashSet.HashSet' for context tracking (from the hnix-string sublibrary).
module Nix.String
  ( -- * Core types (from Nix.String.Text)
    NixString
  , getStringContext
  , mkNixString
  , StringContext
  , ContextFlavor(DirectPath, AllOutputs, DerivationOutput)
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
    -- * NixLikeContext (from Nix.String.Text.NixLike)
  , NixLikeContext(..)
  , NixLikeContextValue(..)
  , toNixLikeContext
  , fromNixLikeContext
    -- * String context operations
  , hasContext
  , intercalateNixString
  , getStringNoContext
  , ignoreContext
  , mkNixStringWithoutContext
  , mkNixStringWithSingletonContext
  , mkNixStrDirectPath
  , mkNixStrAllOutputs
  , modifyNixContents
    -- * Context accumulator monad (from Nix.String.Text.Context)
  , WithStringContext
  , WithStringContextT(..)
  , extractNixString
  , addStringContext
  , addSingletonStringContext
  , runWithStringContextT
  , runWithStringContextT'
  , runWithStringContext
  , runWithStringContext'
    -- * Constants (for zero-allocation optimization)
  , emptyStringContext
  , nixStringEmpty
  , nixStringOne
  )
where

-- Core types from the implementation
import           Nix.String.Text
                    ( NixString
                    , StringContext
                    , ContextFlavor(DirectPath, AllOutputs, DerivationOutput)
                    , mkDirectPath
                    , mkAllOutputs
                    , mkDerivationOutput
                    , isDirectPath
                    , isAllOutputs
                    , isDerivationOutput
                    , getDerivationOutputName
                    , getStringContextFlavor
                    , getStringContextPath
                    , mkStringContext
                    , getStringContext
                    , mkNixStringWithoutContext
                    , mkNixString
                    , mkNixStringWithSingletonContext
                    , mkNixStrDirectPath
                    , mkNixStrAllOutputs
                    , hasContext
                    , getStringNoContext
                    , ignoreContext
                    , modifyNixContents
                    , intercalateNixString
                    , emptyStringContext
                    , nixStringEmpty
                    , nixStringOne
                    )

-- Context accumulator monad
import           Nix.String.Text.Context
                    ( WithStringContext
                    , WithStringContextT(..)
                    , extractNixString
                    , addStringContext
                    , addSingletonStringContext
                    , runWithStringContextT
                    , runWithStringContextT'
                    , runWithStringContext
                    , runWithStringContext'
                    )

-- NixLikeContext for Nix-compatible context handling
import           Nix.String.Text.NixLike
                    ( NixLikeContext(..)
                    , NixLikeContextValue(..)
                    , toNixLikeContext
                    , fromNixLikeContext
                    )
