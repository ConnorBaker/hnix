{-# LANGUAGE NoStrict #-}

-- | Collection operations (list and attribute set) for the compiled Nix runtime.
--
-- These functions implement the semantics of list and attribute set operations
-- including concatenation, updates, and attribute access. They are called by
-- generated GHC Core code and throw NixError on type mismatches.
--
-- Design principles:
-- * Match Nix semantics exactly for all collection operations
-- * Throw NixError on type mismatches rather than returning Maybe
-- * INLINE aggressively - these are hot paths in generated code
-- * No thunk management - GHC handles that natively
module Nix.Compile.Primops.Collection
  ( -- * List operations
    nixListConcat
    -- * Attribute set operations
  , nixUpdate
  , nixSelect
  , nixSelectOr
  , nixSelectPath
  , nixHasAttr
  , nixHasAttrPath
  ) where

import Relude hiding (empty)
import qualified Data.List.NonEmpty as NE
import Nix.Types.VarName (VarName)
import Nix.Compile.Value
import Nix.Compile.Primops.Coerce (expectList, expectAttrs)

-- * List operations

-- | List concatenation (++).
--
-- Concatenates two lists by combining their elements.
-- Throws TypeError if either argument is not a list.
nixListConcat :: NixValue -> NixValue -> NixValue
nixListConcat v1 v2 = VList (expectList v1 <> expectList v2)
{-# INLINE nixListConcat #-}

-- * Attribute set operations

-- | Attribute set update (//). Right-biased merge.
--
-- Merges two attribute sets with the second argument taking precedence
-- on key conflicts. Throws TypeError if either argument is not a set.
nixUpdate :: NixValue -> NixValue -> NixValue
nixUpdate v1 v2 = VAttrs (unionAttrs (expectAttrs v1) (expectAttrs v2))
{-# INLINE nixUpdate #-}

-- | Select attribute from set. Throws on missing attribute.
--
-- Returns the value associated with the given attribute name.
-- Throws AttrMissing if the attribute does not exist.
nixSelect :: NixAttrs -> VarName -> NixValue
nixSelect attrs name =
  case lookupAttr name attrs of
    Just v -> v
    Nothing -> throwNixError $ AttrMissing name
{-# INLINE nixSelect #-}

-- | Select attribute from set with default.
--
-- Returns the value associated with the given attribute name,
-- or the default value if the attribute does not exist.
nixSelectOr :: NixAttrs -> VarName -> NixValue -> NixValue
nixSelectOr attrs name def =
  case lookupAttr name attrs of
    Just v -> v
    Nothing -> def
{-# INLINE nixSelectOr #-}

-- | Select along an attribute path (a.b.c).
--
-- Recursively selects through nested attribute sets following the given path.
-- Throws AttrMissing if any intermediate attribute does not exist.
-- Throws TypeError if an intermediate value is not an attribute set.
nixSelectPath :: NixAttrs -> NonEmpty VarName -> NixValue
nixSelectPath attrs (name NE.:| rest) =
  case rest of
    [] -> nixSelect attrs name
    (next : more) ->
      let v = nixSelect attrs name
      in nixSelectPath (expectAttrs v) (next NE.:| more)
{-# INLINE nixSelectPath #-}

-- | Check if attribute exists in set.
--
-- Returns a boolean NixValue indicating whether the attribute is present.
-- This is a safe check that never throws - it always returns true or false.
nixHasAttr :: NixAttrs -> VarName -> NixValue
nixHasAttr attrs name = VBool (isJust $ lookupAttr name attrs)
{-# INLINE nixHasAttr #-}

-- | Check if attribute path exists in set.
--
-- Recursively checks whether all attributes in the path exist in nested
-- attribute sets. Returns True if the full path exists, False if any
-- intermediate attribute is missing or not an attribute set.
-- This is a safe check that never throws.
nixHasAttrPath :: NixAttrs -> NonEmpty VarName -> Bool
nixHasAttrPath attrs (name NE.:| rest) =
  case lookupAttr name attrs of
    Nothing -> False
    Just v ->
      case rest of
        [] -> True
        (next : more) ->
          case v of
            VAttrs subAttrs -> nixHasAttrPath subAttrs (next NE.:| more)
            _ -> False
{-# INLINE nixHasAttrPath #-}
