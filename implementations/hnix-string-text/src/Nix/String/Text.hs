{-# LANGUAGE DeriveGeneric #-}

-- | Text-backed implementation of NixString with context tracking.
--
-- This module provides the concrete implementation of Nix strings using
-- 'Data.Text.Text' for the string content and 'Data.HashSet.HashSet' for
-- the string context.
module Nix.String.Text
  ( -- * Types
    NixString
  , StringContext
  , ContextFlavor(DirectPath, AllOutputs, DerivationOutput)
    -- ** Accessors
  , getStringContextFlavor
  , getStringContextPath
  , mkStringContext
  , getStringContext
  , getStringContent
    -- ** ContextFlavor constructors and predicates
  , mkDirectPath
  , mkAllOutputs
  , mkDerivationOutput
  , isDirectPath
  , isAllOutputs
  , isDerivationOutput
  , getDerivationOutputName
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

import           Relude
import qualified Data.HashSet                  as HS
import qualified Data.Text                     as Text
import           Nix.Types.VarName              ( VarName, varNameText )


-- * Context types

-- | A Nix 'StringContext' tracks where string values come from.
-- Internal representation - constructors not exported.
data StringContext = StringContextInternal !ContextFlavor !VarName
  deriving (Eq, Ord, Show, Generic)

instance Hashable StringContext
instance NFData StringContext

-- | Get the flavor of the context
getStringContextFlavor :: StringContext -> ContextFlavor
getStringContextFlavor (StringContextInternal f _) = f
{-# INLINE getStringContextFlavor #-}

-- | Get the path of the context
getStringContextPath :: StringContext -> VarName
getStringContextPath (StringContextInternal _ p) = p
{-# INLINE getStringContextPath #-}

-- | Constructor for StringContext
mkStringContext :: ContextFlavor -> VarName -> StringContext
mkStringContext = StringContextInternal
{-# INLINE mkStringContext #-}

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts.
data ContextFlavor
  = DirectPath
  | AllOutputs
  | DerivationOutput Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable ContextFlavor
instance NFData ContextFlavor

-- Smart constructors for ContextFlavor (to satisfy signature)

-- | Smart constructor for direct path context
mkDirectPath :: ContextFlavor
mkDirectPath = DirectPath
{-# INLINE mkDirectPath #-}

-- | Smart constructor for all outputs context
mkAllOutputs :: ContextFlavor
mkAllOutputs = AllOutputs
{-# INLINE mkAllOutputs #-}

-- | Smart constructor for derivation output context
mkDerivationOutput :: Text -> ContextFlavor
mkDerivationOutput = DerivationOutput
{-# INLINE mkDerivationOutput #-}

-- Predicates for ContextFlavor

-- | Check if a ContextFlavor is DirectPath
isDirectPath :: ContextFlavor -> Bool
isDirectPath DirectPath = True
isDirectPath _ = False
{-# INLINE isDirectPath #-}

-- | Check if a ContextFlavor is AllOutputs
isAllOutputs :: ContextFlavor -> Bool
isAllOutputs AllOutputs = True
isAllOutputs _ = False
{-# INLINE isAllOutputs #-}

-- | Check if a ContextFlavor is DerivationOutput
isDerivationOutput :: ContextFlavor -> Bool
isDerivationOutput (DerivationOutput _) = True
isDerivationOutput _ = False
{-# INLINE isDerivationOutput #-}

-- | Extract the output name from a DerivationOutput flavor.
-- Returns Nothing for other flavors.
getDerivationOutputName :: ContextFlavor -> Maybe Text
getDerivationOutputName (DerivationOutput t) = Just t
getDerivationOutputName _ = Nothing
{-# INLINE getDerivationOutputName #-}


-- * NixString type

-- | Nix string with context tracking.
-- The context tracks which store paths/derivations this string references.
-- Internal representation - constructors not exported.
data NixString = NixStringInternal !(HS.HashSet StringContext) !Text
  deriving (Eq, Ord, Show, Generic)

-- | Get the context of a NixString
getStringContext :: NixString -> HS.HashSet StringContext
getStringContext (NixStringInternal ctx _) = ctx
{-# INLINE getStringContext #-}

-- | Get the content of a NixString
getStringContent :: NixString -> Text
getStringContent (NixStringInternal _ content) = content
{-# INLINE getStringContent #-}

instance Semigroup NixString where
  NixStringInternal s1 t1 <> NixStringInternal s2 t2 = NixStringInternal (s1 <> s2) (t1 <> t2)

instance Monoid NixString where
  mempty = NixStringInternal mempty mempty

instance Hashable NixString
instance NFData NixString


-- * Constants

-- | Shared empty string context to avoid repeated allocations.
-- This is used by 'mkNixStringWithoutContext' and related functions.
emptyStringContext :: HS.HashSet StringContext
emptyStringContext = mempty
{-# NOINLINE emptyStringContext #-}

-- | Empty NixString constant (empty text, no context).
-- Use this instead of @mkNixStringWithoutContext ""@ in hot paths.
nixStringEmpty :: NixString
nixStringEmpty = NixStringInternal emptyStringContext ""
{-# NOINLINE nixStringEmpty #-}

-- | NixString "1" constant (no context).
-- Used for boolean-to-string coercion of True.
nixStringOne :: NixString
nixStringOne = NixStringInternal emptyStringContext "1"
{-# NOINLINE nixStringOne #-}


-- * Construction

-- | Constructs NixString without a context
mkNixStringWithoutContext :: Text -> NixString
mkNixStringWithoutContext = NixStringInternal emptyStringContext

-- | Create NixString using a singleton context
mkNixStringWithSingletonContext :: StringContext -> VarName -> NixString
mkNixStringWithSingletonContext c s = NixStringInternal (one c) (varNameText s)

-- | Create NixString with DirectPath context.
-- This is the most common context type (for store paths).
mkNixStrDirectPath :: VarName -> NixString
mkNixStrDirectPath path = NixStringInternal (one $ StringContextInternal DirectPath path) (varNameText path)
{-# INLINE mkNixStrDirectPath #-}

-- | Create NixString with AllOutputs context.
-- Used for derivation paths that reference all outputs.
mkNixStrAllOutputs :: VarName -> NixString
mkNixStrAllOutputs path = NixStringInternal (one $ StringContextInternal AllOutputs path) (varNameText path)
{-# INLINE mkNixStrAllOutputs #-}

-- | Create NixString from a Text and context
mkNixString :: HS.HashSet StringContext -> Text -> NixString
mkNixString = NixStringInternal


-- * Extraction

-- | Returns True if the NixString has an associated context
hasContext :: NixString -> Bool
hasContext (NixStringInternal c _) = not (null c)

-- | Extract the string contents from a NixString that has no context
getStringNoContext :: NixString -> Maybe Text
getStringNoContext (NixStringInternal c s)
  | null c    = pure s
  | otherwise = mempty

-- | Extract the string contents from a NixString even if the NixString has an associated context
ignoreContext :: NixString -> Text
ignoreContext (NixStringInternal _ s) = s


-- * Modification

-- | Modify the string part of the NixString, leaving the context unchanged
modifyNixContents :: (Text -> Text) -> NixString -> NixString
modifyNixContents f (NixStringInternal c s) = NixStringInternal c (f s)

-- | Combine NixStrings with a separator
intercalateNixString :: NixString -> [NixString] -> NixString
intercalateNixString _   []   = nixStringEmpty
intercalateNixString _   [ns] = ns
intercalateNixString sep nss  =
  NixStringInternal combinedContext combinedText
 where
  -- Use foldl' instead of HS.unions to avoid intermediate list allocation
  combinedContext = foldl' HS.union (getStringContext sep) (getStringContext <$> nss)
  combinedText = Text.intercalate (getStringContent sep) (getStringContent <$> nss)
