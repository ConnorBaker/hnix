{-# LANGUAGE DeriveGeneric #-}

-- | Nix-like context representation.
--
-- This module provides the 'NixLikeContext' type which represents string
-- context in the format used by Nix's @builtins.getContext@ and
-- @builtins.appendContext@.
--
-- NOTE: This module depends on AttrSet.Sig because NixLikeContext uses
-- AttrSet to store context values keyed by path.
module Nix.String.Text.NixLike
  ( NixLikeContext(..)
  , NixLikeContextValue(..)
  , toNixLikeContext
  , fromNixLikeContext
  , toNixLikeContextValue
  , toStringContexts
  )
where

import           Relude
import qualified Data.HashSet                  as HS
import           Nix.Types.VarName              ( VarName )
import           Nix.AttrSet.Sig                ( AttrSet )
import qualified Nix.AttrSet.Sig               as A
import           Nix.String.Text                ( StringContext
                                                , ContextFlavor
                                                , mkDirectPath
                                                , mkAllOutputs
                                                , mkDerivationOutput
                                                , isDirectPath
                                                , isAllOutputs
                                                , getDerivationOutputName
                                                , getStringContextFlavor
                                                , getStringContextPath
                                                , mkStringContext
                                                )


-- | Nix-like context representation.
-- This matches the format used by @builtins.getContext@.
newtype NixLikeContext =
  NixLikeContext
    { getNixLikeContext :: AttrSet NixLikeContextValue
    }
  deriving (Eq, Ord, Show, Generic)

-- | Value in a NixLikeContext entry.
data NixLikeContextValue =
  NixLikeContextValue
    { nlcvPath :: Bool
    , nlcvAllOutputs :: Bool
    , nlcvOutputs :: [Text]
    }
  deriving (Show, Eq, Ord, Generic)

instance Semigroup NixLikeContextValue where
  a <> b =
    NixLikeContextValue
      { nlcvPath       = nlcvPath       a || nlcvPath       b
      , nlcvAllOutputs = nlcvAllOutputs a || nlcvAllOutputs b
      , nlcvOutputs    = nlcvOutputs    a <> nlcvOutputs    b
      }

instance Monoid NixLikeContextValue where
  mempty = NixLikeContextValue False False mempty


-- | Convert from NixLikeContext to a HashSet of StringContext.
fromNixLikeContext :: NixLikeContext -> HS.HashSet StringContext
fromNixLikeContext =
  HS.fromList . (uncurry toStringContexts <=< A.toList . getNixLikeContext)

-- | Convert a path and NixLikeContextValue to a list of StringContext.
-- This is used internally by fromNixLikeContext.
toStringContexts :: VarName -> NixLikeContextValue -> [StringContext]
toStringContexts path = go
 where
  go :: NixLikeContextValue -> [StringContext]
  go cv =
    case cv of
      NixLikeContextValue True _    _ ->
        mkLstCtxFor mkDirectPath cv { nlcvPath = False }
      NixLikeContextValue _    True _ ->
        mkLstCtxFor mkAllOutputs cv { nlcvAllOutputs = False }
      NixLikeContextValue _    _    ls | not (null ls) ->
        mkCtxFor . mkDerivationOutput <$> ls
      _ -> mempty
   where
    mkCtxFor :: ContextFlavor -> StringContext
    mkCtxFor context = mkStringContext context path
    mkLstCtxFor :: ContextFlavor -> NixLikeContextValue -> [StringContext]
    mkLstCtxFor t c = one (mkCtxFor t) <> go c

-- | Convert a StringContext to a (NixLikeContextValue, VarName) pair.
toNixLikeContextValue :: StringContext -> (NixLikeContextValue, VarName)
toNixLikeContextValue sc =
  ( flavorToValue (getStringContextFlavor sc)
  , getStringContextPath sc
  )
 where
  flavorToValue :: ContextFlavor -> NixLikeContextValue
  flavorToValue flavor
    | isDirectPath flavor = NixLikeContextValue True False mempty
    | isAllOutputs flavor = NixLikeContextValue False True mempty
    | otherwise = case getDerivationOutputName flavor of
        Just t  -> NixLikeContextValue False False $ one t
        Nothing -> NixLikeContextValue False False mempty  -- Should not happen

-- | Convert a HashSet of StringContext to NixLikeContext.
toNixLikeContext :: HS.HashSet StringContext -> NixLikeContext
toNixLikeContext stringContext =
  NixLikeContext $
    HS.foldr
      fun
      mempty
      stringContext
 where
  fun :: StringContext -> AttrSet NixLikeContextValue -> AttrSet NixLikeContextValue
  fun sc =
    uncurry (A.insertWith (<>)) (swap $ toNixLikeContextValue sc)
