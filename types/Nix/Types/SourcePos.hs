{-# LANGUAGE DeriveAnyClass #-}

-- | Source position types for tracking AST locations.
--
-- These types are optimized for performance with UNPACK pragmas since
-- source positions are created for every AST node during parsing.
module Nix.Types.SourcePos
  ( NPos(..)
  , NSourcePos(..)
  , toNSourcePos
  , toSourcePos
  , nullPos
  -- Re-exports from megaparsec
  , SourcePos(..)
  , unPos
  , mkPos
  ) where

import           Relude
import           Nix.Types.Path                 (Path(..))
import           Codec.Serialise                (Serialise)
import qualified Codec.Serialise               as Serialise
import qualified Data.Binary                   as Binary
import           Data.Binary                    (Binary)
import           Data.Data                      (Data)
import           Data.Aeson                     (ToJSON, FromJSON)
import qualified Data.Aeson                    as Aeson
import           Text.Megaparsec.Pos            ( Pos
                                                , mkPos
                                                , unPos
                                                , SourcePos(SourcePos)
                                                )

-- | NPos wraps megaparsec's Pos (which is a Word internally).
-- UNPACK eliminates the Pos wrapper indirection during parsing
-- where source positions are created intensively.
newtype NPos = NPos Pos
 deriving stock
   ( Eq, Ord
   , Read, Show
   , Data
   , Generic
   )
 deriving newtype NFData

instance Semigroup NPos where
  (NPos x) <> (NPos y) = NPos (x <> y)

-- | Represents source positions.
-- Source line & column positions change intensively during parsing,
-- so they are declared strict to avoid memory leaks.
--
-- Performance note: UNPACK on NPos fields eliminates boxing overhead.
-- Source positions are created for every AST node during parsing,
-- so this optimization has high impact.
--
-- The data type is a reimplementation of 'Text.Megaparsec.Pos' 'SourcePos'.
data NSourcePos =
  NSourcePos
  { -- | Name of source file
    getSourceName :: !Path,
    -- | Line number
    getSourceLine :: {-# UNPACK #-} !NPos,
    -- | Column number
    getSourceColumn :: {-# UNPACK #-} !NPos
  }
 deriving
   ( Eq, Ord
   , Read, Show
   , Data, NFData
   , Generic
   )

-- | Helper for 'SourcePos' -> 'NSourcePos' coersion.
toNSourcePos :: SourcePos -> NSourcePos
toNSourcePos (SourcePos f l c) =
  NSourcePos (coerce f) (coerce l) (coerce c)

-- | Helper for 'NSourcePos' -> 'SourcePos' coersion.
toSourcePos :: NSourcePos -> SourcePos
toSourcePos (NSourcePos f l c) =
  SourcePos (coerce f) (coerce l) (coerce c)

-- | A null/placeholder source position.
nullPos :: NSourcePos
nullPos = on (NSourcePos "<string>") (coerce . mkPos) 1 1

-- ** Additional N{,Source}Pos instances

instance Serialise NPos where
  encode = Serialise.encode . unPos . coerce
  decode = coerce . mkPos <$> Serialise.decode

instance Serialise NSourcePos where
  encode (NSourcePos f l c) =
    coerce $
    Serialise.encode f <>
    Serialise.encode l <>
    Serialise.encode c
  decode =
    liftA3 NSourcePos
      Serialise.decode
      Serialise.decode
      Serialise.decode

instance Hashable NPos where
  hashWithSalt salt = hashWithSalt salt . unPos . coerce

instance Hashable NSourcePos where
  hashWithSalt salt (NSourcePos f l c) =
    salt
      `hashWithSalt` f
      `hashWithSalt` l
      `hashWithSalt` c

instance Binary NPos where
  put = (Binary.put @Int) . unPos . coerce
  get = coerce . mkPos <$> Binary.get
instance Binary NSourcePos

instance ToJSON NPos where
  toJSON = Aeson.toJSON . unPos . coerce
instance ToJSON NSourcePos

instance FromJSON NPos where
  parseJSON = coerce . fmap mkPos . Aeson.parseJSON
instance FromJSON NSourcePos
