{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Path type for filesystem paths with type safety.
--
-- This module provides an explicit type boundary between FilePath and String,
-- making path handling more type-safe throughout the codebase.
module Nix.Types.Path
  ( Path(..)
  , isAbsolute
  , (</>)
  , joinPath
  , splitDirectories
  , takeDirectory
  , takeFileName
  , takeBaseName
  , takeExtension
  , takeExtensions
  , addExtension
  , dropExtensions
  , replaceExtension
  , readFile
  ) where

import           Relude                  hiding (readFile)
import           Data.Binary             (Binary)
import           Data.Data               (Data)
import           Codec.Serialise         (Serialise)
import qualified Data.Aeson              as A
import qualified System.FilePath         as FilePath

-- | Explicit type boundary between FilePath & String.
newtype Path = Path FilePath
  deriving stock (Generic, Data)
  deriving newtype
    ( Eq, Ord
    , NFData, Serialise, Binary, A.ToJSON, A.FromJSON
    , Show, Read, Hashable
    , Semigroup, Monoid
    )

instance ToText Path where
  toText = toText @String . coerce

instance IsString Path where
  fromString = coerce

-- ** Path functions

-- | 'Path's 'FilePath.isAbsolute'.
isAbsolute :: Path -> Bool
isAbsolute = coerce FilePath.isAbsolute

-- | 'Path's 'FilePath.(</>)'.
(</>) :: Path -> Path -> Path
(</>) = coerce (FilePath.</>)
infixr 5 </>

-- | 'Path's 'FilePath.joinPath'.
joinPath :: [Path] -> Path
joinPath = coerce FilePath.joinPath

-- | 'Path's 'FilePath.splitDirectories'.
splitDirectories :: Path -> [Path]
splitDirectories = coerce FilePath.splitDirectories

-- | 'Path's 'FilePath.takeDirectory'.
takeDirectory :: Path -> Path
takeDirectory = coerce FilePath.takeDirectory

-- | 'Path's 'FilePath.takeFileName'.
takeFileName :: Path -> Path
takeFileName = coerce FilePath.takeFileName

-- | 'Path's 'FilePath.takeBaseName'.
takeBaseName :: Path -> String
takeBaseName = coerce FilePath.takeBaseName

-- | 'Path's 'FilePath.takeExtension'.
takeExtension :: Path -> String
takeExtension = coerce FilePath.takeExtensions

-- | 'Path's 'FilePath.takeExtensions'.
takeExtensions :: Path -> String
takeExtensions = coerce FilePath.takeExtensions

-- | 'Path's 'FilePath.addExtensions'.
addExtension :: Path -> String -> Path
addExtension = coerce FilePath.addExtension

-- | 'Path's 'FilePath.dropExtensions'.
dropExtensions :: Path -> Path
dropExtensions = coerce FilePath.dropExtensions

-- | 'Path's 'FilePath.replaceExtension'.
replaceExtension :: Path -> String -> Path
replaceExtension = coerce FilePath.replaceExtension

-- | 'Path's 'FilePath.readFile'.
readFile :: MonadIO m => Path -> m Text
readFile = fmap decodeUtf8 . readFileBS . coerce
