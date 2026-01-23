-- | Simplified cited module with provenance removed.
--
-- This module previously provided the 'Cited' type for wrapping values with
-- optional provenance tracking. Since provenance has been removed, the 'Cited'
-- type is now just an 'Identity' wrapper.
module Nix.Cited.Basic
  ( handleDisplayProvenance
  ) where

import           Nix.Prelude
import           Control.Monad.Catch     hiding ( catchJust )
import           Nix.Frames
import           Nix.Thunk


-- | Handle display of provenance information during thunk forcing.
--
-- Since provenance is removed, this just catches ThunkLoop exceptions
-- and re-throws them through the proper error channel.
handleDisplayProvenance
  :: (MonadCatch m
    , Typeable m
    , Has e Frames
    , MonadReader e m
    )
  => m a
  -> m a
handleDisplayProvenance f =
  catch f (throwError @ThunkLoop)
