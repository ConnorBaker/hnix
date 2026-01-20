-- | Utility functions and typeclasses for hnix-core.
module Nix.Core.Utils
  ( Has(..)
  , askLocal
  , whenTrue
  ) where

import Relude
import Lens.Family2

-- | Typeclass for accessing components of a larger structure.
class Has a b where
  hasLens :: Lens' a b

instance Has a a where
  hasLens f = f

instance Has (a, b) a where
  hasLens f (a, b) = (, b) <$> f a

instance Has (a, b) b where
  hasLens f (a, b) = (a, ) <$> f b

-- | Retrieve monad state by 'Lens''.
askLocal :: (MonadReader t m, Has t a) => m a
askLocal = asks $ view hasLens

-- | Returns the first argument if the Bool is True, otherwise mempty.
-- Note: Uses lazy pattern on first argument to preserve short-circuit
-- behavior under the Strict extension.
whenTrue :: Monoid a => a -> Bool -> a
whenTrue ~x b =
  if b
    then x
    else mempty
{-# INLINABLE whenTrue #-}
