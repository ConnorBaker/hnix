-- | String context accumulator monad.
--
-- This module provides a monad transformer for accumulating string context
-- while producing a result string. This is useful for string operations that
-- need to track context from multiple source strings.
module Nix.String.Text.Context
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
where

import           Relude
import           Control.Monad.Writer           ( WriterT(..), MonadWriter(tell))
import qualified Data.HashSet                  as HS
import           Nix.String.Text                ( NixString
                                                , StringContext
                                                , getStringContext
                                                , getStringContent
                                                , mkNixString
                                                )


-- | A monad for accumulating string context while producing a result string.
newtype WithStringContextT m a =
  WithStringContextT
    (WriterT (HS.HashSet StringContext) m a )
  deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter (HS.HashSet StringContext))

type WithStringContext = WithStringContextT Identity


-- | Get the contents of a 'NixString' and write its context into the resulting set.
extractNixString :: Monad m => NixString -> WithStringContextT m Text
extractNixString ns =
  WithStringContextT $
    getStringContent ns <$ tell (getStringContext ns)

-- | Add 'StringContext's into the resulting set.
addStringContext
  :: Monad m => HS.HashSet StringContext -> WithStringContextT m ()
addStringContext = WithStringContextT . tell

-- | Add a 'StringContext' into the resulting set.
addSingletonStringContext :: Monad m => StringContext -> WithStringContextT m ()
addSingletonStringContext = WithStringContextT . tell . one

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContextT :: Monad m => WithStringContextT m Text -> m NixString
runWithStringContextT (WithStringContextT m) =
  uncurry (flip mkNixString) <$> runWriterT m

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContext :: WithStringContextT Identity Text -> NixString
runWithStringContext = runIdentity . runWithStringContextT

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContextT' :: Monad m => WithStringContextT m a -> m (a, HS.HashSet StringContext)
runWithStringContextT' (WithStringContextT m) = runWriterT m

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContext' :: WithStringContextT Identity a -> (a, HS.HashSet StringContext)
runWithStringContext' = runIdentity . runWithStringContextT'
