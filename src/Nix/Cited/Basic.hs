{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Nix.Cited.Basic
  ( Cited(..)
  , CitedRep
  , pattern CitedP
  , cite
  , citeLite
  , extractCited
  , provenanceCited
  , handleDisplayProvenance
  , displayProvenance
  ) where

import           Nix.Prelude
import           Control.Comonad                ( Comonad(..) )
import           Control.Comonad.Env            ( ComonadEnv(..) )
import           Control.Monad.Catch     hiding ( catchJust )
import           Data.Functor.Identity          ()
import           Nix.Cited
import           Nix.Config.Singleton           ( SBoolI, sbool, SBool(..) )
import           Nix.Eval                      as Eval
                                                ( EvalFrame(EvaluatingExpr,ForcingExpr) )
import           Nix.Exec
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Options
import           Nix.Thunk
import           Nix.Value


-- * Type family for provenance representation
--
-- When @prov ~ 'True@: stores provenance list + value (NCited)
-- When @prov ~ 'False@: stores just the value (Identity) - zero overhead

type family CitedRep (prov :: Bool) m v a where
  CitedRep 'True  m v a = NCited m v a
  CitedRep 'False m v a = Identity a


-- * Unified Cited type parameterized by provenance flag
--
-- The @prov@ parameter selects the internal representation:
-- - @'True@: full provenance tracking via 'NCited'
-- - @'False@: zero overhead via 'Identity'

newtype Cited (prov :: Bool) t f m a =
  Cited { runCited :: CitedRep prov m (NValue t f m) a }

-- Can't derive Generic through type family, but we don't need it for these instances

-- | Extract the value from a Cited wrapper.
--
-- Function SPECIALIZE pragmas are safe here because they only affect this
-- standalone function, eliminating sbool dispatch overhead without changing
-- evaluation semantics.
extractCited :: forall prov t f m a. SBoolI prov => Cited prov t f m a -> a
extractCited (Cited rep) = case sbool @prov of
  STrue  -> getCited rep
  SFalse -> runIdentity rep
{-# INLINABLE extractCited #-}
{-# SPECIALIZE extractCited :: Cited 'True t f m a -> a #-}
{-# SPECIALIZE extractCited :: Cited 'False t f m a -> a #-}

-- | Get provenance from a Cited wrapper (empty list when prov ~ 'False).
--
-- Function SPECIALIZE pragmas are safe here because they only affect this
-- standalone function, eliminating sbool dispatch overhead without changing
-- evaluation semantics.
provenanceCited :: forall prov t f m a. SBoolI prov => Cited prov t f m a -> [Provenance m (NValue t f m)]
provenanceCited (Cited rep) = case sbool @prov of
  STrue  -> getProvenance rep
  SFalse -> []
{-# INLINABLE provenanceCited #-}
{-# SPECIALIZE provenanceCited :: Cited 'True t f m a -> [Provenance m (NValue t f m)] #-}
{-# SPECIALIZE provenanceCited :: Cited 'False t f m a -> [Provenance m (NValue t f m)] #-}

instance SBoolI prov => Functor (Cited prov t f m) where
  fmap f (Cited rep) = Cited $ case sbool @prov of
    STrue  -> fmap f rep
    SFalse -> fmap f rep
  {-# INLINABLE fmap #-}

instance SBoolI prov => Applicative (Cited prov t f m) where
  pure a = Cited $ case sbool @prov of
    STrue  -> pure a
    SFalse -> pure a
  {-# INLINABLE pure #-}
  Cited f <*> Cited a = Cited $ case sbool @prov of
    STrue  -> f <*> a
    SFalse -> f <*> a
  {-# INLINABLE (<*>) #-}

instance SBoolI prov => Foldable (Cited prov t f m) where
  foldMap f (Cited rep) = case sbool @prov of
    STrue  -> foldMap f rep
    SFalse -> foldMap f rep
  {-# INLINABLE foldMap #-}

instance SBoolI prov => Traversable (Cited prov t f m) where
  traverse f (Cited rep) = case sbool @prov of
    STrue  -> Cited <$> traverse f rep
    SFalse -> Cited <$> traverse f rep
  {-# INLINABLE traverse #-}

instance SBoolI prov => Comonad (Cited prov t f m) where
  extract = extractCited
  {-# INLINABLE extract #-}
  duplicate c@(Cited rep) = Cited $ case sbool @prov of
    STrue  -> NCited (getProvenance rep) c
    SFalse -> Identity c
  {-# INLINABLE duplicate #-}

instance SBoolI prov => ComonadEnv [Provenance m (NValue t f m)] (Cited prov t f m) where
  ask = provenanceCited
  {-# INLINABLE ask #-}


-- ** Helpers

-- | @Cited@ pattern for provenance-enabled case.
-- > pattern CitedP m a = Cited (NCited m a)
--
-- This pattern only works when @prov ~ 'True@.
pattern CitedP
  :: [Provenance m (NValue t f m)]
  -> a
  -> Cited 'True t f m a
pattern CitedP m a = Cited (NCited m a)
{-# complete CitedP #-}

-- | Create a Cited value with provenance (for @prov ~ 'True@).
cite
  :: Functor m
  => [Provenance m (NValue t f m)]
  -> m a
  -> m (Cited 'True t f m a)
cite v = fmap (Cited . NCited v)

-- | Create a Cited value without provenance (for @prov ~ 'False@).
citeLite
  :: Functor m
  => m a
  -> m (Cited 'False t f m a)
citeLite = fmap (Cited . Identity)


-- ** Instances

-- | HasCitations1 instance - dispatches based on prov.
--
-- __WARNING__: Do NOT add @SPECIALIZE instance@ pragmas here.
--
-- Testing revealed that @SPECIALIZE instance@ pragmas on this instance cause
-- evaluation failures when evaluating nixpkgs (e.g., @release.nix@). The likely
-- cause is interaction between instance specialization and the @Strict@ language
-- extension: specialized instances may evaluate arguments more eagerly, breaking
-- Nix's lazy evaluation semantics and causing paths to be accessed that would
-- otherwise remain unevaluated.
--
-- The @INLINABLE@ pragmas on methods allow GHC to specialize opportunistically
-- without forcing the strictness changes that break evaluation. Function-level
-- @SPECIALIZE@ pragmas (like those on 'extractCited' and 'provenanceCited') are
-- safe because they only affect standalone functions, not instance resolution.
instance SBoolI prov => HasCitations1 m (NValue t f m) (Cited prov t f m) where
  citations1 = provenanceCited
  {-# INLINABLE citations1 #-}
  addProvenance1 p (Cited rep) = Cited $ case sbool @prov of
    STrue  -> addProvenance p rep
    SFalse -> rep  -- No-op for Identity (no provenance tracking)
  {-# INLINABLE addProvenance1 #-}

-- | Unified MonadThunk instance for Cited (any @prov@).
--
-- Uses singleton dispatch to select the appropriate implementation:
-- - When @prov ~ 'True@: tracks provenance information
-- - When @prov ~ 'False@: zero-overhead wrapper (no provenance tracking)
--
-- This unified instance allows polymorphic code to use @MonadThunk (Cited prov ...)@
-- without knowing the concrete value of @prov@ at compile time.
instance
  ( Has e Options
  , Framed e m
  , MonadThunk t m v
  , Typeable m
  , Typeable f
  , Typeable u
  , MonadCatch m
  , SBoolI prov
  )
  => MonadThunk (Cited prov u f m t) m v where

  thunk :: m v -> m (Cited prov u f m t)
  thunk mv = case sbool @prov of
    STrue -> do
      opts <- askOptions
      mt <- thunk @t mv
      if isThunks opts
        then do
          frames <- askFrames

          -- Gather the current evaluation context at the time of thunk
          -- creation, and record it along with the thunk.
          let
            fun :: SomeException -> [Provenance m (NValue u f m)]
            fun (fromException -> Just (EvaluatingExpr scope (Ann s e))) =
              one $ Provenance scope $ AnnF s (Nothing <$ e)
            fun _ = mempty

            ps :: [Provenance m (NValue u f m)]
            ps = foldMap (fun . frame) (framesToList frames)

          cite ps (pure mt)
        else cite mempty (pure mt)
    SFalse -> citeLite (thunk @t mv)
  {-# INLINABLE thunk #-}

  thunkId :: Cited prov u f m t -> ThunkId m
  thunkId (Cited rep) = case sbool @prov of
    STrue  -> let NCited _ t = rep in thunkId @_ @m t
    SFalse -> let Identity t = rep in thunkId @_ @m t
  {-# INLINABLE thunkId #-}

  query :: m v -> Cited prov u f m t -> m v
  query m (Cited rep) = case sbool @prov of
    STrue  -> let NCited _ t = rep in query m t
    SFalse -> let Identity t = rep in query m t
  {-# INLINABLE query #-}

  -- | The ThunkLoop exception is thrown as an exception with MonadThrow,
  --   which does not capture the current stack frame information to provide
  --   it in a NixException, so we catch and re-throw it here using
  --   'throwError' from Frames.hs.
  force :: Cited prov u f m t -> m v
  force (Cited rep) = case sbool @prov of
    STrue  -> let NCited ps t = rep in handleDisplayProvenance ps $ force t
    SFalse -> let Identity t = rep in force t  -- No provenance handling needed
  {-# INLINABLE force #-}

  forceEff :: Cited prov u f m t -> m v
  forceEff (Cited rep) = case sbool @prov of
    STrue  -> let NCited ps t = rep in handleDisplayProvenance ps $ forceEff t
    SFalse -> let Identity t = rep in forceEff t
  {-# INLINABLE forceEff #-}

  further :: Cited prov u f m t -> m (Cited prov u f m t)
  further (Cited rep) = case sbool @prov of
    STrue  -> let NCited ps t = rep in cite ps $ further t
    SFalse -> let Identity t = rep in citeLite $ further t
  {-# INLINABLE further #-}


-- * Representation

handleDisplayProvenance
  :: (MonadCatch m
    , Typeable m
    , Typeable v
    , Has e Frames
    , MonadReader e m
    )
  => [Provenance m v]
  -> m a
  -> m a
handleDisplayProvenance ps f =
  catch
    (displayProvenance ps f)
    (throwError @ThunkLoop)

displayProvenance
  :: (MonadThrow m
    , MonadReader e m
    , Has e Frames
    , Typeable m
    , Typeable v
    )
  => [Provenance m v]
  -> m a
  -> m a
displayProvenance =
  handlePresence
    id
    (\ (Provenance scope e@(AnnF s _) : _) ->
      withFrame Info $ ForcingExpr scope $ wrapExprLoc s e
    )
