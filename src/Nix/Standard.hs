{-# language AllowAmbiguousTypes #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language ConstraintKinds #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

{-# options_ghc -Wno-orphans #-}


module Nix.Standard where

import           Nix.Prelude                   hiding ( StateT
                                                , runStateT
                                                , evalStateT
                                                , execStateT
                                                , ask
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( StateT
                                                , evalStateT
                                                )
import           Control.Comonad                ( Comonad(..) )
import           Control.Comonad.Env            ( ComonadEnv(..) )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , MonadMask
                                                )
import           Control.Monad.Free             ( Free(Free) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Ref              ( MonadRef(newRef)
                                                , MonadAtomicRef
                                                )
import qualified Text.Show
import           Nix.Cited
import           Nix.Cited.Basic
import           Nix.Config.Singleton
import           Nix.Context
import           Nix.EvalStats                  ( EvalStats(..)
                                                , newEvalStats
                                                , printEvalStats
                                                , recordThunkCreate
                                                , recordThunkForce
                                                , recordScopeLookup
                                                , ScopeLookupResult(..)
                                                )
import qualified GHC.Clock                     as Clock
import           Nix.Effects
import           Nix.Effects.Basic
import           Nix.Effects.Derivation
import           Nix.Expr.Types.Annotated
import           Nix.Fresh
import           Nix.Fresh.Basic
import           Nix.Options
import           Nix.Render
import           Nix.Scope
import           Nix.Expr.Types                 ( VarName )
import           Nix.Store.Overlay
import           Nix.Thunk
import           Nix.Thunk.Basic                ( NThunkF(..)
                                                , isComputed
                                                )
import           Nix.Utils.Fix1                 ( Fix1T(Fix1T) )
import           Nix.Value
import           Nix.Value.Interned            ( InternedValues, mkInternedValues, give, GivenInterned )
import           Nix.Value.Monad
import qualified System.IO                     as IO
import qualified System.Nix.StorePath          as Store

-- | Type alias for the interned values in the standard monad.
type StdInterned (prov :: Bool) m = InternedValues (ThunkF prov m) (CitedF prov m) m

-- | Constraint alias for functions that access interned values in the standard monad.
--
-- This is the specialized form of 'GivenInterned' for 'StdM'.
-- Use this in type signatures for standard evaluation code:
--
-- @
-- myEval :: (StdBase m, GivenStdInterned prov cfg m) => NExprLoc -> StdM prov cfg m (StdValM prov cfg m)
-- @
type GivenStdInterned (prov :: Bool) (cfg :: EvalCfg) m =
  GivenInterned (ThunkF prov (StdM prov cfg m)) (CitedF prov (StdM prov cfg m)) (StdM prov cfg m)


-- * Provenance-indexed types
--
-- The core types are parameterized by a @prov :: Bool@ type parameter that
-- controls provenance tracking. When @prov ~ 'True@, full provenance is tracked.
-- When @prov ~ 'False@, provenance tracking is eliminated with zero overhead.
--
-- The unified @Cited prov t f m a@ type from "Nix.Cited.Basic" provides the
-- foundation. We build @CitedF@, @ThunkF@, and @ValueF@ on top of it.

-- | Cited functor parameterized by provenance.
--
-- When @prov ~ 'True@: wraps @NCited@ (stores provenance list)
-- When @prov ~ 'False@: wraps @Identity@ (zero overhead)
newtype CitedF (prov :: Bool) m a =
  CitedF
    (Cited prov (ThunkF prov m) (CitedF prov m) m a)

instance SBoolI prov => Functor (CitedF prov m) where
  fmap f (CitedF c) = CitedF (fmap f c)
  {-# INLINE fmap #-}

instance SBoolI prov => Applicative (CitedF prov m) where
  pure = CitedF . pure
  {-# INLINE pure #-}
  CitedF f <*> CitedF a = CitedF (f <*> a)
  {-# INLINE (<*>) #-}

instance SBoolI prov => Foldable (CitedF prov m) where
  foldMap f (CitedF c) = foldMap f c
  {-# INLINE foldMap #-}

instance SBoolI prov => Traversable (CitedF prov m) where
  traverse f (CitedF c) = CitedF <$> traverse f c
  {-# INLINE traverse #-}

instance SBoolI prov => Comonad (CitedF prov m) where
  extract (CitedF c) = extract c
  {-# INLINE extract #-}
  duplicate (CitedF c) = CitedF (CitedF <$> duplicate c)
  {-# INLINE duplicate #-}

instance SBoolI prov => ComonadEnv [Provenance m (ValueF prov m)] (CitedF prov m) where
  ask (CitedF c) = coerce (ask c)
  {-# INLINE ask #-}

-- | Thunk type parameterized by provenance.
newtype ThunkF (prov :: Bool) m =
  ThunkF
    (CitedF prov m (NThunkF m (ValueF prov m)))

-- | Value type parameterized by provenance.
type ValueF (prov :: Bool) m = NValue (ThunkF prov m) (CitedF prov m) m

-- | Value' type parameterized by provenance.
type ValueF' (prov :: Bool) m = NValue' (ThunkF prov m) (CitedF prov m) m (ValueF prov m)

-- | Standard evaluation monad with compile-time configuration and provenance.
--
-- The @prov@ parameter controls provenance tracking at the type level.
-- The @cfg@ parameter enables zero-cost conditional execution for other features.
-- Use 'withEvalCfg' at program startup to bridge runtime options to @cfg@ and @prov@.
type StdM (prov :: Bool) (cfg :: EvalCfg) m = StandardT prov cfg (StdIdT m)

-- | Thunk type in the standard monad.
--
-- Now uses provenance-indexed @ThunkF prov@ type.
type StdThunM (prov :: Bool) (cfg :: EvalCfg) m = ThunkF prov (StdM prov cfg m)

-- | Cited type in the standard monad.
--
-- Now uses provenance-indexed @CitedF prov@ type.
type StdCitedM (prov :: Bool) (cfg :: EvalCfg) m = CitedF prov (StdM prov cfg m)

-- | Value type in the standard monad.
--
-- Now uses provenance-indexed @ValueF prov@ type. The @prov@ parameter is explicit,
-- eliminating the need for type family reduction with existential @cfg@.
type StdValM (prov :: Bool) (cfg :: EvalCfg) m = ValueF prov (StdM prov cfg m)

type StdBase m =
  ( MonadFix m
  , MonadFile m
  , MonadCatch m
  , MonadThrow m
  , MonadMask m
  , MonadEnv m
  , MonadPaths m
  , MonadExec m
  , MonadHttp m
  , MonadInstantiate m
  , MonadIntrospect m
  , MonadPlus m
  , MonadPutStr m
  , MonadStore m
  , MonadStoreRead m
  , MonadAtomicRef m
  , Typeable m
  )

-- | Type alias for the inner Cited type (used for coercion).
--
-- > Cited prov (ThunkF prov m) (CitedF prov m) m (NThunkF m (ValueF prov m))
type InnerCitedThunk (prov :: Bool) m =
  Cited prov (ThunkF prov m) (CitedF prov m) m (NThunkF m (ValueF prov m))

-- | Type alias for the inner thunk type (the payload inside CitedF).
type InnerThunk (prov :: Bool) m = NThunkF m (ValueF prov m)

-- | Show instance for ThunkF (provenance-indexed).
instance Show (ThunkF prov m) where
  show _ = toString thunkStubText

-- | HasCitations1 instance for CitedF (provenance-indexed).
--
-- Dispatches based on the @prov@ singleton:
-- - When @prov ~ 'True@: delegates to underlying NCited
-- - When @prov ~ 'False@: no-op (empty citations, identity addProvenance)
instance SBoolI prov => HasCitations1 m (ValueF prov m) (CitedF prov m) where
  citations1 (CitedF c) = citations1 c
  {-# INLINE citations1 #-}
  addProvenance1 x (CitedF c) = CitedF $ addProvenance1 x c
  {-# INLINE addProvenance1 #-}

-- | HasCitations instance for ThunkF (provenance-indexed).
instance SBoolI prov => HasCitations m (ValueF prov m) (ThunkF prov m) where
  citations (ThunkF c) = citations1 c
  {-# INLINE citations #-}
  addProvenance x (ThunkF c) = ThunkF $ addProvenance1 x c
  {-# INLINE addProvenance #-}

-- | Unified MonadThunk instance for ThunkF (any @prov@).
--
-- Uses the unified @MonadThunk (Cited prov ...)@ instance from Cited.Basic.
-- Includes optional stats tracking when enabled (via 'askEvalStats').
--
-- This unified instance allows polymorphic code to use @MonadThunk (ThunkF prov ...)@
-- without knowing the concrete value of @prov@ at compile time.
instance
  ( Typeable       m
  , Typeable       prov
  , MonadThunkId   m
  , MonadAtomicRef m
  , MonadCatch     m
  , MonadIO        m
  , SBoolI prov
  , MonadReader (Context cfg m (ValueF prov m) (StdInterned prov m)) m
  )
  => MonadThunk (ThunkF prov m) m (ValueF prov m) where

  thunkId
    :: ThunkF prov m
    -> ThunkId  m
  thunkId = thunkId @(InnerCitedThunk prov m) . coerce
  {-# INLINABLE thunkId #-}

  thunk
    :: m (ValueF prov m)
    -> m (ThunkF prov m)
  thunk action = do
    mstats <- askEvalStats
    traverse_ recordThunkCreate mstats
    coerce <$> thunk @(InnerCitedThunk prov m) action
  {-# INLINABLE thunk #-}

  query
    :: m (ValueF prov m)
    ->    ThunkF prov m
    -> m (ValueF prov m)
  query b = query @(InnerCitedThunk prov m) b . coerce
  {-# INLINABLE query #-}

  force
    ::    ThunkF prov m
    -> m (ValueF prov m)
  force t = do
    mstats <- askEvalStats
    case mstats of
      Nothing -> force @(InnerCitedThunk prov m) (coerce t)
      Just stats -> do
        -- Only check computed state when profiling (for accurate stats)
        let innerThunk = extractCited (coerce t :: InnerCitedThunk prov m)
        wasComputed <- isComputed innerThunk

        -- Save parent's accumulated child thunk time and reset for our children
        parentThunkChildTime <- liftIO $ readIORef (statsThunkChildTime stats)
        liftIO $ writeIORef (statsThunkChildTime stats) 0

        -- Save IO time before forcing (to exclude IO from pure compute time)
        ioTimeBefore <- liftIO $ readIORef (statsIOTime stats)

        start <- liftIO Clock.getMonotonicTimeNSec
        result <- force @(InnerCitedThunk prov m) (coerce t)
        end <- liftIO Clock.getMonotonicTimeNSec

        -- Get IO time after forcing
        ioTimeAfter <- liftIO $ readIORef (statsIOTime stats)
        let ioTimeDuring = ioTimeAfter - ioTimeBefore

        -- Get time spent in child thunk forces
        ourThunkChildTime <- liftIO $ readIORef (statsThunkChildTime stats)

        let elapsed = end - start
            -- For cache misses: exclusive time = total - child thunks - IO
            exclTime = if elapsed > ourThunkChildTime + ioTimeDuring
                         then elapsed - ourThunkChildTime - ioTimeDuring
                         else 0

        -- For hits, record full elapsed (it's just overhead); for misses, record exclusive time
        recordThunkForce stats wasComputed (if wasComputed then elapsed else exclTime)

        -- Add our total time to parent's child thunk time accumulator
        liftIO $ writeIORef (statsThunkChildTime stats) (parentThunkChildTime + elapsed)

        pure result
  {-# INLINABLE force #-}

  forceEff
    ::    ThunkF prov m
    -> m (ValueF prov m)
  forceEff = forceEff @(InnerCitedThunk prov m) . coerce
  {-# INLINABLE forceEff #-}

  further
    ::    ThunkF prov m
    -> m (ThunkF prov m)
  further = fmap coerce . further @(InnerCitedThunk prov m) . coerce
  {-# INLINABLE further #-}

-- | Scoped instance for provenance-indexed value types.
--
-- Uses instrumented lookupVar that can record scope stats when enabled.
instance
  ( MonadReader (Context cfg m (ValueF prov m) (StdInterned prov m)) m
  , MonadIO m
  , SBoolI prov
  )
  => Scoped (ValueF prov m) m where
  askScopes   = askScopesReader
  clearScopes = clearScopesReader @m @(ValueF prov m)
  pushScopes  = pushScopesReader
  setScopes   = setScopesReader
  lookupVar   = lookupVarWithStatsF

-- | Instrumented lookupVar for ValueF that records scope stats when enabled.
lookupVarWithStatsF
  :: forall prov cfg m
  . ( MonadReader (Context cfg m (ValueF prov m) (StdInterned prov m)) m
    , MonadIO m
    , SBoolI prov
    )
  => VarName
  -> m (Maybe (ValueF prov m))
lookupVarWithStatsF k = do
  mstats <- askEvalStats
  case mstats of
    Nothing -> lookupVarReader k
    Just stats -> do
      (result, info, elapsed) <- lookupVarReaderWithInfo @m @(ValueF prov m) k
      let scopeResult = case info of
            LexicalHit depth searched -> ScopeLexicalHit depth searched
            DynamicHit depth searched -> ScopeDynamicHit depth searched
            LookupMiss depth -> ScopeMiss depth
      recordScopeLookup stats scopeResult elapsed
      pure result


-- | MonadEffects instance for provenance-indexed thunk/cited types.
--
-- Uses the provenance-indexed @ThunkF prov@ and @CitedF prov@ types.
instance
  ( MonadFix m
  , MonadFile m
  , MonadCatch m
  , MonadEnv m
  , MonadPaths m
  , MonadExec m
  , MonadHttp m
  , MonadInstantiate m
  , MonadIntrospect m
  , MonadPlus m
  , MonadPutStr m
  , MonadStore m
  , MonadStoreRead m
  , MonadAtomicRef m
  , Typeable m
  , SBoolI prov
  , Scoped (ValueF prov m) m
  , MonadReader (Context cfg m (ValueF prov m) (StdInterned prov m)) m
  , MonadState (HashMap Path NExprLoc, HashMap Text Text) m
  , MonadDataErrorContext (ThunkF prov m) (CitedF prov m) m
  , MonadThunk (ThunkF prov m) m (ValueF prov m)
  , MonadValue (ValueF prov m) m
  , HasProvCfg cfg
  , GivenInterned (ThunkF prov m) (CitedF prov m) m  -- For interned value access
  )
  => MonadEffects (ThunkF prov m) (CitedF prov m) m where
  toAbsolutePath   = defaultToAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

-- * @instance MonadValue (ValueF prov m) m@
--
-- Unified instance for provenance-indexed values. Uses the constraints needed
-- for both prov ~ 'True and prov ~ 'False cases.
instance
  ( MonadAtomicRef m
  , MonadCatch m
  , MonadIO m
  , Typeable m
  , Typeable prov
  , SBoolI prov
  , MonadReader (Context cfg m (ValueF prov m) (StdInterned prov m)) m
  , MonadThunkId m
  , MonadThunk (ThunkF prov m) m (ValueF prov m)
  )
  => MonadValue (ValueF prov m) m where

  defer
    :: m (ValueF prov m)
    -> m (ValueF prov m)
  defer action = pure . coerce <$> thunk @(ThunkF prov m) action
  {-# INLINABLE defer #-}

  demand
    :: ValueF prov m
    -> m (ValueF prov m)
  demand = go
   where
    go :: ValueF prov m -> m (ValueF prov m)
    go =
      free
        (go <=< force @(ThunkF prov m) . coerce)
        (pure . Free)
  {-# INLINABLE demand #-}

  inform
    :: ValueF prov m
    -> m (ValueF prov m)
  inform = go
   where
    go :: ValueF prov m -> m (ValueF prov m)
    go =
      free
        ((pure . coerce <$>) . (further @(InnerCitedThunk prov m) . coerce))
        ((Free <$>) . bindNValue' id go)
  {-# INLINABLE inform #-}


-- | The core evaluation transformer, parameterized by provenance and config.
--
-- The @prov@ parameter controls provenance tracking at the type level:
-- - When @prov ~ 'True@: full provenance tracking with NCited
-- - When @prov ~ 'False@: zero-overhead provenance elimination with Identity
--
-- The @cfg@ parameter enables zero-cost conditional execution for other features
-- (stats, tracing). When these are known at compile time (via 'withEvalCfg'),
-- GHC eliminates unused branches.
--
-- By making @prov@ an explicit type parameter (rather than extracting it from
-- @cfg@ via type families), we avoid GHC's inability to reduce type families
-- under existential quantification.
newtype StandardTF (prov :: Bool) (cfg :: EvalCfg) r m a
  = StandardTF
      (ReaderT
        (Context cfg r (ValueF prov r) (StdInterned prov r))
        (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
        a
      )
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadPlus
    , MonadFix
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    , MonadState (HashMap Path NExprLoc, HashMap Text Text)
    , MonadReader (Context cfg r (ValueF prov r) (StdInterned prov r))
    )

instance MonadTrans (StandardTF prov cfg r) where
  lift = StandardTF . lift . lift
  {-# INLINABLE lift #-}

instance (MonadPutStr r, MonadPutStr m)
  => MonadPutStr (StandardTF prov cfg r m)
instance (MonadHttp r, MonadHttp m)
  => MonadHttp (StandardTF prov cfg r m)
instance (MonadEnv r, MonadEnv m)
  => MonadEnv (StandardTF prov cfg r m)
instance (MonadPaths r, MonadPaths m)
  => MonadPaths (StandardTF prov cfg r m)
instance (MonadInstantiate r, MonadInstantiate m)
  => MonadInstantiate (StandardTF prov cfg r m)
instance (MonadExec r, MonadExec m)
  => MonadExec (StandardTF prov cfg r m)
instance (MonadIntrospect r, MonadIntrospect m)
  => MonadIntrospect (StandardTF prov cfg r m)

---------------------------------------------------------------------------------

-- | Standard evaluation monad, parameterized by provenance and config.
--
-- When @prov@ and @cfg@ are known at compile time, GHC eliminates unused branches
-- for disabled features (provenance, stats, tracing).
type StandardT (prov :: Bool) (cfg :: EvalCfg) m = Fix1T (StandardTF prov cfg) m

instance MonadTrans (Fix1T (StandardTF prov cfg)) where
  lift = Fix1T . lift
  {-# INLINABLE lift #-}

instance MonadThunkId m
  => MonadThunkId (StandardT prov cfg m) where

  type ThunkId (StandardT prov cfg m) = ThunkId m

mkStandardT
  :: ReaderT
      (Context cfg (StandardT prov cfg m) (ValueF prov (StandardT prov cfg m)) (StdInterned prov (StandardT prov cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
  -> StandardT prov cfg m a
mkStandardT = coerce
{-# INLINABLE mkStandardT #-}

runStandardT
  :: StandardT prov cfg m a
  -> ReaderT
      (Context cfg (StandardT prov cfg m) (ValueF prov (StandardT prov cfg m)) (StdInterned prov (StandardT prov cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
runStandardT = coerce
{-# INLINABLE runStandardT #-}

runWithBasicEffectsAndStats
  :: forall m prov cfg a
   . (MonadIO m, MonadAtomicRef m, SBoolI prov)
  => Options
  -> Maybe EvalStats
  -> (GivenStdInterned prov cfg m => StandardT prov cfg (StdIdT m) a)
  -> m a
runWithBasicEffectsAndStats opts mstats action =
  -- Use 'give' to provide interned values via the Given constraint.
  -- This enables zero-overhead access to interned values via pure functions
  -- (internedTrue, internedFalse, etc.) instead of monadic lookups.
  give interned $
    fun $ (`evalStateT` mempty) $ (`runReaderT` newContextWithInterned opts mstats interned) $ runStandardT action
 where
  interned :: StdInterned prov (StdM prov cfg m)
  interned = mkInternedValues

  fun :: StdIdT m a -> m a
  fun act = runFreshIdT act =<< newRef (1 :: Int)

runWithBasicEffects
  :: (MonadIO m, MonadAtomicRef m, SBoolI prov)
  => Options
  -> (GivenStdInterned prov cfg m => StandardT prov cfg (StdIdT m) a)
  -> m a
runWithBasicEffects opts action = runWithBasicEffectsAndStats opts Nothing action

-- | Type-parameterized runner with compile-time configuration dispatch.
--
-- When configuration flags are known at compile time (established via 'withEvalCfg'),
-- this function enables zero-cost conditional execution. The @prov@ singleton from
-- @cfg@ is extracted and used to branch at the top level, calling specialized runners
-- for @'True@ or @'False@.
--
-- The action receives interned values via the @GivenStdInterned@ constraint,
-- enabling zero-overhead access to singleton values (true, false, null, [], {}).
--
-- Example usage:
--
-- @
-- main' opts = withEvalCfg (isEvalStats opts) (isValues opts) (isTrace opts) $
--   \\(_ :: Proxy cfg) ->
--     runWithStoreEffectsIOT \@cfg opts myAction
-- @
runWithStoreEffectsIOT
  :: forall (cfg :: EvalCfg) a
   . KnownEvalCfg cfg
  => Options
  -> (forall (prov :: Bool) m. (StdBase m, KnownEvalCfg cfg, SBoolI prov, Typeable prov, GivenStdInterned prov cfg m) => StdM prov cfg m a)
  -> IO a
runWithStoreEffectsIOT opts action =
  -- Branch on provenance singleton at the top level.
  -- This allows GHC to specialize the entire evaluation path for each case.
  case singProv @cfg of
    STrue  -> runWithStoreEffectsIOT' @'True  @cfg opts action
    SFalse -> runWithStoreEffectsIOT' @'False @cfg opts action
{-# INLINABLE runWithStoreEffectsIOT #-}

-- | Internal helper for runWithStoreEffectsIOT with explicit provenance.
runWithStoreEffectsIOT'
  :: forall (prov :: Bool) (cfg :: EvalCfg) a
   . (KnownEvalCfg cfg, SBoolI prov, Typeable prov)
  => Options
  -> (forall m. (StdBase m, KnownEvalCfg cfg, SBoolI prov, Typeable prov, GivenStdInterned prov cfg m) => StdM prov cfg m a)
  -> IO a
runWithStoreEffectsIOT' opts action = do
  -- Create stats collector only when type-level says it's needed
  -- When CfgStats cfg ~ 'False, GHC eliminates the Just branch
  mstats <- ifStats @cfg (Just <$> newEvalStats) (pure Nothing)

  -- Warn about invalid option combinations
  when (getStoreMode opts == StoreRemote && getStoreDir opts /= "/nix/store") $
    IO.hPutStrLn IO.stderr "Warning: --store-dir is ignored in remote mode (nix-daemon always uses /nix/store)"

  -- Run the action
  result <- case getStoreMode opts of
    StoreRemote ->
      runWithBasicEffectsAndStats opts mstats (action :: StdM prov cfg IO a)
    StoreOverlay ->
      let
        storeDir = Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts
        storeCfg = OverlayStoreConfig
          { overlayStoreDir = storeDir
          , overlayReadThrough = isStoreReadThrough opts
          }
      in
        evalOverlayStoreT storeCfg defaultOverlayStoreState $
          runWithBasicEffectsAndStats opts mstats (action :: StdM prov cfg (OverlayStoreT IO) a)

  -- Print stats only when enabled at type level
  -- When CfgStats cfg ~ 'False, GHC eliminates this branch
  whenStatsM @cfg $ traverse_ printEvalStats mstats

  pure result
{-# INLINABLE runWithStoreEffectsIOT' #-}
