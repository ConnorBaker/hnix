{-# language PatternSynonyms #-}

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
type StdInterned m = InternedValues (ThunkF m) (CitedF m) m

-- | Constraint alias for functions that access interned values in the standard monad.
--
-- This is the specialized form of 'GivenInterned' for 'StdM'.
-- Use this in type signatures for standard evaluation code:
--
-- @
-- myEval :: (StdBase m, GivenStdInterned cfg m) => NExprLoc -> StdM cfg m (StdValM cfg m)
-- @
type GivenStdInterned (cfg :: EvalCfg) m =
  GivenInterned (ThunkF (StdM cfg m)) (CitedF (StdM cfg m)) (StdM cfg m)


-- * Value types without provenance
--
-- With provenance removed, the types are simplified:
-- - CitedF is just Identity (zero overhead wrapper)
-- - ThunkF wraps NThunkF directly
-- - ValueF uses these simplified types

-- | Cited functor - now just Identity (zero overhead).
--
-- Previously parameterized by @prov :: Bool@ for optional provenance tracking.
-- With provenance removed, this is always Identity.
newtype CitedF m a =
  CitedF (Identity a)
  deriving (Functor, Applicative, Foldable, Traversable)

instance Comonad (CitedF m) where
  extract (CitedF (Identity a)) = a
  {-# INLINE extract #-}
  duplicate c = CitedF (Identity c)
  {-# INLINE duplicate #-}

instance ComonadEnv [()] (CitedF m) where
  ask _ = []  -- No provenance
  {-# INLINE ask #-}

-- | Thunk type without provenance.
newtype ThunkF m =
  ThunkF (CitedF m (NThunkF m (ValueF m)))

-- | Value type without provenance.
type ValueF m = NValue (ThunkF m) (CitedF m) m

-- | Value' type without provenance.
type ValueF' m = NValue' (ThunkF m) (CitedF m) m (ValueF m)

-- | Standard evaluation monad with compile-time configuration.
--
-- The @cfg@ parameter enables zero-cost conditional execution for stats and tracing.
-- Use 'withEvalCfg' at program startup to bridge runtime options to @cfg@.
type StdM (cfg :: EvalCfg) m = StandardT cfg (StdIdT m)

-- | Thunk type in the standard monad.
type StdThunM (cfg :: EvalCfg) m = ThunkF (StdM cfg m)

-- | Cited type in the standard monad.
type StdCitedM (cfg :: EvalCfg) m = CitedF (StdM cfg m)

-- | Value type in the standard monad.
type StdValM (cfg :: EvalCfg) m = ValueF (StdM cfg m)

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

-- | Type alias for the inner thunk type (the payload inside CitedF).
type InnerThunk m = NThunkF m (ValueF m)

-- | Show instance for ThunkF.
instance Show (ThunkF m) where
  show _ = toString thunkStubText

-- | HasCitations1 instance for CitedF.
-- With provenance removed, this is a no-op.
instance HasCitations1 m (ValueF m) (CitedF m) where
  citations1 _ = []
  {-# INLINE citations1 #-}
  addProvenance1 _ = id
  {-# INLINE addProvenance1 #-}

-- | HasCitations instance for ThunkF.
-- With provenance removed, this is a no-op.
instance HasCitations m (ValueF m) (ThunkF m) where
  citations _ = []
  {-# INLINE citations #-}
  addProvenance _ = id
  {-# INLINE addProvenance #-}

-- | MonadThunk instance for ThunkF.
-- Simplified without provenance tracking.
instance
  ( Typeable       m
  , MonadThunkId   m
  , MonadAtomicRef m
  , MonadCatch     m
  , MonadIO        m
  , MonadReader (Context cfg m (ValueF m) (StdInterned m)) m
  )
  => MonadThunk (ThunkF m) m (ValueF m) where

  thunkId
    :: ThunkF m
    -> ThunkId  m
  thunkId (ThunkF (CitedF (Identity t))) = thunkId @(NThunkF m (ValueF m)) @m t
  {-# INLINABLE thunkId #-}

  thunk
    :: m (ValueF m)
    -> m (ThunkF m)
  thunk action = do
    mstats <- askEvalStats
    traverse_ recordThunkCreate mstats
    ThunkF . CitedF . Identity <$> thunk @(NThunkF m (ValueF m)) action
  {-# INLINABLE thunk #-}

  query
    :: m (ValueF m)
    ->    ThunkF m
    -> m (ValueF m)
  query b (ThunkF (CitedF (Identity t))) = query @(NThunkF m (ValueF m)) @m b t
  {-# INLINABLE query #-}

  force
    ::    ThunkF m
    -> m (ValueF m)
  force (ThunkF (CitedF (Identity t))) = do
    mstats <- askEvalStats
    case mstats of
      Nothing -> handleDisplayProvenance $ force @(NThunkF m (ValueF m)) @m t
      Just stats -> do
        -- Only check computed state when profiling (for accurate stats)
        wasComputed <- isComputed t

        -- Save parent's accumulated child thunk time and reset for our children
        parentThunkChildTime <- liftIO $ readIORef (statsThunkChildTime stats)
        liftIO $ writeIORef (statsThunkChildTime stats) 0

        -- Save IO time before forcing (to exclude IO from pure compute time)
        ioTimeBefore <- liftIO $ readIORef (statsIOTime stats)

        start <- liftIO Clock.getMonotonicTimeNSec
        result <- handleDisplayProvenance $ force @(NThunkF m (ValueF m)) @m t
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
    ::    ThunkF m
    -> m (ValueF m)
  forceEff (ThunkF (CitedF (Identity t))) = handleDisplayProvenance $ forceEff @(NThunkF m (ValueF m)) @m t
  {-# INLINABLE forceEff #-}

  further
    ::    ThunkF m
    -> m (ThunkF m)
  further (ThunkF (CitedF (Identity t))) = ThunkF . CitedF . Identity <$> further @(NThunkF m (ValueF m)) @m t
  {-# INLINABLE further #-}

-- | Scoped instance for value types.
--
-- Uses instrumented lookupVar that can record scope stats when enabled.
instance
  ( MonadReader (Context cfg m (ValueF m) (StdInterned m)) m
  , MonadIO m
  )
  => Scoped (ValueF m) m where
  askScopes   = askScopesReader
  clearScopes = clearScopesReader @m @(ValueF m)
  pushScopes  = pushScopesReader
  setScopes   = setScopesReader
  lookupVar   = lookupVarWithStatsF

-- | Instrumented lookupVar for ValueF that records scope stats when enabled.
lookupVarWithStatsF
  :: forall cfg m
  . ( MonadReader (Context cfg m (ValueF m) (StdInterned m)) m
    , MonadIO m
    )
  => VarName
  -> m (Maybe (ValueF m))
lookupVarWithStatsF k = do
  mstats <- askEvalStats
  case mstats of
    Nothing -> lookupVarReader k
    Just stats -> do
      (result, info, elapsed) <- lookupVarReaderWithInfo @m @(ValueF m) k
      let scopeResult = case info of
            LexicalHit depth searched -> ScopeLexicalHit depth searched
            DynamicHit depth searched -> ScopeDynamicHit depth searched
            LookupMiss depth -> ScopeMiss depth
      recordScopeLookup stats scopeResult elapsed
      pure result


-- | MonadEffects instance for thunk/cited types.
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
  , Scoped (ValueF m) m
  , MonadReader (Context cfg m (ValueF m) (StdInterned m)) m
  , MonadState (HashMap Path NExprLoc, HashMap Text Text) m
  , MonadDataErrorContext (ThunkF m) (CitedF m) m
  , MonadThunk (ThunkF m) m (ValueF m)
  , MonadValue (ValueF m) m
  , GivenInterned (ThunkF m) (CitedF m) m  -- For interned value access
  )
  => MonadEffects (ThunkF m) (CitedF m) m where
  toAbsolutePath   = defaultToAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

-- * @instance MonadValue (ValueF m) m@
instance
  ( MonadAtomicRef m
  , MonadCatch m
  , MonadIO m
  , Typeable m
  , MonadReader (Context cfg m (ValueF m) (StdInterned m)) m
  , MonadThunkId m
  , MonadThunk (ThunkF m) m (ValueF m)
  )
  => MonadValue (ValueF m) m where

  defer
    :: m (ValueF m)
    -> m (ValueF m)
  defer action = pure . coerce <$> thunk @(ThunkF m) action
  {-# INLINABLE defer #-}

  demand
    :: ValueF m
    -> m (ValueF m)
  demand = go
   where
    go :: ValueF m -> m (ValueF m)
    go =
      free
        (go <=< force @(ThunkF m) . coerce)
        (pure . Free)
  {-# INLINABLE demand #-}

  inform
    :: ValueF m
    -> m (ValueF m)
  inform = go
   where
    go :: ValueF m -> m (ValueF m)
    go =
      free
        ((pure . coerce <$>) . (further @(ThunkF m) . coerce))
        ((Free <$>) . bindNValue' id go)
  {-# INLINABLE inform #-}


-- | The core evaluation transformer, parameterized by config.
--
-- The @cfg@ parameter enables zero-cost conditional execution for features
-- (stats, tracing). When these are known at compile time (via 'withEvalCfg'),
-- GHC eliminates unused branches.
newtype StandardTF (cfg :: EvalCfg) r m a
  = StandardTF
      (ReaderT
        (Context cfg r (ValueF r) (StdInterned r))
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
    , MonadReader (Context cfg r (ValueF r) (StdInterned r))
    )

instance MonadTrans (StandardTF cfg r) where
  lift = StandardTF . lift . lift
  {-# INLINABLE lift #-}

instance (MonadPutStr r, MonadPutStr m)
  => MonadPutStr (StandardTF cfg r m)
instance (MonadHttp r, MonadHttp m)
  => MonadHttp (StandardTF cfg r m)
instance (MonadEnv r, MonadEnv m)
  => MonadEnv (StandardTF cfg r m)
instance (MonadPaths r, MonadPaths m)
  => MonadPaths (StandardTF cfg r m)
instance (MonadInstantiate r, MonadInstantiate m)
  => MonadInstantiate (StandardTF cfg r m)
instance (MonadExec r, MonadExec m)
  => MonadExec (StandardTF cfg r m)
instance (MonadIntrospect r, MonadIntrospect m)
  => MonadIntrospect (StandardTF cfg r m)

---------------------------------------------------------------------------------

-- | Standard evaluation monad, parameterized by config.
--
-- When @cfg@ is known at compile time, GHC eliminates unused branches
-- for disabled features (stats, tracing).
type StandardT (cfg :: EvalCfg) m = Fix1T (StandardTF cfg) m

instance MonadTrans (Fix1T (StandardTF cfg)) where
  lift = Fix1T . lift
  {-# INLINABLE lift #-}

instance MonadThunkId m
  => MonadThunkId (StandardT cfg m) where

  type ThunkId (StandardT cfg m) = ThunkId m

mkStandardT
  :: ReaderT
      (Context cfg (StandardT cfg m) (ValueF (StandardT cfg m)) (StdInterned (StandardT cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
  -> StandardT cfg m a
mkStandardT = coerce
{-# INLINABLE mkStandardT #-}

runStandardT
  :: StandardT cfg m a
  -> ReaderT
      (Context cfg (StandardT cfg m) (ValueF (StandardT cfg m)) (StdInterned (StandardT cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
runStandardT = coerce
{-# INLINABLE runStandardT #-}

runWithBasicEffectsAndStats
  :: forall m cfg a
   . (MonadIO m, MonadAtomicRef m)
  => Options
  -> Maybe EvalStats
  -> (GivenStdInterned cfg m => StandardT cfg (StdIdT m) a)
  -> m a
runWithBasicEffectsAndStats opts mstats action =
  -- Use 'give' to provide interned values via the Given constraint.
  -- This enables zero-overhead access to interned values via pure functions
  -- (internedTrue, internedFalse, etc.) instead of monadic lookups.
  give interned $
    fun $ (`evalStateT` mempty) $ (`runReaderT` newContextWithInterned opts mstats interned) $ runStandardT action
 where
  interned :: StdInterned (StdM cfg m)
  interned = mkInternedValues

  fun :: StdIdT m a -> m a
  fun act = runFreshIdT act =<< newRef (1 :: Int)

runWithBasicEffects
  :: (MonadIO m, MonadAtomicRef m)
  => Options
  -> (GivenStdInterned cfg m => StandardT cfg (StdIdT m) a)
  -> m a
runWithBasicEffects opts action = runWithBasicEffectsAndStats opts Nothing action

-- | Type-parameterized runner with compile-time configuration dispatch.
--
-- When configuration flags are known at compile time (established via 'withEvalCfg'),
-- this function enables zero-cost conditional execution.
--
-- The action receives interned values via the @GivenStdInterned@ constraint,
-- enabling zero-overhead access to singleton values (true, false, null, [], {}).
--
-- Example usage:
--
-- @
-- main' opts = withEvalCfg (isEvalStats opts) (isTrace opts) $
--   \\(_ :: Proxy cfg) ->
--     runWithStoreEffectsIOT \@cfg opts myAction
-- @
runWithStoreEffectsIOT
  :: forall (cfg :: EvalCfg) a
   . KnownEvalCfg cfg
  => Options
  -> (forall m. (StdBase m, KnownEvalCfg cfg, GivenStdInterned cfg m) => StdM cfg m a)
  -> IO a
runWithStoreEffectsIOT opts action = do
  -- Create stats collector only when type-level says it's needed
  -- When CfgStats cfg ~ 'False, GHC eliminates the Just branch
  mstats <- ifStats @cfg (Just <$> newEvalStats) (pure Nothing)

  -- Warn about invalid option combinations
  when (getStoreMode opts == StoreRemote && getStoreDir opts /= "/nix/store") $
    IO.hPutStrLn IO.stderr "Warning: --store-dir is ignored in remote mode (nix-daemon always uses /nix/store)"

  -- Run the action
  result <- case getStoreMode opts of
    StoreRemote ->
      runWithBasicEffectsAndStats opts mstats (action :: StdM cfg IO a)
    StoreOverlay ->
      let
        storeDir = Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts
        storeCfg = OverlayStoreConfig
          { overlayStoreDir = storeDir
          , overlayReadThrough = isStoreReadThrough opts
          }
      in
        evalOverlayStoreT storeCfg defaultOverlayStoreState $
          runWithBasicEffectsAndStats opts mstats (action :: StdM cfg (OverlayStoreT IO) a)

  -- Print stats only when enabled at type level
  -- When CfgStats cfg ~ 'False, GHC eliminates this branch
  whenStatsM @cfg $ traverse_ printEvalStats mstats

  pure result
{-# INLINABLE runWithStoreEffectsIOT #-}
