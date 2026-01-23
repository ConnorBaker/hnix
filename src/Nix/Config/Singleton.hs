
-- | Type-level evaluation configuration for zero-cost conditional execution.
--
-- This module provides compile-time specialization for evaluation hot paths
-- using a type-level configuration record. Configuration flags (stats collection,
-- tracing) are bundled into a single 'EvalCfg' type, allowing GHC to generate
-- specialized code with zero runtime overhead for disabled features.
--
-- == Usage
--
-- At program startup, use 'withEvalCfg' to bridge runtime options to
-- type-level configuration:
--
-- @
-- main :: IO ()
-- main = do
--   opts <- parseOptions
--   withEvalCfg
--     (isEvalStats opts)
--     (isTrace opts)
--     (\\(_ :: Proxy DefaultCfg) -> runEvaluation \@DefaultCfg ...)
--     (\\(_ :: Proxy cfg) -> runEvaluation \@cfg ...)
-- @
--
-- Within evaluation code, use 'singStats' and 'singTrace'
-- for zero-cost conditional execution:
--
-- @
-- evalExpr :: forall cfg m. KnownEvalCfg cfg => Expr -> m Value
-- evalExpr expr = case singTrace \@cfg of
--   STrue  -> evalWithTracing expr
--   SFalse -> evalWithoutTracing expr
-- @
--
-- When @CfgTrace cfg ~ 'False@, GHC eliminates the 'STrue' branch entirely via
-- case-of-known-constructor optimization.
module Nix.Config.Singleton
  ( -- * Configuration type
    EvalCfg  -- Abstract: constructor not exported
  , KnownEvalCfg
    -- * Default configuration (all flags disabled, for tests)
  , DefaultCfg
    -- * Per-flag constraints (use these for minimal constraints)
  , HasStatsCfg
  , HasTraceCfg
    -- * Type families
  , CfgStats
  , CfgTrace
    -- * Singleton accessors (zero-cost)
  , singStats
  , singTrace
    -- * Helper functions (zero-cost dispatch)
  , ifSBool
  , ifStats
  , ifTrace
  , whenStatsM
  , whenTraceM
    -- * Runtime bridge
  , withEvalCfg
    -- * Re-exports from singleton-bool
  , SBool(..)
  , SBoolI(..)
  ) where

import           Relude

import           Data.Singletons.Bool (SBool(..), SBoolI(..), sbool)

-- | Type-level configuration record (promoted to kind level via DataKinds).
--
-- Each field corresponds to a runtime configuration option that affects
-- evaluation behavior. By lifting these to the type level, we enable
-- compile-time specialization.
data EvalCfg = MkEvalCfg
  { _cfgStats :: Bool  -- ^ Collect evaluation statistics
  , _cfgTrace :: Bool  -- ^ Enable expression tracing
  }

-- | Default configuration with all flags disabled.
-- Use this for tests and simple usage where no special features are needed.
type DefaultCfg = 'MkEvalCfg 'False 'False

-- | Extract the stats flag from a configuration.
type family CfgStats (cfg :: EvalCfg) :: Bool where
  CfgStats ('MkEvalCfg s _) = s

-- | Extract the trace flag from a configuration.
type family CfgTrace (cfg :: EvalCfg) :: Bool where
  CfgTrace ('MkEvalCfg _ t) = t

-- | Constraint that all configuration flags are known at compile time.
--
-- Functions with this constraint can use 'singStats' and 'singTrace'
-- for zero-cost conditional execution.
--
-- Includes 'Typeable cfg' because renderFrames requires @Typeable v@ where
-- @v = StdValM cfg m@, which needs @Typeable cfg@.
type KnownEvalCfg cfg =
  ( SBoolI (CfgStats cfg)
  , SBoolI (CfgTrace cfg)
  , Typeable cfg
  )

-- | Constraint for functions that only need stats flag.
-- Use this instead of 'KnownEvalCfg' to minimize constraints.
type HasStatsCfg cfg = SBoolI (CfgStats cfg)

-- | Constraint for functions that only need trace flag.
-- Use this instead of 'KnownEvalCfg' to minimize constraints.
type HasTraceCfg cfg = SBoolI (CfgTrace cfg)

-- | Access the stats singleton for compile-time dispatch.
--
-- @
-- case singStats \@cfg of
--   STrue  -> collectStats
--   SFalse -> pure ()  -- Eliminated by GHC when CfgStats cfg ~ 'False
-- @
singStats :: forall cfg. HasStatsCfg cfg => SBool (CfgStats cfg)
singStats = sbool @(CfgStats cfg)
{-# INLINE singStats #-}

-- | Access the trace singleton for compile-time dispatch.
singTrace :: forall cfg. HasTraceCfg cfg => SBool (CfgTrace cfg)
singTrace = sbool @(CfgTrace cfg)
{-# INLINE singTrace #-}

-- | Zero-cost conditional based on singleton bool.
--
-- @
-- ifSBool STrue  trueVal falseVal = trueVal
-- ifSBool SFalse trueVal falseVal = falseVal
-- @
--
-- GHC eliminates the unused branch when the singleton is known at compile time.
ifSBool :: SBool b -> a -> a -> a
ifSBool STrue  t _ = t
ifSBool SFalse _ f = f
{-# INLINE ifSBool #-}

-- | Zero-cost conditional on stats flag.
--
-- @
-- ifStats \@cfg trueVal falseVal
-- @
--
-- When @CfgStats cfg ~ 'False@, the @trueVal@ is eliminated at compile time.
ifStats :: forall cfg a. HasStatsCfg cfg => a -> a -> a
ifStats = ifSBool (singStats @cfg)
{-# INLINE ifStats #-}

-- | Zero-cost conditional on trace flag.
ifTrace :: forall cfg a. HasTraceCfg cfg => a -> a -> a
ifTrace = ifSBool (singTrace @cfg)
{-# INLINE ifTrace #-}

-- | Execute action only when stats enabled (zero-cost when disabled).
whenStatsM :: forall cfg m. (HasStatsCfg cfg, Applicative m) => m () -> m ()
whenStatsM action = ifStats @cfg action (pure ())
{-# INLINE whenStatsM #-}

-- | Execute action only when tracing enabled (zero-cost when disabled).
whenTraceM :: forall cfg m. (HasTraceCfg cfg, Applicative m) => m () -> m ()
whenTraceM action = ifTrace @cfg action (pure ())
{-# INLINE whenTraceM #-}

-- | Bridge runtime booleans to type-level configuration.
--
-- This function should be called ONCE at program startup to convert
-- runtime configuration options into a type-level 'EvalCfg'. All subsequent
-- evaluation uses the single @cfg@ type parameter, enabling compile-time
-- specialization.
--
-- @
-- main = do
--   opts <- parseOptions
--   withEvalCfg
--     (isEvalStats opts)
--     (isTrace opts)
--     (\\(_ :: Proxy DefaultCfg) -> runEvaluation \@DefaultCfg ...)
--     (\\(_ :: Proxy cfg) -> do
--         -- From here, use \@cfg type application
--         runEvaluation \@cfg ...)
-- @
--
-- The explicit case dispatch (rather than CPS with existential quantification)
-- ensures that GHC sees CONCRETE types in each branch, enabling full
-- specialization of downstream code. This eliminates sbool runtime dispatch.
--
-- Previously this used @reifyBoolT@ which existentially quantified the type,
-- preventing GHC from specializing. With explicit enumeration, each branch
-- has a statically-known @cfg@ type.
withEvalCfg
  :: Bool  -- ^ Collect stats
  -> Bool  -- ^ Enable tracing
  -> (Proxy DefaultCfg -> r)  -- ^ Fast path for default config (all flags False)
  -> (forall cfg. KnownEvalCfg cfg => Proxy cfg -> r)
  -> r
withEvalCfg stats tracing kDefault k = case (stats, tracing) of
  (False, False) -> kDefault (Proxy @DefaultCfg)
  (False, True)  -> k (Proxy @('MkEvalCfg 'False 'True))
  (True,  False) -> k (Proxy @('MkEvalCfg 'True  'False))
  (True,  True)  -> k (Proxy @('MkEvalCfg 'True  'True))
{-# INLINE withEvalCfg #-}
