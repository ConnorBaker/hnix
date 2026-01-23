{-# LANGUAGE UndecidableInstances #-}

module Nix.Context
  ( Context(..)
  , CtxCfg
  , HasEvalCfg
    -- * Per-flag constraint aliases (shorter than 'HasStatsCfg (CtxCfg e)')
  , HasStatsCfgE
  , HasTraceCfgE
  , newContext
  , newContextWithStats
  , newContextWithInterned
  , askEvalStats
  ) where

import           Nix.Prelude
import           GHC.TypeLits                   ( TypeError, ErrorMessage(..) )
import           Nix.Config.Singleton           ( EvalCfg, KnownEvalCfg
                                                , HasStatsCfg, HasTraceCfg
                                                )
import           Nix.Options                    ( Options )
import           Nix.Scope                      ( Scopes )
import           Nix.Frames                     ( Frames )
import           Nix.EvalStats                  ( EvalStats )
import           Nix.Expr.Types.Annotated       ( SrcSpan
                                                , nullSpan
                                                )

-- | Type family to extract configuration from Context.
-- This enables compile-time dispatch in MonadEval instances.
--
-- If you're wrapping Context in a newtype, you'll need to add a type
-- instance for your wrapper. For example:
--
-- @
-- newtype MyEnv cfg m t = MyEnv (Context cfg m t i)
-- type instance CtxCfg (MyEnv cfg m t) = cfg
-- @
type family CtxCfg e where
  CtxCfg (Context cfg m t i) = cfg
  CtxCfg e = TypeError
    ( 'Text "Cannot extract EvalCfg from type: " ':<>: 'ShowType e
    ':$$: 'Text "CtxCfg only works with 'Context cfg m t i' or types with a CtxCfg instance."
    ':$$: 'Text "If using a custom environment wrapper, add: type instance CtxCfg (YourType ...) = cfg"
    )

-- | Constraint that the environment's configuration is known at compile time.
-- Use this instead of passing an independent @cfg@ type parameter - it ties
-- the configuration to the environment and guarantees consistency across
-- stats, provenance, and tracing dispatch.
--
-- Example:
--
-- @
-- evalExprLoc :: (MonadNix e t f m, HasEvalCfg e) => NExprLoc -> m (NValue t f m)
-- evalExprLoc = case singTrace @(CtxCfg e) of ...
-- @
type HasEvalCfg e = KnownEvalCfg (CtxCfg e)

-- | Constraint for functions that only need stats flag from environment.
-- Use this instead of @HasStatsCfg (CtxCfg e)@ to minimize verbosity.
type HasStatsCfgE e = HasStatsCfg (CtxCfg e)

-- | Constraint for functions that only need trace flag from environment.
-- Use this instead of @HasTraceCfg (CtxCfg e)@ to minimize verbosity.
type HasTraceCfgE e = HasTraceCfg (CtxCfg e)

--  2021-07-18: NOTE: It should be Options -> Scopes -> Frames -> Source(span)
-- | Evaluation context parameterized by compile-time configuration.
--
-- The @cfg@ parameter is a phantom type that carries the 'EvalCfg'
-- configuration. This enables zero-cost conditional execution in
-- the 'MonadEval' instance - GHC eliminates unused branches when
-- @cfg@ is known at compile time.
--
-- The @t@ parameter is the value type stored in scopes.
--
-- The @i@ parameter is for interned values. For NValue-based evaluation,
-- this is @InternedValues t f m@. For other contexts (like Lint), use @()@.
data Context (cfg :: EvalCfg) m t i =
  Context
    { getOptions   :: !Options
    , getScopes    :: !(Scopes m t)
    , getSource    :: !SrcSpan
    , getFrames    :: !Frames
    , getEvalStats :: !(Maybe EvalStats)
    , getInterned  :: !i
    }

instance Has (Context cfg m t i) (Scopes m t) where
  hasLens g a = (\x -> a { getScopes = x }) <$> g (getScopes a)

instance Has (Context cfg m t i) SrcSpan where
  hasLens g a = (\x -> a { getSource = x }) <$> g (getSource a)

instance Has (Context cfg m t i) Frames where
  hasLens g a = (\x -> a { getFrames = x }) <$> g (getFrames a)

instance Has (Context cfg m t i) Options where
  hasLens g a = (\x -> a { getOptions = x }) <$> g (getOptions a)

instance Has (Context cfg m t i) (Maybe EvalStats) where
  hasLens g a = (\x -> a { getEvalStats = x }) <$> g (getEvalStats a)

instance Has (Context cfg m t i) i where
  hasLens g a = (\x -> a { getInterned = x }) <$> g (getInterned a)

-- | Create a new evaluation context without interned values.
--
-- Use this for contexts that don't use NValue (like Lint).
newContext :: Options -> Context cfg m t ()
newContext o = Context o mempty nullSpan mempty Nothing ()

-- | Create a new evaluation context with optional statistics tracking.
--
-- Use this for contexts that don't use NValue (like Lint).
newContextWithStats :: Options -> Maybe EvalStats -> Context cfg m t ()
newContextWithStats o stats = Context o mempty nullSpan mempty stats ()

-- | Create a new evaluation context with interned values.
--
-- Use this for NValue-based evaluation where value interning is beneficial.
newContextWithInterned :: Options -> Maybe EvalStats -> i -> Context cfg m t i
newContextWithInterned o stats interned = Context o mempty nullSpan mempty stats interned

askEvalStats :: forall e m . (MonadReader e m, Has e (Maybe EvalStats)) => m (Maybe EvalStats)
askEvalStats = askLocal
