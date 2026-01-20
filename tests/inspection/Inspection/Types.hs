-- | Concrete type aliases for inspection testing.
--
-- These aliases fix the common case parameters:
-- - @prov ~ 'False@ (provenance disabled)
-- - @cfg ~ DefaultCfg@ (all config flags disabled)
--
-- This allows inspection tests to verify specialization for the most common
-- evaluation scenario.
module Inspection.Types
  ( -- * Standard monad (common case)
    TestM
    -- * Value/Thunk/Cited types (common case)
  , TestThunk
  , TestCited
  , TestValue
  , TestValue'
    -- * Underlying Cited representation
  , TestInnerCited
  , TestInnerThunk
    -- * Re-exports for convenience
  , DefaultCfg
  , SBool(..)
  , SBoolI
  ) where

import           Relude

import           Data.Singletons.Bool           ( SBool(..), SBoolI(..) )
import           Nix.Config.Singleton           ( DefaultCfg )
import           Nix.Standard                   ( StdM
                                                , ThunkF
                                                , CitedF
                                                , ValueF
                                                , ValueF'
                                                , InnerCitedThunk
                                                , InnerThunk
                                                )


-- | Standard monad with provenance disabled and default config.
--
-- This is the common case for normal evaluation without debugging features.
type TestM = StdM 'False DefaultCfg IO

-- | Thunk type for the common case.
type TestThunk = ThunkF 'False TestM

-- | Cited functor for the common case.
type TestCited = CitedF 'False TestM

-- | Value type for the common case.
type TestValue = ValueF 'False TestM

-- | Value' type for the common case.
type TestValue' = ValueF' 'False TestM

-- | Inner Cited type (inside ThunkF) for the common case.
type TestInnerCited = InnerCitedThunk 'False TestM

-- | Inner thunk type (payload inside CitedF) for the common case.
type TestInnerThunk = InnerThunk 'False TestM
