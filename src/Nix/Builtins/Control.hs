{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Control flow and debugging builtins.
--
-- This module contains builtins for control flow and debugging:
-- throw, seq, deepSeq, tryEval, trace, traceVerbose, warn, addErrorContext, break.
module Nix.Builtins.Control
  ( -- * Error handling
    throwNix
  , tryEvalNix
  , addErrorContextNix
    -- * Evaluation control
  , seqNix
  , deepSeqNix
  , breakNix
    -- * Tracing and debugging
  , traceNix
  , traceVerboseNix
  , warnNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad.Catch            ( MonadCatch(catch) )
import qualified Nix.Core.AttrSet              as A
import           Nix.Builtins.Internal          ( pattern NVBool )
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec
import           Nix.Expr.Types                 ( mkVarName )
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Pretty                     ( printNix )
import           Nix.String                     ( ignoreContext )
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Monad


-- * Error handling

throwNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
throwNix =
  throwError . ErrorCall . toString . ignoreContext
    <=< coerceStringlikeToNixString CopyToStore

tryEvalNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
tryEvalNix e = (`catch` (pure . onError))
  (onSuccess <$> demand e)
 where
  onSuccess v =
    NVSet
      mempty
      $ A.fromList
        [ (mkVarName "success", NVBool True)
        , (mkVarName "value"  , v            )
        ]

  onError :: SomeException -> NValue t f m
  onError _ =
    NVSet
      mempty
      $ A.fromList
        $ (\n -> (mkVarName n, NVBool False)) <$>
          [ "success"
          , "value"
          ]

-- | Add error context (currently a no-op in HNix).
addErrorContextNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
addErrorContextNix _ = pure


-- * Evaluation control

-- | Evaluate `a` to WHNF to collect its topmost effect.
seqNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
seqNix a b = b <$ demand a

-- | Evaluate 'a' to NF to collect all of its effects, therefore data cycles are ignored.
deepSeqNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
deepSeqNix a b = b <$ normalForm_ a

-- | Debug breakpoint (currently a no-op in HNix).
breakNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
breakNix = pure


-- * Tracing and debugging

traceNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
traceNix msg action =
  do
    -- Normalize the value to handle thunks and cycles, then pretty-print
    normalized <- normalizeValue msg
    traceEffect @t @f @m $ toString $ printNix normalized
    pure action

-- | Emit a warning message during evaluation.
warnNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
warnNix msg action =
  do
    msgNs <- fromValue =<< demand msg
    traceEffect @t @f @m $ "evaluation warning: " <> toString (ignoreContext msgNs)
    demand action

traceVerboseNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
traceVerboseNix msg action =
  do
    opts <- askOptions
    if isTrace opts
      then do
        traceEffect @t @f @m . toString . ignoreContext =<< fromValue msg
        demand action
      else
        demand action
