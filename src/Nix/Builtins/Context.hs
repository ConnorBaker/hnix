{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | String context builtins.
--
-- This module contains builtins for manipulating string context:
-- hasContext, getContext, appendContext, unsafeDiscardStringContext,
-- unsafeDiscardOutputDependency, addDrvOutputDependencies, outputOf.
module Nix.Builtins.Context
  ( -- * Context queries
    hasContextNix
  , getContextNix
    -- * Context manipulation
  , appendContextNix
  , unsafeDiscardStringContextNix
  , unsafeDiscardOutputDependencyNix
  , addDrvOutputDependenciesNix
  , outputOfNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Data.HashSet                  as HS
import qualified Nix.Core.AttrSet              as A
import qualified Data.Text                     as Text
import           Nix.Convert
import qualified Nix.Core.List                 as L
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Frames
import           Nix.String
import           Nix.Value
import           Nix.Value.Interned             ( internedBool )
import           Nix.Value.Monad


-- * Context queries

-- | Check if a string has context. Returns interned boolean.
hasContextNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
hasContextNix nv = do
  ns <- fromValue nv
  pure . internedBool $ hasContext ns

getContextNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
getContextNix =
  \case
    (NVStr ns) ->
      NVSet emptyPositionSet <$> traverseToValue (getNixLikeContext $ toNixLikeContext $ getStringContext ns)
    x -> throwError $ ErrorCall $ "Invalid type for builtins.getContext: " <> show x
  <=< demand


-- * Context manipulation

appendContextNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
appendContextNix tx ty =
  do
    x <- demand tx
    y <- demand ty

    case (x, y) of
      (NVStr ns, NVSet _ attrs) ->
        do
          let
            getPathNOuts :: NValue t f m -> m NixLikeContextValue
            getPathNOuts tx =
              do
                x <- demand tx

                case x of
                  NVSet _ atts ->
                    do
                      -- TODO: Fail for unexpected keys.

                      let
                        getK :: VarName -> m Bool
                        getK k =
                          case A.lookup k atts of
                            Nothing -> pure False
                            Just v -> fromValue =<< demand v

                        getOutputs :: m [Text]
                        getOutputs =
                          case A.lookup (mkVarName "outputs") atts of
                            Nothing -> stub
                            Just touts -> do
                              outs <- demand touts
                              case outs of
                                NVList vs -> L.nlToList <$> L.nlMapM (fmap ignoreContext . fromValue) vs
                                _x -> throwError $ ErrorCall $ "Invalid types for context value outputs in builtins.appendContext: " <> show _x

                      path <- getK "path"
                      allOutputs <- getK "allOutputs"

                      NixLikeContextValue path allOutputs <$> getOutputs

                  _x -> throwError $ ErrorCall $ "Invalid types for context value in builtins.appendContext: " <> show _x
            addContext :: AttrSet NixLikeContextValue -> NixString
            addContext newContextValues =
              mkNixString
                (fromNixLikeContext $
                  NixLikeContext $
                    A.unionWith
                      (<>)
                      newContextValues
                      $ getNixLikeContext $
                          toNixLikeContext $
                            getStringContext ns
                )
                $ ignoreContext ns

          toValue . addContext =<< traverse getPathNOuts attrs

      _xy -> throwError $ ErrorCall $ "Invalid types for builtins.appendContext: " <> show _xy

unsafeDiscardStringContextNix
  :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
unsafeDiscardStringContextNix =
  inHask (mkNixStringWithoutContext . ignoreContext)

unsafeDiscardOutputDependencyNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
unsafeDiscardOutputDependencyNix nv =
  do
    (nc, ns) <- (getStringContext &&& ignoreContext) <$> fromValue nv
    toValue $ mkNixString (HS.map discard nc) ns
 where
  discard :: StringContext -> StringContext
  discard (StringContext AllOutputs a) = StringContext DirectPath a
  discard x                            = x

addDrvOutputDependenciesNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
addDrvOutputDependenciesNix nv =
  do
    ns <- fromValue =<< demand nv
    let
      ctx = getStringContext ns
      contents = ignoreContext ns
      ctxSize = HS.size ctx

    sc <-
      case HS.toList ctx of
        [single] -> pure single
        _ ->
          throwError $
            ErrorCall $
              "builtins.addDrvOutputDependencies: string context must have exactly one element, but has "
              <> show ctxSize

    let
      path = getStringContextPath sc
      pathText = varNameText path
      ensureDrv =
        when (not (".drv" `Text.isSuffixOf` pathText)) $
          throwError $ ErrorCall $ "builtins.addDrvOutputDependencies: path '" <> show pathText <> "' is not a derivation"

    case getStringContextFlavor sc of
      DirectPath -> do
        ensureDrv
        toValue $ mkNixString (one $ StringContext AllOutputs path) contents
      AllOutputs ->
        pure $ NVStr ns
      DerivationOutput out ->
        throwError $
          ErrorCall $
            "builtins.addDrvOutputDependencies: cannot act on derivation output '" <> show out <> "'"

outputOfNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
outputOfNix nvDrvRef nvOutputName =
  do
    drvRef <- fromValue =<< demand nvDrvRef
    outputName <- fromStringNoContext =<< fromValue =<< demand nvOutputName

    let
      contents = ignoreContext drvRef
      ctx = getStringContext drvRef

    drvPath <-
      case HS.toList ctx of
        [sc] -> do
          let p = getStringContextPath sc
          ensureDrvPath p
          pure p
        [] -> do
          let p = mkVarName contents
          ensureDrvText contents
          pure p
        _ ->
          throwError $
            ErrorCall $
              "builtins.outputOf: string context must have exactly one element, but has "
              <> show (HS.size ctx)

    toValue $ mkNixString (one $ StringContext (DerivationOutput outputName) drvPath) contents
 where
  ensureDrvText :: Text -> m ()
  ensureDrvText p =
    when (not (".drv" `Text.isSuffixOf` p)) $
      throwError $ ErrorCall $ "builtins.outputOf: path '" <> show p <> "' is not a derivation"

  ensureDrvPath :: VarName -> m ()
  ensureDrvPath = ensureDrvText . varNameText
