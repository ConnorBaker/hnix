{-# language CPP #-}
{-# language PartialTypeSignatures #-}

{-# options_ghc -Wno-orphans #-}
{-# options_ghc -fno-warn-name-shadowing #-}


module Nix.Exec where

import           Nix.Prelude             hiding ( putStr
                                                , putStrLn
                                                , print
                                                )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Fix
import           Data.Fix
import qualified Data.List.NonEmpty            as NE
import qualified Nix.Core.AttrSet              as A
import qualified Data.Text                     as Text
import           Nix.Atoms
import           Nix.Cited
import           Nix.Config.Singleton
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval                      as Eval
import           Nix.Expr.Desugar              ( desugarExprLoc )
import           Nix.Expr.Strings              ( runAntiquoted )
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Options
import           Nix.Context                    ( askEvalStats, CtxCfg, HasEvalCfg )
import           Nix.Value.Interned             ( InternedValues(..)
                                                , GivenInterned
                                                , internedTrue
                                                , internedFalse
                                                , internedNull
                                                , internedEmptyList
                                                , internedEmptySet
                                                , internedEmptyString
                                                , internedBool
                                                )
import           Nix.EvalStats                  ( EvalStats(..), withExprTiming, withBuiltinTiming )
import           Nix.Pretty
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Thunk
import           Nix.Value
import           Nix.Value.Equal
import           Data.Vector                    ( Vector )
import qualified Nix.Core.List                 as L
import           Nix.Value.Monad
import           Prettyprinter
import qualified Text.Show.Pretty              as PS
import qualified GHC.Clock                     as Clock
import           Data.Data                     ( toConstr )
import           Nix.Types.VarName.Static       ( sCurPos, sFunctor )

#ifdef MIN_VERSION_ghc_datasize
import           GHC.DataSize
#endif

type MonadCited t f m =
  ( HasCitations m (NValue t f m) t
  , HasCitations1 m (NValue t f m) f
  , MonadDataContext f m
  )

type MonadCitedThunks t f m =
  ( MonadThunk t m (NValue t f m)
  , MonadDataErrorContext t f m
  , HasCitations m (NValue t f m) t
  , HasCitations1 m (NValue t f m) f
  )

-- | Core constraint for Nix evaluation monad.
--
-- Type-level configuration for compile-time specialization (stats, tracing) is
-- provided separately via 'EvalConfig' from "Nix.Config.Singleton". Functions
-- like 'evalExprLocT' use both 'MonadNix' and 'HasStats'/'HasTracing' constraints
-- to achieve zero-cost conditional execution. We deliberately avoid adding type
-- parameters here, as doing so would cause cascading ambiguous type variable
-- errors throughout the codebase.
--
-- The @GivenInterned t f m@ constraint provides zero-overhead access to
-- singleton values (true, false, null, [], {}) via the @reflection@ library.
-- Use @internedTrue@, @internedFalse@, @internedBool@, etc. for pure access.
type MonadNix e t f m =
  ( Has e SrcSpan
  , Has e Options
  , Has e (Maybe EvalStats)
  , Has e (InternedValues t f m)  -- TODO: Remove after full migration to Given
  , GivenInterned t f m  -- Zero-overhead interned value access
  , Scoped (NValue t f m) m
  , Framed e m
  , MonadFix m
  , MonadCatch m
  , MonadThrow m
  , Alternative m
  , MonadEffects t f m
  , MonadCitedThunks t f m
  , MonadValue (NValue t f m) m
  )

data ExecFrame t f m = Assertion SrcSpan (NValue t f m)
  deriving (Show)

instance MonadDataErrorContext t f m => Exception (ExecFrame t f m)

nverr :: forall e t f s m a. (MonadNix e t f m, Exception s) => s -> m a
nverr = evalError @(NValue t f m)

askSpan :: forall e m . (MonadReader e m, Has e SrcSpan) => m SrcSpan
askSpan = askLocal

wrapExprLoc :: SrcSpan -> NExprLocF r -> NExprLoc
wrapExprLoc span x = Fix $ NSymAnn span "<?>" <$ x
{-# INLINABLE wrapExprLoc #-}

--  2021-01-07: NOTE: This instance belongs to be beside MonadEval type class.
-- Currently instance is stuck in orphanage between the requirements to be MonadEval, aka Eval stage, and emposed requirement to be MonadNix (Execution stage). MonadNix constraint tries to put the cart before horse and seems superflous, since Eval in Nix also needs and can throw exceptions. It is between `nverr` and `evalError`.
instance MonadNix e t f m => MonadEval (NValue t f m) m where
  freeVariable var =
    nverr @e @t @f $ ErrorCall $ toString @Text $ "Undefined variable '" <> varNameText var <> "'"

  synHole name =
    do
      span  <- askSpan
      scope <- askScopes
      evalError @(NValue t f m) $ SynHole $
        SynHoleInfo
          { _synHoleInfo_expr  = NSynHoleAnn span name
          , _synHoleInfo_scope = scope
          }


  attrMissing ks ms =
    evalError @(NValue t f m) $ ErrorCall $ toString $
      case ms of
        Nothing -> "Inheriting unknown attribute: " <> attr
        Just s -> "Could not look up attribute " <> attr <> " in " <> show (prettyNValue s)
       where
        attr = Text.intercalate "." $ NE.toList $ fmap varNameText ks

  evalCurPos = do
    span@(SrcSpan delta _) <- askSpan
    toValue delta

  evaledSym _name val = pure val

  -- Use interned values for common constants.
  evalConstant c =
    case c of
      NBool b -> pure (internedBool b)
      NNull   -> pure internedNull
      _       -> pure $ NVConstant c

  evalString str =
    do
      -- Use coerceAnyToNixString to properly handle __toString and outPath
      ns <- assembleStringWithCoercion (stringParts str)
      evalStr ns
   where
    assembleStringWithCoercion :: [Antiquoted Text (m (NValue t f m))] -> m NixString
    assembleStringWithCoercion parts = fold <$> traverse coercePart parts

    coercePart :: Antiquoted Text (m (NValue t f m)) -> m NixString
    coercePart =
      runAntiquoted
        "\n"
        (pure . mkNixStringWithoutContext)
        coerceValue

    coerceValue :: m (NValue t f m) -> m NixString
    coerceValue mv = do
      v <- mv
      -- Use CopyToStore: when paths are interpolated in strings, they get added to the store
      coerceAnyToNixString callFunc CopyToStore v

  evalLiteralPath p = do
    realPath <- toAbsolutePath @t @f @m p
    pure $ NVPath realPath

  evalPath str =
    do
      mns <- assembleString str
      case mns of
        Nothing -> nverr $ ErrorCall "Failed to assemble path"
        Just ns -> do
          let litText = ignoreContext ns
          let litPath = fromString (toString litText)
          real <- toAbsolutePath @t @f @m litPath
          pure $ NVPath real

  evalEnvPath p = do
    realPath <- findEnvPath @t @f @m (coerce p)
    pure $ NVPath realPath

  evalUnary = execUnaryOp'

  evalBinary = execBinaryOp'

  evalWith c b = evalWithAttrSet c b

  evalIf c tVal fVal = do
    bl :: Bool <- fromValue c
    if bl then tVal else fVal

  evalAssert c body = do
    span <- askSpan
    b :: Bool <- fromValue c
    if b
      then body
      else nverr $ Assertion span c

  evalApp f x = do
    result <- callFunc f =<< defer x
    pure result

  evalAbs
    :: Params (m (NValue t f m))
    -> ( forall a
      . m (NValue t f m)
      -> ( AttrSet (m (NValue t f m))
        -> m (NValue t f m)
        -> m (a, NValue t f m)
        )
      -> m (a, NValue t f m)
      )
    -> m (NValue t f m)
  evalAbs p k =
    let closureFunc = fmap snd . flip (k @()) (const (fmap (mempty ,))) . pure
    in pure $ NVClosure (void p) closureFunc

  -- | Evaluate a list literal. Returns interned empty list for [].
  evalList elems =
    case elems of
      [] -> pure internedEmptyList
      _  -> pure $ NVList $ L.fromList elems

  -- | Evaluate a set literal. Returns interned empty set for {}.
  evalSet attrs posSet =
    if A.null attrs
      then pure internedEmptySet
      else pure $ NVSet posSet attrs

  -- | Evaluate a NixString result. Returns interned empty string for "".
  evalStr ns =
    if Text.null (ignoreContext ns) && not (hasContext ns)
      then pure internedEmptyString
      else pure $ NVStr ns

  evalError = throwError

infixl 1 `callFunc`
callFunc
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
callFunc fun arg =
  do
    -- O(min(2001, depth)) depth check - faster than O(n) for shallow stacks
    -- See Nix.Frames for performance notes
    frames <- askFrames
    when (exceedsDepth 2000 frames) $ throwError $ ErrorCall "Function call stack exhausted"

    fun' <- demand fun
    case fun' of
      NVBuiltin name f    ->
        do
          span <- askSpan
          mstats <- askEvalStats
          withFrame Info ((Calling @m @(NValue t f m)) name span) $
            case mstats of
              Nothing -> f arg
              Just stats -> withBuiltinTiming stats (varNameText name) (f arg)
      NVClosure _params f -> f arg
      (NVSet _ m) | Just f <- A.lookup sFunctor m ->
        (`callFunc` arg) =<< (`callFunc` fun') f
      _x -> throwError $ ErrorCall $ "Attempt to call non-function: " <> show _x

-- | Unary operator execution.
execUnaryOp'
  :: forall e t f m
   . MonadNix e t f m
  => NUnaryOp
  -> NValue t f m
  -> m (NValue t f m)
execUnaryOp' op arg =
  case arg of
    NVConstant c ->
      case (op, c) of
        (NNeg, NInt   i) ->
          case checkedNeg i of
            Left err -> throwError $ ErrorCall err
            Right r  -> pure $ NVConstant $ NInt r
        (NNeg, NFloat f) -> pure $ NVConstant $ NFloat (negate f)
        -- Use interned boolean
        (NNot, NBool  b) -> pure (internedBool (not b))
        _seq ->
          nverr @e @t @f $ ErrorCall $ "unsupported argument type for unary operator " <> show _seq
    _x ->
      nverr @e @t @f $ ErrorCall $ "argument to unary operator must evaluate to an atomic type: " <> show _x

-- | Binary operator execution.
--
-- Handles short-circuit operators (NEq, NNEq, NOr, NAnd, NImpl) directly,
-- and delegates forced operations to 'execBinaryOpForced''.
execBinaryOp'
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m)
  => NBinaryOp
  -> NValue t f m
  -> m (NValue t f m)
  -> m (NValue t f m)
execBinaryOp' op lval rarg =
  case op of
    NEq   -> helperEq id
    NNEq  -> helperEq not
    NOr   -> do
      bl <- fromValue lval
      if bl
        then wrapBool True
        else evalRight
    NAnd  -> do
      bl <- fromValue lval
      if bl
        then evalRight
        else wrapBool False
    NImpl -> do
      bl <- fromValue lval
      if bl
        then evalRight
        else wrapBool True
    _     ->
      do
        rval  <- rarg
        rval' <- demand rval
        lval' <- demand lval
        execBinaryOpForced' op lval' rval'

 where
  helperEq :: (Bool -> Bool) -> m (NValue t f m)
  helperEq flag = do
    rval <- rarg
    eq <- valueEqM lval rval
    wrapBool $ flag eq

  evalRight = do
    rval <- rarg
    x <- fromValue rval
    wrapBool x

  -- | Use interned booleans
  wrapBool :: Bool -> m (NValue t f m)
  wrapBool b = pure (internedBool b)
  {-# INLINE wrapBool #-}

-- | Forced binary operator execution.
execBinaryOpForced'
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m)
  => NBinaryOp
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
execBinaryOpForced' op lval rval =
  case op of
    NLt    -> mkCmpOp (<)
    NLte   -> mkCmpOp (<=)
    NGt    -> mkCmpOp (>)
    NGte   -> mkCmpOp (>=)
    NMinus -> mkCheckedBinNumOp checkedSub (-)
    NMult  -> mkCheckedBinNumOp checkedMul (*)
    NDiv   -> mkCheckedBinNumOp checkedDiv (/)
    NConcat ->
      case (lval, rval) of
        (NVList ls, NVList rs)
          -- Fast paths: avoid allocation when one or both lists are empty
          | L.null ls && L.null rs -> pure internedEmptyList
          | L.null ls -> pure rval
          | L.null rs -> pure lval
          | otherwise -> pure $ NVList $ ls <> rs
        _ -> unsupportedTypes

    NUpdate ->
      case (lval, rval) of
        (NVSet lp ls, NVSet rp rs)
          -- Fast paths: avoid allocation when one or both sets are empty
          | A.null ls && A.null rs -> pure internedEmptySet
          | A.null ls -> pure rval
          | A.null rs -> pure lval
          | otherwise  -> pure $ NVSet (rp <> lp) (rs <> ls)
        (NVSet _lp _ls, NVConstant NNull) -> pure lval
        (NVConstant NNull, NVSet _rp _rs) -> pure rval
        _ -> unsupportedTypes

    NPlus ->
      case (lval, rval) of
        (NVConstant _, NVConstant _) -> mkCheckedBinNumOp checkedAdd (+)
        (NVStr ls, NVStr rs)
          -- Fast paths: avoid allocation when one or both strings are empty AND have no context.
          -- If a string has context, we must concatenate to preserve it even if text is empty.
          | Text.null (ignoreContext ls) && not (hasContext ls) &&
            Text.null (ignoreContext rs) && not (hasContext rs) -> pure lval
          | Text.null (ignoreContext ls) && not (hasContext ls) -> pure rval
          | Text.null (ignoreContext rs) && not (hasContext rs) -> pure lval
          | otherwise -> pure $ NVStr (ls <> rs)
        (NVStr ls, NVPath p) ->
          NVStr . (ls <>) <$> coercePathToNixString CopyToStore p
        (NVPath ls, NVStr rs) ->
          case getStringNoContext rs of
            Nothing ->
              throwError $ ErrorCall "A string that refers to a store path cannot be appended to a path." -- data/nix/src/libexpr/eval.cc:1412
            Just rs2 -> NVPath <$> toAbsolutePath @t @f (ls <> coerce (toString rs2))
        (NVPath ls, NVPath rs) -> NVPath <$> toAbsolutePath @t @f (ls <> rs)

        (ls@NVSet{}, NVStr rs) ->
          NVStr . (<> rs) <$> coerceAnyToNixString callFunc DontCopyToStore ls
        (NVStr ls, rs@NVSet{}) ->
          NVStr . (ls <>) <$> coerceAnyToNixString callFunc DontCopyToStore rs
        _ -> unsupportedTypes
    _other   -> shouldBeAlreadyHandled

 where
  -- | Create a boolean result using interned booleans.
  mkBoolP :: Bool -> m (NValue t f m)
  mkBoolP b = pure (internedBool b)

  mkIntP :: Int64 -> m (NValue t f m)
  mkIntP = pure . NVConstant . NInt

  mkFloatP :: Double -> m (NValue t f m)
  mkFloatP = pure . NVConstant . NFloat

  mkCmpOp :: (forall a. Ord a => a -> a -> Bool) -> m (NValue t f m)
  mkCmpOp cmpOp = case (lval, rval) of
    (NVConstant l, NVConstant r) -> mkBoolP $ l `cmpOp` r
    (NVStr l, NVStr r) -> mkBoolP $ l `cmpOp` r
    _ -> unsupportedTypes

  -- | Binary numeric operation with checked Int64 arithmetic that throws on overflow.
  mkCheckedBinNumOp
    :: (Int64 -> Int64 -> Either String Int64)
    -> (Double -> Double -> Double)
    -> m (NValue t f m)
  mkCheckedBinNumOp intOp floatOp =
    case (lval, rval) of
      (NVConstant l, NVConstant r) ->
        case (l, r) of
          (NInt   li, NInt   ri) ->
            case intOp li ri of
              Left err -> throwError $ ErrorCall err
              Right result -> mkIntP result
          (NInt   li, NFloat rf) -> mkFloatP $ fromIntegral li `floatOp` rf
          (NFloat lf, NInt   ri) -> mkFloatP $ lf `floatOp` fromIntegral ri
          (NFloat lf, NFloat rf) -> mkFloatP $ lf `floatOp` rf
          _ -> unsupportedTypes
      _ -> unsupportedTypes

  unsupportedTypes = throwError $ ErrorCall $ "Unsupported argument types for binary operator " <> show op <> ": " <> show lval <> ", " <> show rval

  shouldBeAlreadyHandled = throwError $ ErrorCall $ "This cannot happen: operator " <> show op <> " should have been handled in execBinaryOp."

-- This function is here, rather than in 'Nix.String', because of the need to
-- use 'throwError'.
fromStringNoContext
  :: Framed e m
  => NixString
  -> m Text
fromStringNoContext ns =
  case getStringNoContext ns of
    Nothing -> throwError $ ErrorCall $ "expected string with no context, but got " <> show ns
    Just v -> pure v

addTracing
  ::( MonadNix e t f m
    , Has e Options
    , Alternative n
    , MonadReader Int n
    , MonadFail n
    )
  => Alg NExprLocF (m a)
  -> Alg NExprLocF (n (m a))
addTracing k v = do
  depth <- ask
  guard $ depth < 2000
  local succ $ do
    v'@(AnnF span x) <- sequenceA v
    pure $ do
      opts <- askOptions
      let
        rendered =
          if getVerbosity opts >= Chatty
            then pretty $ PS.ppShow $ void x
            else prettyNix $ Fix $ Fix (NSym "?") <$ x
        msg x = pretty ("eval: " <> replicate depth ' ') <> x
      loc <- renderLocation span $ msg rendered <> " ...\n"
      putStr $ show loc
      res <- k v'
      print $ msg rendered <> " ...done"
      pure res

addTiming
  :: forall e t f m a
   . MonadNix e t f m
  => Int
  -> Alg NExprLocF (m a)
  -> Alg NExprLocF (m a)
addTiming thresholdMs k v@(AnnF span x) = do
  mstats <- askEvalStats
  -- Get IO time before evaluation (if stats available)
  ioTimeBefore <- case mstats of
    Just stats -> liftIO $ readIORef (statsIOTime stats)
    Nothing -> pure 0

  start <- liftIO Clock.getMonotonicTimeNSec
  res <- k v
  end <- liftIO Clock.getMonotonicTimeNSec

  -- Get IO time after evaluation and compute pure elapsed time
  ioTimeAfter <- case mstats of
    Just stats -> liftIO $ readIORef (statsIOTime stats)
    Nothing -> pure 0
  let ioTimeDuring = ioTimeAfter - ioTimeBefore
      totalNs = end - start
      -- Exclude IO time from the reported timing
      pureNs = if totalNs > ioTimeDuring then totalNs - ioTimeDuring else 0
      elapsedMs :: Int
      elapsedMs = fromIntegral (pureNs `div` 1000000)

  when (elapsedMs >= max 0 thresholdMs) $ do
    let headTag = Text.pack (show (toConstr (void x)))
    let msg =
          "timing "
            <> Text.pack (show elapsedMs)
            <> "ms "
            <> headTag
            <> "\n"
    loc <- renderLocation span (pretty msg)
    putStr $ show loc
  pure res

addStats
  :: forall e t f m a
   . MonadNix e t f m
  => EvalStats
  -> Alg NExprLocF (m a)
  -> Alg NExprLocF (m a)
addStats stats k v@(AnnF _ x) =
  let exprType = Text.pack (show (toConstr (void x)))
  in withExprTiming stats exprType (k v)
{-# INLINABLE addStats #-}

evalWithTracingAndMetaInfo
  :: forall e t f m
  . MonadNix e t f m
  => NExprLoc
  -> ReaderT Int m (m (NValue t f m))
evalWithTracingAndMetaInfo =
  adi
    addMetaInfo
    (addTracing Eval.evalContent)
  where
  addMetaInfo :: (NExprLoc -> ReaderT r m a) -> NExprLoc -> ReaderT r m a
  addMetaInfo = (ReaderT .) . flip . (Eval.addMetaInfo .) . flip . (runReaderT .)
{-# INLINABLE evalWithTracingAndMetaInfo #-}

evalWithTimingAndMetaInfo
  :: forall e t f m
  . MonadNix e t f m
  => Int
  -> NExprLoc
  -> m (NValue t f m)
evalWithTimingAndMetaInfo thresholdMs =
  adi
    Eval.addMetaInfo
    (addTiming thresholdMs Eval.evalContent)
{-# INLINABLE evalWithTimingAndMetaInfo #-}

evalWithTracingTimingAndMetaInfo
  :: forall e t f m
  . MonadNix e t f m
  => Int
  -> NExprLoc
  -> ReaderT Int m (m (NValue t f m))
evalWithTracingTimingAndMetaInfo thresholdMs =
  adi
    addMetaInfo
    (addTracing (addTiming thresholdMs Eval.evalContent))
  where
  addMetaInfo :: (NExprLoc -> ReaderT r m a) -> NExprLoc -> ReaderT r m a
  addMetaInfo = (ReaderT .) . flip . (Eval.addMetaInfo .) . flip . (runReaderT .)
{-# INLINABLE evalWithTracingTimingAndMetaInfo #-}

evalWithStatsAndMetaInfo
  :: forall e t f m
  . MonadNix e t f m
  => EvalStats
  -> NExprLoc
  -> m (NValue t f m)
evalWithStatsAndMetaInfo stats =
  adi
    Eval.addMetaInfo
    (addStats stats Eval.evalContent)
{-# INLINABLE evalWithStatsAndMetaInfo #-}

evalExprLoc :: forall e t f m. MonadNix e t f m => NExprLoc -> m (NValue t f m)
evalExprLoc expr =
  do
    opts <- askOptions
    mstats <- askEvalStats
    let thresholdMs = getEvalTimingThresholdMs opts
    let
      -- Apply AST-level desugaring before evaluation
      desugared = desugarExprLoc expr
      traced = isTrace opts
      timed  = isEvalTiming opts
      pTracedAdi =
        case (traced, timed, mstats) of
          (True, True, _) ->
            join . (`runReaderT` (0 :: Int)) . evalWithTracingTimingAndMetaInfo thresholdMs
          (True, False, _) ->
            join . (`runReaderT` (0 :: Int)) . evalWithTracingAndMetaInfo
          (False, True, _) ->
            evalWithTimingAndMetaInfo thresholdMs
          (False, False, Just stats) ->
            evalWithStatsAndMetaInfo stats
          (False, False, Nothing) ->
            Eval.evalWithMetaInfo
    pTracedAdi desugared
{-# INLINABLE evalExprLoc #-}

-- | Evaluation with compile-time feature dispatch tied to environment config.
--
-- Unlike 'evalExprLoc' which does runtime dispatch, this function uses the
-- configuration from the environment type ('CtxCfg e') for compile-time
-- specialization. GHC eliminates branches for disabled features.
--
-- The 'HasEvalCfg e' constraint guarantees that stats and tracing
-- all use the same configuration - there's no risk of mismatch.
--
-- Example usage:
--
-- @
-- withEvalCfg (isEvalStats opts) (isTrace opts) $
--   \\(_ :: Proxy cfg) ->
--     runWithStoreEffectsIOT @cfg opts $
--       evalExprLocT expr  -- config inferred from environment
-- @
evalExprLocT
  :: forall e t f m
   . (MonadNix e t f m, HasEvalCfg e)
  => NExprLoc
  -> m (NValue t f m)
evalExprLocT expr =
  let
    -- Apply AST-level desugaring before evaluation
    desugared = desugarExprLoc expr
  in case singTrace @(CtxCfg e) of
    STrue ->
      -- Tracing enabled: use tracing evaluator
      join $ (`runReaderT` (0 :: Int)) $ evalWithTracingAndMetaInfo desugared
    SFalse -> case singStats @(CtxCfg e) of
      STrue -> do
        -- Stats enabled: use stats evaluator (still need runtime lookup for handle)
        mstats <- askEvalStats
        case mstats of
          Just s  -> evalWithStatsAndMetaInfo s desugared
          Nothing -> Eval.evalWithMetaInfo desugared  -- fallback
      SFalse ->
        -- Stats disabled: use plain evaluator (fastest path, zero overhead)
        Eval.evalWithMetaInfo desugared
{-# INLINABLE evalExprLocT #-}

exec :: (MonadNix e t f m, MonadInstantiate m) => Vector Text -> m (NValue t f m)
exec args = do
  res <- exec' args
  case res of
    Left err -> throwError err
    Right expr -> evalExprLoc expr

-- | Type-parameterized version of 'exec' with compile-time feature dispatch.
-- Uses 'CtxCfg e' from the environment for all config dispatch, ensuring
-- stats and tracing settings stay consistent.
execT
  :: forall e t f m
   . (MonadNix e t f m, MonadInstantiate m, HasEvalCfg e)
  => Vector Text
  -> m (NValue t f m)
execT args = do
  res <- exec' args
  case res of
    Left err -> throwError err
    Right expr -> evalExprLocT expr
{-# INLINABLE execT #-}

-- Please, delete `nix` from the name
nixInstantiateExpr
  :: (MonadNix e t f m, MonadInstantiate m) => Text -> m (NValue t f m)
nixInstantiateExpr s = do
  res <- instantiateExpr s
  case res of
    Left err -> throwError err
    Right expr -> evalExprLoc expr

-- | Type-parameterized version of 'nixInstantiateExpr' with compile-time feature dispatch.
-- Uses 'CtxCfg e' from the environment for all config dispatch, ensuring
-- stats and tracing settings stay consistent.
nixInstantiateExprT
  :: forall e t f m
   . (MonadNix e t f m, MonadInstantiate m, HasEvalCfg e)
  => Text
  -> m (NValue t f m)
nixInstantiateExprT s = do
  res <- instantiateExpr s
  case res of
    Left err -> throwError err
    Right expr -> evalExprLocT expr
{-# INLINABLE nixInstantiateExprT #-}
