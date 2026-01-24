{-# LANGUAGE RecordWildCards #-}

-- | Compilation monad and environment for NExpr to GHC Core translation.
--
-- The Compile monad provides:
-- * Access to a unique supply for generating fresh GHC Ids
-- * A reader environment with scope bindings and runtime references
-- * Source span tracking for error messages
module Nix.Compile.Monad
  ( -- * Compile monad
    Compile
  , runCompile
    -- * Compile environment
  , CompileEnv(..)
  , initCompileEnv
    -- * Scope operations
  , lookupVar
  , withScope
  , withBinding
    -- * Environment operations
  , getEnvId
  , withEnv
    -- * Fresh name generation
  , freshId
  , freshLocalId
    -- * Source span operations
  , getSpan
  , withSpan
    -- * Accessing refs
  , getRefs
    -- * Core construction helpers
  , mkVIntExpr
  , mkVFloatExpr
  , mkVBoolExpr
  , mkVNullExpr
  , mkVStringExpr
  , mkVListExpr
  , mkVAttrsExpr
  , callPrimop1
  , callPrimop2
  ) where

import Relude hiding (Type)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.Builtin.Types (trueDataConId, falseDataConId, consDataCon, nilDataCon)
import GHC.Core
import GHC.Core.Make (mkCoreApps, mkCoreConApps, mkCoreTup, mkNothingExpr, mkJustExpr, MkStringIds, mkStringExprFSWith)
import GHC.Core.Type
import GHC.Data.FastString (FastString, unpackFS, mkFastString)
import GHC.Driver.Session (DynFlags)
import GHC.Platform (Platform)
import GHC.Types.Id
import GHC.Types.Literal (mkLitString, mkLitDouble, mkLitInt)
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM (UniqFM)
import qualified GHC.Types.Unique.FM as UFM
import GHC.Types.Unique.Supply
import GHC.Types.Var
import Nix.Compile.Refs (RuntimeRefs(..))
import Nix.Types.VarName (VarName, varNameFS)

-- * Compile Environment

-- | Environment for compilation.
data CompileEnv = CompileEnv
  { ceEnvId :: !Id
    -- ^ The Id bound to the current NixEnv value (for 'with' scope lookup).
    -- Generated code passes this through to dynamic lookup calls.
  , ceScope :: !(UniqFM FastString Id)
    -- ^ Lexical scope: maps VarName (via FastString) to Core Ids.
    -- When we see NSym, we look up here first, then fall back to dynamic lookup.
  , ceRefs :: !RuntimeRefs
    -- ^ References to runtime library functions and data constructors.
    -- Pre-loaded at session start to avoid repeated lookups.
  , ceCurrentSpan :: !SrcSpan
    -- ^ Current source location for error messages.
  , ceDynFlags :: !DynFlags
    -- ^ GHC dynamic flags (needed for some Core operations).
  }

-- | Create initial compile environment.
initCompileEnv :: DynFlags -> RuntimeRefs -> Id -> CompileEnv
initCompileEnv dflags refs envId = CompileEnv
  { ceEnvId = envId
  , ceScope = UFM.emptyUFM
  , ceRefs = refs
  , ceCurrentSpan = noSrcSpan
  , ceDynFlags = dflags
  }

-- * Compile Monad

-- | The compilation monad.
-- * ReaderT for environment (scope, refs, source location)
-- * StateT for unique supply (fresh name generation)
-- * IO for any GHC operations that need it
newtype Compile a = Compile
  { unCompile :: ReaderT CompileEnv (StateT UniqSupply IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix)

-- | Run the compile monad.
runCompile :: Compile a -> CompileEnv -> UniqSupply -> IO a
runCompile (Compile m) env supply = evalStateT (runReaderT m env) supply

-- * Scope operations

-- | Look up a variable in the lexical scope.
-- Returns Nothing if not found (caller should then try dynamic lookup).
lookupVar :: VarName -> Compile (Maybe Id)
lookupVar name = Compile $ do
  scope <- asks ceScope
  pure $ UFM.lookupUFM scope (varNameFS name)

-- | Run computation with additional bindings in scope.
withScope :: [(VarName, Id)] -> Compile a -> Compile a
withScope bindings (Compile m) = Compile $ local addBindings m
  where
    addBindings env = env
      { ceScope = foldl' (\s (n, i) -> UFM.addToUFM s (varNameFS n) i) (ceScope env) bindings
      }

-- | Add a single binding to the scope.
withBinding :: VarName -> Id -> Compile a -> Compile a
withBinding name ident = withScope [(name, ident)]

-- * Environment operations

-- | Get the current NixEnv Id (for 'with' scope).
getEnvId :: Compile Id
getEnvId = Compile $ asks ceEnvId

-- | Run computation with a different NixEnv Id.
-- Used when entering a 'with' expression.
withEnv :: Id -> Compile a -> Compile a
withEnv envId (Compile m) = Compile $ local (\e -> e { ceEnvId = envId }) m

-- * Fresh name generation

-- | Generate a fresh Id with the given name and type.
freshId :: FastString -> Type -> Compile Id
freshId name ty = Compile $ do
  supply <- get
  let (uniq, supply') = takeUniqFromSupply supply
  put supply'
  -- Create a local Id (not exported, not a data constructor)
  let occName = mkVarOcc (unpackFS name)
      idName = mkInternalName uniq occName noSrcSpan
  pure $ mkLocalId idName ManyTy ty

-- | Generate a fresh local Id with a simple string name.
freshLocalId :: String -> Type -> Compile Id
freshLocalId name = freshId (mkFastString name)

-- * Source span operations

-- | Get the current source span.
getSpan :: Compile SrcSpan
getSpan = Compile $ asks ceCurrentSpan

-- | Run computation with a different source span.
withSpan :: SrcSpan -> Compile a -> Compile a
withSpan span (Compile m) = Compile $ local (\e -> e { ceCurrentSpan = span }) m

-- * Accessing refs

-- | Get runtime references.
getRefs :: Compile RuntimeRefs
getRefs = Compile $ asks ceRefs

-- * Core construction helpers

-- These helpers create Core expressions that wrap Haskell values in
-- the appropriate NixValue constructors.

-- | Create a VInt expression.
mkVIntExpr :: RuntimeRefs -> Integer -> CoreExpr
mkVIntExpr refs n = mkCoreConApps (refVIntCon refs) [Lit (mkLitInt (refPlatform refs) n)]

-- | Create a VFloat expression.
mkVFloatExpr :: RuntimeRefs -> Double -> CoreExpr
mkVFloatExpr refs n = mkCoreConApps (refVFloatCon refs) [mkDoubleLitExpr n]

-- | Create a VBool expression.
mkVBoolExpr :: RuntimeRefs -> Bool -> CoreExpr
mkVBoolExpr refs b = mkCoreConApps (refVBoolCon refs) [if b then mkTrueExpr (refDynFlags refs) else mkFalseExpr (refDynFlags refs)]

-- | Create a VNull expression.
mkVNullExpr :: RuntimeRefs -> CoreExpr
mkVNullExpr refs = Var (refVNullId refs)

-- | Create a VString expression (without context).
-- Uses mkText to convert the String literal to Text, matching the VString constructor's
-- expected type (Text, not String).
mkVStringExpr :: RuntimeRefs -> FastString -> CoreExpr
mkVStringExpr refs fs =
  mkCoreConApps (refVStringCon refs)
    [ mkCoreApps (Var (refMkTextId refs)) [mkFastStringLitWith (refMkStringIds refs) fs]
    , Var (refEmptyContextId refs)
    ]

-- | Create a VList expression from a list of element expressions.
-- Note: fromList has type `forall a. [a] -> Vector a`, so we must supply the
-- type argument explicitly in Core using `Type ty`.
mkVListExpr :: RuntimeRefs -> [CoreExpr] -> CoreExpr
mkVListExpr refs elems =
  -- Build: VList (V.fromList @NixValue [e1, e2, ...])
  mkCoreConApps (refVListCon refs)
    [ mkCoreApps (Var (refVFromListId refs))
        [ Type (refNixValueType refs)  -- Type argument for fromList
        , mkListExprLocal (refNixValueType refs) elems
        ]
    ]

-- | Create a VAttrs expression from key-value pair expressions.
mkVAttrsExpr :: RuntimeRefs -> CoreExpr -> CoreExpr
mkVAttrsExpr refs attrsExpr =
  mkCoreConApps (refVAttrsCon refs) [attrsExpr]

-- | Call a single-argument primop.
callPrimop1 :: Id -> CoreExpr -> CoreExpr
callPrimop1 primop arg = mkCoreApps (Var primop) [arg]

-- | Call a two-argument primop.
callPrimop2 :: Id -> CoreExpr -> CoreExpr -> CoreExpr
callPrimop2 primop arg1 arg2 = mkCoreApps (Var primop) [arg1, arg2]

-- Internal helpers

-- | Create a boxed String Core expression from a FastString.
-- Uses GHC's mkStringExprFSWith which properly wraps the string literal
-- with unpackCString# to produce a String ([Char]), not an Addr#.
mkFastStringLitWith :: MkStringIds -> FastString -> CoreExpr
mkFastStringLitWith stringIds = mkStringExprFSWith stringIds

mkDoubleLitExpr :: Double -> CoreExpr
mkDoubleLitExpr = Lit . mkLitDouble . toRational

mkTrueExpr :: DynFlags -> CoreExpr
mkTrueExpr _ = Var trueDataConId

mkFalseExpr :: DynFlags -> CoreExpr
mkFalseExpr _ = Var falseDataConId

mkListExprLocal :: Type -> [CoreExpr] -> CoreExpr
mkListExprLocal ty = foldr (\e acc -> mkCoreConApps consDataCon [Type ty, e, acc])
                           (mkCoreConApps nilDataCon [Type ty])
