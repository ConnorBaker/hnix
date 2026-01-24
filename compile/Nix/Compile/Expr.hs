{-# LANGUAGE RecordWildCards #-}

-- | NExpr to GHC Core compiler.
--
-- This module implements the core compilation logic that translates
-- Nix expressions (NExpr) to GHC Core expressions. The generated Core
-- manipulates NixValue types and calls primops/builtins from the runtime.
--
-- Compilation strategy:
-- * Literals become constructor applications (VInt n, VBool b, etc.)
-- * Variables look up in lexical scope first, then dynamic 'with' scopes
-- * Let bindings become Core let expressions (recursive by default in Nix)
-- * Functions become lambdas wrapped in VClosure
-- * Attribute sets build NixAttrs using runtime functions
-- * Operators become primop calls
-- * Short-circuit operators (&&, ||, ->) become if-then-else
module Nix.Compile.Expr
  ( -- * Main compilation function
    compileExpr
  , compileExprLoc
    -- * Individual constructors (exported for testing)
  , compileAtom
  , compileVar
  , compileString
  , compileList
  , compileAttrSet
  , compileLambda
  , compileApp
  , compileLet
  , compileIf
  , compileWith
  , compileSelect
  , compileHasAttr
  , compileBinary
  , compileUnary
  , compileAssert
  , compileLiteralPath
  , compileEnvPath
  ) where

import Relude hiding (empty, Type, Alt)
import Control.Monad (foldM)
import Data.List (foldl1', partition)
import Data.Fix (Fix(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import GHC.Core
import GHC.Core.Make (mkCoreApps, mkCoreConApps, mkCoreTup, mkNothingExpr, mkJustExpr, mkStringExprFSWith)
import GHC.Builtin.Types (boolTy, trueDataCon, trueDataConId, falseDataCon, falseDataConId, unitDataCon, unitTy, nilDataCon, consDataCon, mkBoxedTupleTy)
import GHC.Core.Utils (exprType)
import GHC.Types.Id
import GHC.Types.Var
import GHC.Core.Type (Type)
import GHC.Data.FastString (FastString, mkFastString)
import GHC.Types.Literal ()

import Nix.Atoms (NAtom(..))
import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc, stripAnnotation)
import Nix.Utils (Path)
import Nix.Compile.Monad
import Nix.Compile.Refs

-- * Main compilation entry point

-- | Compile an NExpr to GHC Core.
--
-- The resulting CoreExpr has type NixValue.
compileExpr :: NExpr -> Compile CoreExpr
compileExpr (Fix nf) = case nf of
  NConstant atom -> compileAtom atom
  NSym name -> compileVar name
  NStr nstring -> compileString nstring
  NList exprs -> compileList exprs
  NSet recur bindings -> compileAttrSet recur bindings
  NLiteralPath p -> compileLiteralPath p
  NEnvPath p -> compileEnvPath p
  NPath nstring -> compilePathInterp nstring
  NApp fn arg -> compileApp fn arg
  NUnary op e -> compileUnary op e
  NBinary op l r -> compileBinary op l r
  NSelect mdef set path -> compileSelect mdef set path
  NHasAttr set path -> compileHasAttr set path
  NAbs params body -> compileLambda params body
  NLet bindings body -> compileLet bindings body
  NIf cond th el -> compileIf cond th el
  NWith set body -> compileWith set body
  NAssert cond body -> compileAssert cond body
  NSynHole name -> compileHole name

-- | Compile a located expression (strips source location annotation).
compileExprLoc :: NExprLoc -> Compile CoreExpr
compileExprLoc = compileExpr . stripAnnotation

-- * Individual compilation rules

-- | Compile an atomic literal.
compileAtom :: NAtom -> Compile CoreExpr
compileAtom atom = do
  refs <- getRefs
  pure $ case atom of
    NInt n -> mkVIntExpr refs (fromIntegral n)
    NFloat n -> mkVFloatExpr refs n
    NBool b -> mkVBoolExpr refs b
    NNull -> mkVNullExpr refs
    NURI t -> mkVStringExprText refs t  -- URIs become strings

-- | Compile a variable reference.
--
-- First looks up in lexical scope. If not found, checks for global builtins
-- (like `builtins`, `toString`, `map`, etc.) then generates code to look up
-- in the dynamic 'with' scope chain.
compileVar :: VarName -> Compile CoreExpr
compileVar name = do
  refs <- getRefs
  mId <- lookupVar name

  case mId of
    -- Found in lexical scope - just reference the Id
    Just ident -> pure $ Var ident

    -- Not in lexical scope - check for global builtins or generate dynamic lookup
    Nothing -> do
      -- Check for global builtins (exposed at top level without builtins. prefix)
      case lookupGlobalBuiltin refs name of
        Just builtinId -> pure $ Var builtinId
        Nothing
          -- Special case: import requires NixEnv to resolve relative paths
          | name == mkVarName "import" -> do
              envId <- getEnvId
              -- Generate: builtinImport envId
              -- builtinImport :: NixEnv -> NixValue, returns a VBuiltin
              pure $ mkCoreApps (Var (refGlobalImportId refs)) [Var envId]
          | otherwise -> do
              envId <- getEnvId
              -- Generate dynamic lookup: lookupWithScopesOrThrow name env
              -- This throws UndefinedVariable if not found in any 'with' scope
              let nameExpr = mkVarNameExpr refs name
              pure $ mkCoreApps (Var (refLookupWithScopesId refs)) [nameExpr, Var envId]

-- | Look up a global builtin by name.
-- These are builtins that Nix exposes at the top level in addition to
-- being available under the builtins attribute set.
--
-- In Nix, the following builtins are available globally:
-- abort, baseNameOf, dirOf, false, isNull, map, null,
-- removeAttrs, throw, toString, trace, true, builtins
lookupGlobalBuiltin :: RuntimeRefs -> VarName -> Maybe Id
lookupGlobalBuiltin refs name
  | name == mkVarName "builtins"    = Just $ refBuiltinsAttrSetId refs
  | name == mkVarName "abort"       = Just $ refGlobalAbortId refs
  | name == mkVarName "baseNameOf"  = Just $ refGlobalBaseNameOfId refs
  | name == mkVarName "dirOf"       = Just $ refGlobalDirOfId refs
  | name == mkVarName "false"       = Just $ refGlobalFalseId refs
  | name == mkVarName "isNull"      = Just $ refGlobalIsNullId refs
  | name == mkVarName "map"         = Just $ refGlobalMapId refs
  | name == mkVarName "null"        = Just $ refGlobalNullId refs
  | name == mkVarName "removeAttrs" = Just $ refGlobalRemoveAttrsId refs
  | name == mkVarName "throw"       = Just $ refGlobalThrowId refs
  | name == mkVarName "toString"    = Just $ refGlobalToStringId refs
  | name == mkVarName "trace"       = Just $ refGlobalTraceId refs
  | name == mkVarName "true"        = Just $ refGlobalTrueId refs
  | otherwise                       = Nothing

-- | Compile a string literal with possible interpolations.
compileString :: NString NExpr -> Compile CoreExpr
compileString (DoubleQuoted parts) = compileParts parts
compileString (Indented _ parts) = compileParts parts
  -- Note: Indentation stripping happens at parse time, so we just compile the parts

-- | Compile string parts (plain text and antiquotes).
compileParts :: [Antiquoted Text NExpr] -> Compile CoreExpr
compileParts [] = do
  refs <- getRefs
  pure $ mkVStringExprText refs ""
compileParts [Plain t] = do
  refs <- getRefs
  pure $ mkVStringExprText refs t
compileParts parts = do
  refs <- getRefs
  -- Compile each part and concatenate
  compiled <- traverse compilePart parts
  -- Fold with string concatenation
  pure $ foldl1' (\acc e -> callPrimop2 (refNixAddId refs) acc e) compiled
  where
    compilePart :: Antiquoted Text NExpr -> Compile CoreExpr
    compilePart (Plain t) = do
      refs <- getRefs
      pure $ mkVStringExprText refs t
    compilePart EscapedNewline = do
      refs <- getRefs
      pure $ mkVStringExprText refs "\n"
    compilePart (Antiquoted e) = do
      refs <- getRefs
      e' <- compileExpr e
      -- Coerce the interpolated expression to string
      pure $ callPrimop1 (refNixCoerceToStringId refs) e'

-- | Compile a list literal.
compileList :: [NExpr] -> Compile CoreExpr
compileList elems = do
  refs <- getRefs
  compiled <- traverse compileExpr elems
  pure $ mkVListExpr refs compiled

-- | Compile an attribute set.
compileAttrSet :: Recursivity -> [Binding NExpr] -> Compile CoreExpr
compileAttrSet NonRecursive bindings = compileNonRecAttrSet bindings
compileAttrSet Recursive bindings = compileRecAttrSet bindings

-- | Compile a non-recursive attribute set.
compileNonRecAttrSet :: [Binding NExpr] -> Compile CoreExpr
compileNonRecAttrSet bindings = do
  refs <- getRefs
  -- Compile each binding
  compiledBindings <- traverse compileBinding bindings
  -- Build attribute set by processing each compiled binding
  let empty = Var (refEmptyAttrsId refs)
  attrsExpr <- foldM (insertCompiledBinding refs) empty compiledBindings
  pure $ mkVAttrsExpr refs attrsExpr

-- | Insert a compiled binding into an attribute set expression.
insertCompiledBinding :: RuntimeRefs -> CoreExpr -> CompiledBinding -> Compile CoreExpr
insertCompiledBinding refs acc (SimplePairs pairs) =
  -- Simple key-value pairs: use insertAttr
  pure $ foldr (\(k, v) accExpr ->
    mkCoreApps (Var (refInsertAttrId refs)) [k, v, accExpr]) acc pairs
insertCompiledBinding refs acc (DynamicPath pathVec val) =
  -- Dynamic path: use insertAtPathDynamic
  -- insertAtPathDynamic :: Vector NixValue -> NixValue -> NixAttrs -> NixAttrs
  pure $ mkCoreApps (Var (refInsertAtPathDynamicId refs)) [pathVec, val, acc]

-- | Compile a recursive attribute set.
compileRecAttrSet :: [Binding NExpr] -> Compile CoreExpr
compileRecAttrSet bindings = do
  refs <- getRefs

  -- Partition: inherit bindings vs regular bindings
  let (inheritBinds, regularBinds) = partition isInheritBinding bindings

  -- Compile inherit values NOW (in outer scope, before rec extends scope)
  inheritCompiled <- compileInheritBindings refs inheritBinds

  -- Extract simple bindings from regular bindings only
  let simpleBinds = extractSimpleBindings regularBinds

  -- Create fresh Ids for ALL names (regular + inherited)
  let allNames = map fst simpleBinds ++ map fst inheritCompiled
  idPairs <- forM allNames $ \name -> do
    ident <- freshId (varNameFS name) (refNixValueType refs)
    pure (name, ident)

  -- Split idPairs for regular vs inherited
  let regularIdPairs = take (length simpleBinds) idPairs
      inheritIdPairs = drop (length simpleBinds) idPairs

  -- Compile RHSs for regular bindings in extended scope (so they can reference each other)
  rhss <- withScope idPairs $ forM simpleBinds $ \(_, expr) -> compileExpr expr

  -- Build the letrec bindings
  -- Regular bindings: compiled in extended scope
  -- Inherit bindings: already compiled in outer scope
  let regularCoreBinds = zipWith (\(_, ident) rhs -> (ident, rhs)) regularIdPairs rhss
      inheritCoreBinds = zipWith (\(_, ident) (_, valExpr) -> (ident, valExpr)) inheritIdPairs inheritCompiled
      allCoreBinds = regularCoreBinds ++ inheritCoreBinds
      recBind = Rec allCoreBinds

  -- Build the attribute set from the bound Ids
  let attrPairs = [(mkVarNameExpr refs name, Var ident) | (name, ident) <- idPairs]
      empty = Var (refEmptyAttrsId refs)
      attrsExpr = foldr (\(k, v) acc ->
        mkCoreApps (Var (refInsertAttrId refs)) [k, v, acc]) empty attrPairs

  pure $ Let recBind $ mkVAttrsExpr refs attrsExpr

-- | Extract simple bindings (name = expr) from a binding list.
-- Handles nested paths by desugaring: { a.b = 1; a.c = 2; } -> { a = { b = 1; c = 2; }; }
-- Skips inherit bindings (those are handled separately in compileRecAttrSet).
extractSimpleBindings :: [Binding NExpr] -> [(VarName, NExpr)]
extractSimpleBindings bindings =
  let namedBinds = [(path, e) | NamedVar path e _ <- bindings]
      -- Group by first key
      grouped = groupByFirstKey namedBinds
  in map desugarGroup grouped

-- | Group bindings by their first key.
-- Returns [(firstKey, [(Maybe restPath, expr)])] where Nothing means single key.
groupByFirstKey :: [(NAttrPath NExpr, NExpr)] -> [(VarName, [(Maybe (NAttrPath NExpr), NExpr)])]
groupByFirstKey binds =
  let -- Group using HashMap for efficiency
      grouped :: HM.HashMap VarName [(Maybe (NAttrPath NExpr), NExpr)]
      grouped = foldr insertBind HM.empty binds

      insertBind :: (NAttrPath NExpr, NExpr) -> HM.HashMap VarName [(Maybe (NAttrPath NExpr), NExpr)] -> HM.HashMap VarName [(Maybe (NAttrPath NExpr), NExpr)]
      insertBind (path, expr) acc =
        let firstKey = case NE.head path of
              StaticKey k -> k
              DynamicKey _ -> error "Dynamic keys in rec/let first position not supported"
            restPath = case NE.tail path of
              [] -> Nothing  -- Single key
              (k:ks) -> Just (k :| ks)  -- Has more keys
        in HM.insertWith (++) firstKey [(restPath, expr)] acc
  in HM.toList grouped

-- | Desugar a group of bindings with the same first key.
-- If there's only one binding with no rest path, return it directly.
-- Otherwise, build a nested NSet.
desugarGroup :: (VarName, [(Maybe (NAttrPath NExpr), NExpr)]) -> (VarName, NExpr)
desugarGroup (key, [(Nothing, expr)]) = (key, expr)  -- Single complete binding
desugarGroup (key, subBinds) =
  -- Build nested set: { subpath1 = expr1; subpath2 = expr2; ... }
  let nestedBindings = [NamedVar path expr nullPos | (Just path, expr) <- subBinds]
      -- Handle the case where there's a direct assignment AND nested paths
      -- e.g., { a = 1; a.b = 2; } is an error in Nix
      directBinds = [e | (Nothing, e) <- subBinds]
  in case directBinds of
    [] -> (key, Fix (NSet NonRecursive nestedBindings))
    [_] | null nestedBindings -> error "desugarGroup: impossible - should be caught earlier"
    [_] -> error $ "Attribute '" <> show key <> "' has both direct value and nested paths"
    _ -> error $ "Attribute '" <> show key <> "' defined multiple times"

-- | Check if a binding is an inherit (simple or from scope).
isInheritBinding :: Binding r -> Bool
isInheritBinding (Inherit _ _ _) = True
isInheritBinding (NamedVar _ _ _) = False

-- | Compile inherit bindings in the current (outer) scope.
-- Returns [(VarName, CoreExpr)] pairs for each inherited name.
compileInheritBindings :: RuntimeRefs -> [Binding NExpr] -> Compile [(VarName, CoreExpr)]
compileInheritBindings refs = fmap concat . traverse go
  where
    go :: Binding NExpr -> Compile [(VarName, CoreExpr)]
    go (Inherit Nothing names _) = forM names $ \keyName -> do
      let varName = staticKeyName keyName
      -- Compile reference in current (outer) scope
      valExpr <- compileVar varName
      pure (varName, valExpr)
    go (Inherit (Just scopeExpr) names _) = do
      scope' <- compileExpr scopeExpr
      forM names $ \keyName -> do
        let varName = staticKeyName keyName
        let selectExpr = callPrimop2 (refNixSelectId refs)
                           (callPrimop1 (refExpectAttrsId refs) scope')
                           (mkVarNameExpr refs varName)
        pure (varName, selectExpr)
    go (NamedVar _ _ _) = pure []  -- Not an inherit, skip

-- | Check if a key name is static (not dynamic).
isStaticKey :: NKeyName r -> Bool
isStaticKey (StaticKey _) = True
isStaticKey (DynamicKey _) = False

-- | Build nested attribute set for static paths.
-- Given keys [b, c] and val, builds: VAttrs { b = VAttrs { c = val; }; }
buildNestedAttrs :: RuntimeRefs -> [NKeyName NExpr] -> CoreExpr -> CoreExpr
buildNestedAttrs _ [] val = val
buildNestedAttrs refs (StaticKey k : ks) val =
  let inner = buildNestedAttrs refs ks val
      empty = Var (refEmptyAttrsId refs)
      inserted = mkCoreApps (Var (refInsertAttrId refs))
                   [mkVarNameExpr refs k, inner, empty]
  in mkVAttrsExpr refs inserted
buildNestedAttrs _ (DynamicKey _ : _) _ =
  error "buildNestedAttrs: called with dynamic key (should use compileDynamicPathBinding)"

-- | Result of compiling a binding - either simple (key, val) pairs or a dynamic path.
data CompiledBinding
  = SimplePairs ![(CoreExpr, CoreExpr)]
    -- ^ Simple key-value pairs that can be inserted with insertAttr
  | DynamicPath !CoreExpr !CoreExpr
    -- ^ Dynamic path: (pathVector :: Vector NixValue, value :: NixValue)
    -- Requires insertAtPathDynamic at runtime

-- | Compile a binding to either simple key-value pairs or a dynamic path.
compileBinding :: Binding NExpr -> Compile CompiledBinding
compileBinding (NamedVar path expr _pos) = do
  refs <- getRefs
  expr' <- compileExpr expr
  case NE.toList path of
    [StaticKey k] ->
      -- Single key: simple case
      pure $ SimplePairs [(mkVarNameExpr refs k, expr')]
    [DynamicKey antiquoted] -> do
      -- Single dynamic key: compile to VarName at runtime
      keyExpr <- compileAntiquotedToValue refs antiquoted
      let keyVarName = mkCoreApps (Var (refMkVarNameFromValueId refs)) [keyExpr]
      pure $ SimplePairs [(keyVarName, expr')]
    keys | all isStaticKey keys ->
      -- All static keys: build nested attrset at compile time
      -- { a.b.c = 1; } becomes { a = { b = { c = 1; }; }; }
      case keys of
        (StaticKey firstKey : restKeys) ->
          let nestedVal = buildNestedAttrs refs restKeys expr'
          in pure $ SimplePairs [(mkVarNameExpr refs firstKey, nestedVal)]
        _ -> error "compileBinding: empty path (impossible)"
    keys ->
      -- Has dynamic keys: use insertAtPathDynamic at runtime
      compileDynamicPathBinding refs keys expr'
compileBinding (Inherit mScope names _pos) = do
  refs <- getRefs
  pairs <- case mScope of
    Nothing -> do
      -- inherit a b c; means a = a; b = b; c = c; (from outer scope)
      forM names $ \keyName -> do
        let varName = staticKeyName keyName
        varExpr <- compileVar varName
        pure (mkVarNameExpr refs varName, varExpr)

    Just scopeExpr -> do
      -- inherit (scope) a b c; means a = scope.a; b = scope.b; etc.
      scope' <- compileExpr scopeExpr
      forM names $ \keyName -> do
        let varName = staticKeyName keyName
        let selectExpr = callPrimop2 (refNixSelectId refs)
                           (callPrimop1 (refExpectAttrsId refs) scope')
                           (mkVarNameExpr refs varName)
        pure (mkVarNameExpr refs varName, selectExpr)
  pure $ SimplePairs pairs

-- | Compile a path with dynamic keys to a DynamicPath binding.
-- Builds a Vector NixValue containing the path elements.
compileDynamicPathBinding :: RuntimeRefs -> [NKeyName NExpr] -> CoreExpr -> Compile CompiledBinding
compileDynamicPathBinding refs keys val = do
  -- Compile each key to a NixValue (VString for static, expression for dynamic)
  keyExprs <- traverse (compileKeyToValue refs) keys
  -- Build the path vector: V.fromList @NixValue [key1, key2, ...]
  let pathVec = mkVListExprSimple refs keyExprs
  pure $ DynamicPath pathVec val

-- | Compile a key name to a NixValue (for use in path vectors).
compileKeyToValue :: RuntimeRefs -> NKeyName NExpr -> Compile CoreExpr
compileKeyToValue refs (StaticKey k) =
  -- Static key: create a VString from the VarName
  pure $ mkVStringExprText refs (varNameText k)
compileKeyToValue refs (DynamicKey antiquoted) =
  -- Dynamic key: compile the expression
  compileAntiquotedToValue refs antiquoted

-- | Compile an antiquoted value (used in dynamic keys).
compileAntiquotedToValue :: RuntimeRefs -> Antiquoted (NString NExpr) NExpr -> Compile CoreExpr
compileAntiquotedToValue _ (Antiquoted e) = compileExpr e
compileAntiquotedToValue _ (Plain nstr) = compileString nstr
compileAntiquotedToValue refs EscapedNewline = pure $ mkVStringExprText refs "\n"

-- | Build a list of NixValues (for path vectors).
-- Note: This creates a Haskell list that fromList will convert to Vector.
mkVListExprSimple :: RuntimeRefs -> [CoreExpr] -> CoreExpr
mkVListExprSimple refs elems =
  mkCoreApps (Var (refVFromListId refs))
    [ Type (refNixValueType refs)
    , mkListExprOf (refNixValueType refs) elems
    ]

-- | Build a Haskell list expression.
mkListExprOf :: Type -> [CoreExpr] -> CoreExpr
mkListExprOf ty = foldr (\e acc -> mkCoreConApps consDataCon [Type ty, e, acc])
                        (mkCoreConApps nilDataCon [Type ty])

-- | Get the VarName from a static key (dynamic keys not yet supported).
staticKeyName :: NKeyName NExpr -> VarName
staticKeyName (StaticKey k) = k
staticKeyName (DynamicKey _) = error "Dynamic keys not yet supported in compiler"

-- | Compile a lambda expression.
compileLambda :: Params NExpr -> NExpr -> Compile CoreExpr
compileLambda (Param name) body = do
  refs <- getRefs
  -- Simple parameter: x: body
  paramId <- freshId (varNameFS name) (refNixValueType refs)
  body' <- withBinding name paramId $ compileExpr body

  -- Wrap in VClosure
  let paramInfo = Param name  -- Parameter info for error messages
      lamExpr = Lam paramId body'

  pure $ mkCoreConApps (refVClosureCon refs)
    [ mkParamsExpr refs paramInfo
    , lamExpr
    ]

compileLambda (ParamSet mName variadic pset) body = do
  refs <- getRefs
  -- Pattern parameter: { a, b ? default, ... }@name: body
  argId <- freshId "arg" (refNixValueType refs)

  -- Extract parameters from the set
  let params = attrSetToList pset
      paramNames = map fst params

  -- Create Ids for each parameter
  paramIds <- forM paramNames $ \pname ->
    (pname,) <$> freshId (varNameFS pname) (refNixValueType refs)

  -- If there's an @name binding, add it to scope
  let scope = paramIds ++ maybe [] (\n -> [(n, argId)]) mName

  -- Build bindings: each param extracts from the argument set
  -- IMPORTANT: Compile default expressions WITH all params in scope,
  -- so defaults can reference other parameters (e.g., { x, y ? x + 1 }: ...)
  -- Since the bindings are Rec, lazy cross-references work correctly.
  let argAttrs = callPrimop1 (refExpectAttrsId refs) (Var argId)
  (paramBinds, body') <- withScope scope $ do
    binds <- forM (zip params paramIds) $ \((pname, mdef), (_, paramId)) -> do
      rhs <- case mdef of
        Nothing ->
          -- Required parameter: attr.name (throws if missing)
          pure $ callPrimop2 (refNixSelectId refs) argAttrs (mkVarNameExpr refs pname)
        Just defExpr -> do
          -- Optional parameter: attr.name or default
          def' <- compileExpr defExpr
          pure $ mkCoreApps (Var (refNixSelectOrId refs)) [argAttrs, mkVarNameExpr refs pname, def']
      pure (paramId, rhs)
    -- Compile body with all parameters in scope
    body'' <- compileExpr body
    pure (binds, body'')

  -- For closed patterns (without ...), generate validation that checks
  -- for unexpected keys in the argument set.
  -- nixCheckClosedPattern :: NixAttrs -> [VarName] -> ()
  -- It throws ThrownError if extra keys are found.
  bodyWithValidation <- case variadic of
    Closed -> do
      -- Generate: case nixCheckClosedPattern argAttrs [expected_names] of () -> body
      -- This validates that the argument set contains only expected keys
      let expectedNamesList = mkVarNameListExpr refs paramNames
          checkExpr = mkCoreApps (Var (refNixCheckClosedPatternId refs)) [argAttrs, expectedNamesList]
      mkSeqM checkExpr body'
    Variadic -> pure body'  -- No validation needed for variadic patterns

  -- Build the let expression with all parameter bindings
  let coreBinds = Rec paramBinds  -- Might need to be NonRec if no mutual deps
      bodyWithBinds = Let coreBinds bodyWithValidation
      lamExpr = Lam argId bodyWithBinds

  pure $ mkCoreConApps (refVClosureCon refs)
    [ mkParamsExpr refs (ParamSet Nothing variadic (fmap (fmap (const ())) pset))
    , lamExpr
    ]

attrSetToList :: ParamSet r -> [(VarName, Maybe r)]
attrSetToList = paramSetToSortedList

-- | Compile function application.
compileApp :: NExpr -> NExpr -> Compile CoreExpr
compileApp fn arg = do
  refs <- getRefs
  fn' <- compileExpr fn
  arg' <- compileExpr arg
  pure $ callPrimop2 (refNixApplyId refs) fn' arg'

-- | Compile a let expression.
compileLet :: [Binding NExpr] -> NExpr -> Compile CoreExpr
compileLet bindings body = do
  refs <- getRefs
  -- Let bindings are always recursive in Nix

  -- Partition: inherit bindings vs regular bindings
  let (inheritBinds, regularBinds) = partition isInheritBinding bindings

  -- Compile inherit values NOW (in outer scope, before let extends scope)
  inheritCompiled <- compileInheritBindings refs inheritBinds

  -- Extract simple bindings from regular bindings only
  let simpleBinds = extractSimpleBindings regularBinds

  -- Create fresh Ids for ALL names (regular + inherited)
  let allNames = map fst simpleBinds ++ map fst inheritCompiled
  idPairs <- forM allNames $ \name -> do
    ident <- freshId (varNameFS name) (refNixValueType refs)
    pure (name, ident)

  -- Split idPairs for regular vs inherited
  let regularIdPairs = take (length simpleBinds) idPairs
      inheritIdPairs = drop (length simpleBinds) idPairs

  -- Compile RHSs and body in extended scope
  (rhss, body') <- withScope idPairs $ do
    rhss <- forM simpleBinds $ \(_, expr) -> compileExpr expr
    body' <- compileExpr body
    pure (rhss, body')

  -- Build letrec with both regular and inherit bindings
  let regularCoreBinds = zipWith (\(_, ident) rhs -> (ident, rhs)) regularIdPairs rhss
      inheritCoreBinds = zipWith (\(_, ident) (_, valExpr) -> (ident, valExpr)) inheritIdPairs inheritCompiled
      allCoreBinds = regularCoreBinds ++ inheritCoreBinds

  pure $ Let (Rec allCoreBinds) body'

-- | Compile an if-then-else expression.
compileIf :: NExpr -> NExpr -> NExpr -> Compile CoreExpr
compileIf cond th el = do
  refs <- getRefs
  cond' <- compileExpr cond
  th' <- compileExpr th
  el' <- compileExpr el
  -- Extract the boolean value
  let condBool = callPrimop1 (refExpectBoolId refs) cond'
  mkIfThenElseM condBool th' el'

-- | Compile a with expression.
compileWith :: NExpr -> NExpr -> Compile CoreExpr
compileWith setExpr bodyExpr = do
  refs <- getRefs
  set' <- compileExpr setExpr
  envId <- getEnvId

  -- Create new env with pushed scope
  newEnvId <- freshLocalId "env" (refNixEnvType refs)
  let attrsExpr = callPrimop1 (refExpectAttrsId refs) set'
      pushExpr = mkCoreApps (Var (refPushWithScopeId refs)) [attrsExpr, Var envId]

  -- Compile body with new env
  body' <- withEnv newEnvId $ compileExpr bodyExpr

  pure $ Let (NonRec newEnvId pushExpr) body'

-- refNixEnvType is now provided by RuntimeRefs (removed local definition)

-- | Compile attribute selection.
compileSelect :: Maybe NExpr -> NExpr -> NAttrPath NExpr -> Compile CoreExpr
compileSelect mdef setExpr path = do
  refs <- getRefs
  set' <- compileExpr setExpr
  let attrsExpr = callPrimop1 (refExpectAttrsId refs) set'

  case mdef of
    Nothing -> do
      -- No default: throws on missing
      compileSelectPath refs attrsExpr (NE.toList path)

    Just defExpr -> do
      -- With default: return default if missing
      def' <- compileExpr defExpr
      compileSelectPathOr refs attrsExpr (NE.toList path) def'

-- | Compile selection along a path (a.b.c).
compileSelectPath :: RuntimeRefs -> CoreExpr -> [NKeyName NExpr] -> Compile CoreExpr
compileSelectPath _ attrs [] = pure attrs
compileSelectPath refs attrs (k:ks) = do
  keyExpr <- compileKeyName refs k
  let selected = callPrimop2 (refNixSelectId refs) attrs keyExpr
  case ks of
    [] -> pure selected
    _ -> compileSelectPath refs (callPrimop1 (refExpectAttrsId refs) selected) ks

-- | Compile selection with default.
-- For multi-level paths (a.b.c or default), we check if the full path exists
-- using hasAttr at each level, and if so select it, otherwise return the default.
compileSelectPathOr :: RuntimeRefs -> CoreExpr -> [NKeyName NExpr] -> CoreExpr -> Compile CoreExpr
compileSelectPathOr refs attrs path def = do
  case path of
    [k] -> do
      keyExpr <- compileKeyName refs k
      pure $ mkCoreApps (Var (refNixSelectOrId refs)) [attrs, keyExpr, def]
    (k:ks) -> do
      -- For multi-level paths, we recursively check and select:
      --   if nixHasAttr attrs k
      --   then let next = expectAttrs (nixSelect attrs k)
      --        in compileSelectPathOr next ks def
      --   else def
      -- Note: nixHasAttr returns NixValue (VBool), but mkIfThenElseM expects Bool,
      -- so we use expectBool to unwrap it.
      keyExpr <- compileKeyName refs k
      let hasAttrExpr = callPrimop2 (refNixHasAttrId refs) attrs keyExpr
      let hasAttrBool = callPrimop1 (refExpectBoolId refs) hasAttrExpr
      let selectExpr = callPrimop2 (refNixSelectId refs) attrs keyExpr
      let nextAttrs = callPrimop1 (refExpectAttrsId refs) selectExpr
      thenExpr <- compileSelectPathOr refs nextAttrs ks def
      mkIfThenElseM hasAttrBool thenExpr def
    [] -> pure def  -- Empty path just returns the default

-- | Compile a key name expression (now monadic to handle dynamic keys).
compileKeyName :: RuntimeRefs -> NKeyName NExpr -> Compile CoreExpr
compileKeyName refs (StaticKey k) = pure $ mkVarNameExpr refs k
compileKeyName refs (DynamicKey antiquoted) = do
  -- Compile the dynamic key expression and convert to VarName at runtime
  keyExpr <- case antiquoted of
    Antiquoted e -> compileExpr e
    Plain nstr -> compileString nstr
    EscapedNewline -> pure $ mkVStringExprText refs "\n"
  -- Call mkVarNameFromValue to convert NixValue -> VarName
  -- Note: This requires refMkVarNameFromValueId to be added to RuntimeRefs
  pure $ mkCoreApps (Var (refMkVarNameFromValueId refs)) [keyExpr]

-- | Compile hasAttr check.
-- For single-level paths, uses nixHasAttr directly.
-- For multi-level paths (a ? b.c.d), checks each level recursively.
compileHasAttr :: NExpr -> NAttrPath NExpr -> Compile CoreExpr
compileHasAttr setExpr path = do
  refs <- getRefs
  set' <- compileExpr setExpr
  let attrsExpr = callPrimop1 (refExpectAttrsId refs) set'
  compileHasAttrPath refs attrsExpr (NE.toList path)

-- | Compile hasAttr check for an attribute path.
-- Handles both single-level and multi-level paths by recursively checking
-- if each attribute exists and is an attrset (for intermediate levels).
compileHasAttrPath :: RuntimeRefs -> CoreExpr -> [NKeyName NExpr] -> Compile CoreExpr
compileHasAttrPath refs attrs path =
  case path of
    [] ->
      -- Empty path: always true (the set exists)
      pure $ mkVBoolExpr refs True
    [k] -> do
      -- Single key: just check if it exists
      keyExpr <- compileKeyName refs k
      pure $ callPrimop2 (refNixHasAttrId refs) attrs keyExpr
    (k:ks) -> do
      -- Multi-level: check if first key exists and is an attrset,
      -- then recursively check the rest of the path.
      --
      -- Nix semantics: `{a = 1;} ? a.b` returns false (not an error).
      -- We need to check if intermediate values are attrsets before recursing.
      --
      -- Generate:
      --   if expectBool (nixHasAttr attrs k)
      --   then let selected = nixSelect attrs k
      --        in if isAttrsPrimop selected
      --           then compileHasAttrPath (expectAttrs selected) ks
      --           else false
      --   else false
      --
      -- Note: nixHasAttr returns NixValue (VBool), but mkIfThenElseM expects Bool,
      -- so we use expectBool to unwrap it.
      keyExpr <- compileKeyName refs k
      let hasAttrExpr = callPrimop2 (refNixHasAttrId refs) attrs keyExpr
      let hasAttrBool = callPrimop1 (refExpectBoolId refs) hasAttrExpr
      let falseLit = mkVBoolExpr refs False
      let selectExpr = callPrimop2 (refNixSelectId refs) attrs keyExpr
      -- Check if selected value is an attrset before recursing
      let isAttrsCheck = callPrimop1 (refIsAttrsPrimopId refs) selectExpr
      let nextAttrs = callPrimop1 (refExpectAttrsId refs) selectExpr
      restExpr <- compileHasAttrPath refs nextAttrs ks
      -- Inner if: if isAttrs then recurse else false
      innerIfExpr <- mkIfThenElseM isAttrsCheck restExpr falseLit
      -- Outer if: if hasAttr then (inner check) else false
      mkIfThenElseM hasAttrBool innerIfExpr falseLit

-- | Compile a binary operation.
compileBinary :: NBinaryOp -> NExpr -> NExpr -> Compile CoreExpr
compileBinary op l r = do
  refs <- getRefs
  case op of
    -- Short-circuit operators become if-then-else
    NAnd -> do
      l' <- compileExpr l
      r' <- compileExpr r
      -- if expectBool l' then r' else VBool False
      let condBool = callPrimop1 (refExpectBoolId refs) l'
          falseLit = mkVBoolExpr refs False
      mkIfThenElseM condBool r' falseLit

    NOr -> do
      l' <- compileExpr l
      r' <- compileExpr r
      -- if expectBool l' then VBool True else r'
      let condBool = callPrimop1 (refExpectBoolId refs) l'
          trueLit = mkVBoolExpr refs True
      mkIfThenElseM condBool trueLit r'

    NImpl -> do
      l' <- compileExpr l
      r' <- compileExpr r
      -- if expectBool l' then r' else VBool True
      let condBool = callPrimop1 (refExpectBoolId refs) l'
          trueLit = mkVBoolExpr refs True
      mkIfThenElseM condBool r' trueLit

    -- All other operators evaluate both sides
    _ -> do
      l' <- compileExpr l
      r' <- compileExpr r
      let primop = getBinaryPrimop refs op
      pure $ callPrimop2 primop l' r'

-- | Get the primop Id for a binary operator.
getBinaryPrimop :: RuntimeRefs -> NBinaryOp -> Id
getBinaryPrimop refs = \case
  NEq -> refNixEqId refs
  NNEq -> refNixNEqId refs
  NLt -> refNixLtId refs
  NLte -> refNixLteId refs
  NGt -> refNixGtId refs
  NGte -> refNixGteId refs
  NAnd -> refNixAndId refs  -- Not used (short-circuited above)
  NOr -> refNixOrId refs    -- Not used
  NImpl -> refNixImplId refs -- Not used
  NUpdate -> refNixUpdateId refs
  NPlus -> refNixAddId refs
  NMinus -> refNixSubId refs
  NMult -> refNixMulId refs
  NDiv -> refNixDivId refs
  NConcat -> refNixListConcatId refs

-- | Compile a unary operation.
compileUnary :: NUnaryOp -> NExpr -> Compile CoreExpr
compileUnary op e = do
  refs <- getRefs
  e' <- compileExpr e
  let primop = case op of
        NNeg -> refNixNegId refs
        NNot -> refNixNotId refs
  pure $ callPrimop1 primop e'

-- | Compile an assertion.
compileAssert :: NExpr -> NExpr -> Compile CoreExpr
compileAssert cond body = do
  refs <- getRefs
  cond' <- compileExpr cond
  body' <- compileExpr body
  pure $ callPrimop2 (refNixAssertId refs) cond' body'

-- | Compile a literal path.
compileLiteralPath :: Path -> Compile CoreExpr
compileLiteralPath p = do
  refs <- getRefs
  pure $ mkCoreConApps (refVPathCon refs) [mkPathLit refs p]

-- | Compile an environment path (<nixpkgs>).
-- Environment paths are resolved at runtime via NIX_PATH lookup.
compileEnvPath :: Path -> Compile CoreExpr
compileEnvPath p = do
  refs <- getRefs
  envId <- getEnvId
  -- Convert the path to a VarName for the primop lookup
  -- The Path contains something like "nixpkgs" or "nixpkgs/lib"
  let pathName = mkVarName (toText p)
      nameExpr = mkVarNameExpr refs pathName
  -- Call: nixResolveEnvPath env varName
  pure $ mkCoreApps (Var (refNixResolveEnvPathId refs)) [Var envId, nameExpr]

-- | Compile a path with interpolations.
-- Path interpolation like ./foo/${bar} produces a path, not a string.
-- We compile the parts as strings, concatenate them, then convert to path.
compilePathInterp :: NString NExpr -> Compile CoreExpr
compilePathInterp nstring = do
  refs <- getRefs
  -- First compile as string (with all interpolations coerced to strings)
  strExpr <- compileString nstring
  -- Then convert the final result to a path using stringToPath
  pure $ callPrimop1 (refStringToPathId refs) strExpr

-- | Compile a syntactic hole (^name).
compileHole :: VarName -> Compile CoreExpr
compileHole name = do
  refs <- getRefs
  -- Holes throw an error when evaluated
  let msg = "Syntactic hole: " <> varNameText name
  pure $ callPrimop1 (refNixThrowId refs) (mkVStringExprText refs msg)

-- * Helper functions

-- | Create a VarName expression by calling the runtime mkVarNameStr helper.
-- This converts the VarName to a String literal, then calls mkVarNameStr
-- to properly construct a VarName at runtime.
mkVarNameExpr :: RuntimeRefs -> VarName -> CoreExpr
mkVarNameExpr refs name =
  mkCoreApps (Var (refMkVarNameStrId refs)) [mkStringExprFSWith (refMkStringIds refs) (varNameFS name)]

-- | Create a VString expression with proper Text construction.
-- Calls the runtime mkText helper to convert the String literal to Text.
mkVStringExprText :: RuntimeRefs -> Text -> CoreExpr
mkVStringExprText refs t = mkCoreConApps (refVStringCon refs)
  [ mkCoreApps (Var (refMkTextId refs)) [mkStringExprFSWith (refMkStringIds refs) (mkFastString (toString t))]
  , Var (refEmptyContextId refs)
  ]

-- | Encode Params as a RuntimeParams Core expression.
-- This constructs the appropriate RuntimeParams value for VClosure:
-- - Param name -> RuntimeParam name
-- - ParamSet mname variadic pset -> RuntimeParamSet mname variadic [(name, hasDefault)]
mkParamsExpr :: RuntimeRefs -> Params () -> CoreExpr
mkParamsExpr refs (Param name) =
  -- RuntimeParam !VarName
  mkCoreConApps (refRuntimeParamCon refs) [mkVarNameExpr refs name]

mkParamsExpr refs (ParamSet mName variadic pset) =
  -- RuntimeParamSet !(Maybe VarName) !RuntimeVariadic ![(VarName, Bool)]
  let mNameExpr = mkMaybeVarNameExpr refs mName
      variadicExpr = mkRuntimeVariadicExpr refs variadic
      paramListExpr = mkParamListExpr refs (paramSetToSortedList pset)
  in mkCoreConApps (refRuntimeParamSetCon refs) [mNameExpr, variadicExpr, paramListExpr]

-- | Convert Maybe VarName to Core expression for Maybe VarName.
mkMaybeVarNameExpr :: RuntimeRefs -> Maybe VarName -> CoreExpr
mkMaybeVarNameExpr refs = \case
  Nothing -> mkNothingExpr (refVarNameType refs)
  Just n  -> mkJustExpr (refVarNameType refs) (mkVarNameExpr refs n)

-- | Convert Variadic to Core expression for RuntimeVariadic.
mkRuntimeVariadicExpr :: RuntimeRefs -> Variadic -> CoreExpr
mkRuntimeVariadicExpr refs = \case
  Closed   -> mkCoreConApps (refRuntimeClosedCon refs) []
  Variadic -> mkCoreConApps (refRuntimeVariadicCon refs) []

-- | Convert [(VarName, Maybe ())] to Core expression for [(VarName, Bool)].
-- The Maybe () indicates whether a default is present (Just () = has default, Nothing = required).
mkParamListExpr :: RuntimeRefs -> [(VarName, Maybe ())] -> CoreExpr
mkParamListExpr refs params =
  let elemTy = mkPairType (refVarNameType refs) boolTy
      nil = mkCoreConApps nilDataCon [Type elemTy]
      cons x xs = mkCoreConApps consDataCon [Type elemTy, x, xs]
      mkPair name hasDefault =
        mkCoreTup [mkVarNameExpr refs name, mkBoolLit (isJust hasDefault)]
  in foldr (\(name, mdef) acc -> cons (mkPair name mdef) acc) nil params

-- | Create a pair type (a, b).
mkPairType :: Type -> Type -> Type
mkPairType a b = mkBoxedTupleTy [a, b]

-- | Create a Bool literal Core expression.
mkBoolLit :: Bool -> CoreExpr
mkBoolLit True  = Var trueDataConId
mkBoolLit False = Var falseDataConId

-- | Create a Path expression by calling the runtime mkPath helper.
-- This converts the Path to a String literal, then calls mkPath
-- to properly construct a Path at runtime.
mkPathLit :: RuntimeRefs -> Path -> CoreExpr
mkPathLit refs p =
  mkCoreApps (Var (refMkPathId refs)) [mkStringExprFSWith (refMkStringIds refs) (mkFastString (toString (toText p)))]

-- | Build an if-then-else Core expression.
-- This is equivalent to: case cond of { False -> elseExpr; True -> thenExpr }
mkIfThenElseM :: CoreExpr -> CoreExpr -> CoreExpr -> Compile CoreExpr
mkIfThenElseM cond thenExpr elseExpr = do
  -- Create a wild binder (unused case variable) using fresh unique
  wildBinder <- freshLocalId "wild" boolTy
  pure $ Case cond wildBinder (exprType thenExpr)
    [ Alt (DataAlt falseDataCon) [] elseExpr
    , Alt (DataAlt trueDataCon) [] thenExpr
    ]

-- | Create a list of VarName expressions for closed pattern validation.
-- Builds a Core expression representing [VarName] by calling mkVarNameStr for each name
-- and constructing a Haskell list using (:) and [].
mkVarNameListExpr :: RuntimeRefs -> [VarName] -> CoreExpr
mkVarNameListExpr refs names =
  let elemTy = refVarNameType refs
      -- Build [name1, name2, ...] as name1 : name2 : ... : []
      nil = mkCoreConApps nilDataCon [Type elemTy]
      cons x xs = mkCoreConApps consDataCon [Type elemTy, x, xs]
  in foldr (\name acc -> cons (mkVarNameExpr refs name) acc) nil names

-- | Sequence two expressions: evaluate the first for its side effect,
-- then return the second.
-- Equivalent to: case e1 of () -> e2
-- Since nixCheckClosedPattern returns (), we pattern match on () to sequence.
mkSeqM :: CoreExpr -> CoreExpr -> Compile CoreExpr
mkSeqM e1 e2 = do
  wildBinder <- freshLocalId "wild" unitTy
  pure $ Case e1 wildBinder (exprType e2)
    [ Alt (DataAlt unitDataCon) [] e2
    ]
