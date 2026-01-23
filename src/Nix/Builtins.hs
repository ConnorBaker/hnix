{-# language CPP #-}
{-# language PartialTypeSignatures #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{-# options_ghc -fno-warn-name-shadowing #-}


-- | Code that implements Nix builtins. Lists the functions that are built into the Nix expression evaluator. Some built-ins (aka `derivation`), are always in the scope, so they can be accessed by the name. To keap the namespace clean, most built-ins are inside the `builtins` scope - a set that contains all what is a built-in.
module Nix.Builtins
  ( withNixContext
  , builtins
  )
where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Fix                       ( foldFix )
import qualified Nix.Core.AttrSet              as A
import qualified Data.Text                     as Text
import qualified Data.Time.Clock.POSIX         as Time
import           NeatInterpolation              ( text )
import           Nix.Atoms
import           Nix.Builtins.Arithmetic
import           Nix.Builtins.AttrSet
import           Nix.Builtins.Context
import           Nix.Builtins.Control
import           Nix.Builtins.Fetch
import           Nix.Builtins.Internal
import           Nix.Builtins.List
import           Nix.Builtins.Path
import           Nix.Builtins.String
import           Nix.Builtins.Type
import           Nix.Convert
import qualified Nix.Core.List                 as L
import           Nix.Effects
import           Nix.Effects.Basic              ( fetchTarball
                                                , fetchGit
                                                , fetchTree
                                                )
import           Nix.Exec
import           Nix.Expr.Types
import qualified Nix.Eval                      as Eval
import           Nix.Frames
import           Nix.Options
import           Nix.Parser
import           Nix.Scope
import           Nix.String
import           Nix.Value
import           Nix.Value.Interned             ( internedTrue, internedFalse, internedNull )
import           Nix.Types.VarName.Static       ( sBuiltins, sIncludes, sCurFile )
import           Nix.Value.Monad

-- This is a big module. There is recursive reuse:
-- @builtins -> builtinsList -> scopedImport -> withNixContext -> builtins@,
-- since @builtins@ is self-recursive: aka we ship @builtins.builtins.builtins...@.

-- ** Builtin functions

derivationNix
  :: forall e t f m. (MonadNix e t f m, Scoped (NValue t f m) m)
  => m (NValue t f m)
derivationNix = foldFix Eval.eval $$(do
    -- This is compiled in so that we only parse it once at compile-time.
    let Right expr = parseNixText [text|
      drvAttrs @ { outputs ? [ "out" ], ... }:

      let

        strict = derivationStrict drvAttrs;

        commonAttrs = drvAttrs
          // (builtins.listToAttrs outputsList)
          // { all = map (x: x.value) outputsList;
               inherit drvAttrs;
             };

        outputToAttrListElement = outputName:
          { name = outputName;
            value = commonAttrs // {
              outPath = builtins.getAttr outputName strict;
              drvPath = strict.drvPath;
              type = "derivation";
              inherit outputName;
            };
          };

        outputsList = map outputToAttrListElement outputs;

      in (builtins.head outputsList).value|]
    [|| expr ||]
  )

unsafeGetAttrPosNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
unsafeGetAttrPosNix nvX nvY =
  do
    x <- demand nvX
    y <- demand nvY

    case (x, y) of
      (NVStr ns, NVSet apos _) ->
        case A.lookup (mkVarName $ ignoreContext ns) apos of
          Nothing -> pure internedNull
          Just v -> toValue v
      _xy -> throwError $ ErrorCall $ "Invalid types for builtins.unsafeGetAttrPosNix: " <> show _xy

builtinsBuiltinNix
  :: forall e t f m
   . MonadNix e t f m
  => m (NValue t f m)
builtinsBuiltinNix = throwError $ ErrorCall "HNix does not provide builtins.builtins at the moment. Using builtins directly should be preferred"

-- | Wrapper for scopedImportNix that supplies withNixContext.
-- This breaks the circular dependency: Path.hs defines the logic,
-- Builtins.hs wires it up with withNixContext.
scopedImportNix
  :: forall e t f m
   . (MonadNix e t f m)
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
scopedImportNix = scopedImportNixWith withNixContext

-- | Wrapper for importNix that supplies withNixContext.
importNix
  :: forall e t f m . (MonadNix e t f m) => NValue t f m -> m (NValue t f m)
importNix = importNixWith withNixContext

currentSystemNix :: MonadNix e t f m => m (NValue t f m)
currentSystemNix =
  do
    os   <- getCurrentSystemOS
    arch <- getCurrentSystemArch

    pure $ mkNVStrWithoutContext $ arch <> "-" <> os

currentTimeNix :: MonadNix e t f m => m (NValue t f m)
currentTimeNix =
  do
    opts <- askOptions
    toValue @Integer $ round $ Time.utcTimeToPOSIXSeconds $ getTime opts

derivationStrictNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
derivationStrictNix = derivationStrict

getRecursiveSizeNix :: (MonadIntrospect m, NVConstraint f) => a -> m (NValue t f m)
getRecursiveSizeNix = fmap (NVConstant . NInt . fromIntegral) . recursiveSize

nixVersionNix :: MonadNix e t f m => m (NValue t f m)
nixVersionNix = toValue $ mkNixStringWithoutContext "2.18"

langVersionNix :: MonadNix e t f m => m (NValue t f m)
langVersionNix = toValue (5 :: Int)

-- ** @builtinsList@

builtinsList :: forall e t f m . (MonadNix e t f m) => m [Builtin (NValue t f m)]
builtinsList =
  sequenceA
    [ add  TopLevel "abort"            throwNix -- for now
    , add  TopLevel "baseNameOf"       baseNameOfNix
    , add0 TopLevel "derivation"       derivationNix
    , add  TopLevel "derivationStrict" derivationStrictNix
    , add  TopLevel "dirOf"            dirOfNix
    , add  TopLevel "import"           importNix
    , add  TopLevel "isNull"           isNullNix
    , add2 TopLevel "map"              mapNix
    , add2 TopLevel "mapAttrs"         mapAttrsNix
    , add  TopLevel "placeholder"      placeHolderNix
    , add2 TopLevel "removeAttrs"      removeAttrsNix
    , add2 TopLevel "scopedImport"     scopedImportNix
    , add  TopLevel "throw"            throwNix
    , add  TopLevel "toString"         toStringNix
    , add2 TopLevel "trace"            traceNix
    , add0 Normal   "nixVersion"       nixVersionNix
    , add0 Normal   "langVersion"      langVersionNix
    , add2 Normal   "add"              addNix
    , add2 Normal   "addErrorContext"  addErrorContextNix
    , add  Normal   "addDrvOutputDependencies" addDrvOutputDependenciesNix
    , add2 Normal   "all"              allNix
    , add2 Normal   "any"              anyNix
    , add2 Normal   "appendContext"    appendContextNix
    , add  Normal   "attrNames"        attrNamesNix
    , add  Normal   "attrValues"       attrValuesNix
    , add2 Normal   "bitAnd"           bitAndNix
    , add2 Normal   "bitOr"            bitOrNix
    , add2 Normal   "bitXor"           bitXorNix
    , add0 Normal   "builtins"         builtinsBuiltinNix
    , add  Normal   "break"            breakNix
    , add2 Normal   "catAttrs"         catAttrsNix
    , add' Normal   "ceil"             (arity1 (ceiling @Double @Integer))
    , add2 Normal   "compareVersions"  compareVersionsNix
    , add  Normal   "convertHash"      convertHashNix
    , add  Normal   "concatLists"      concatListsNix
    , add2 Normal   "concatMap"        concatMapNix
    , add' Normal   "concatStringsSep" (arity2 intercalateNixString)
    , add0 Normal   "currentSystem"    currentSystemNix
    , add0 Normal   "currentTime"      currentTimeNix
    , add2 Normal   "deepSeq"          deepSeqNix
    , add2 Normal   "div"              divNix
    , add2 Normal   "elem"             elemNix
    , add2 Normal   "elemAt"           elemAtNix
    , add  Normal   "exec"             execNix
    , add0 Normal   "false"            (pure internedFalse)
    , add  Normal   "fetchGit"         fetchGit
    , add  Normal   "fetchTree"        fetchTree
    --, add  Normal   "fetchMercurial"   fetchMercurial
    , add  Normal   "fetchTarball"     fetchTarball
    , add  Normal   "fetchurl"         fetchurlNix
    , add2 Normal   "filter"           filterNix
    , add2 Normal   "filterSource"     filterSourceNix
    , add2 Normal   "findFile"         findFileNix
    , add' Normal   "floor"            (arity1 (floor @Double @Integer))
    , add3 Normal   "foldl'"           foldl'Nix
    , add  Normal   "fromJSON"         fromJSONNix
    , add  TopLevel "fromTOML"         fromTOMLNix
    , add  Normal   "functionArgs"     functionArgsNix
    , add  Normal   "genericClosure"   genericClosureNix
    , add2 Normal   "genList"          genListNix
    , add2 Normal   "getAttr"          getAttrNix
    , add  Normal   "getContext"       getContextNix
    , add  Normal   "getEnv"           getEnvNix
    , add2 Normal   "groupBy"          groupByNix
    , add2 Normal   "hasAttr"          hasAttrNix
    , add  Normal   "hasContext"       hasContextNix
    , add' Normal   "hashString"       (hashStringNix @e @t @f @m)
    , add' Normal   "hashFile"         hashFileNix
    , add  Normal   "head"             headNix
    , add2 Normal   "intersectAttrs"   intersectAttrsNix
    , add  Normal   "isAttrs"          isAttrsNix
    , add  Normal   "isBool"           isBoolNix
    , add  Normal   "isFloat"          isFloatNix
    , add  Normal   "isFunction"       isFunctionNix
    , add  Normal   "isInt"            isIntNix
    , add  Normal   "isList"           isListNix
    , add  Normal   "isString"         isStringNix
    , add  Normal   "isPath"           isPathNix
    , add  Normal   "length"           lengthNix
    , add2 Normal   "lessThan"         lessThanNix
    , add  Normal   "listToAttrs"      listToAttrsNix
    , add2 Normal   "match"            matchNix
    , add2 Normal   "mul"              mulNix
    , add0 Normal   "nixPath"          nixPathNix
    , add0 Normal   "null"             (pure internedNull)
    , add2 Normal   "outputOf"         outputOfNix
    , add  Normal   "parseDrvName"     parseDrvNameNix
    , add2 Normal   "partition"        partitionNix
    , add  Normal   "path"             pathNix
    , add  Normal   "pathExists"       pathExistsNix
    , add  Normal   "readDir"          readDirNix
    , add  Normal   "readFile"         readFileNix
    , add  Normal   "readFileType"     readFileTypeNix
    , add3 Normal   "replaceStrings"   replaceStringsNix
    , add2 Normal   "seq"              seqNix
    , add2 Normal   "sort"             sortNix
    , add2 Normal   "split"            splitNix
    , add  Normal   "splitVersion"     splitVersionNix
    , add0 Normal   "storeDir"         (mkNVStrWithoutContext . toText . getStoreDir <$> askOptions)
    , add  Normal   "storePath"        storePathNix
    , add' Normal   "stringLength"     (arity1 $ Text.length . ignoreContext)
    , add' Normal   "sub"              (arity2 ((-) @Integer))
    , add' Normal   "substring"        substringNix
    , add  Normal   "tail"             tailNix
    , add2 Normal   "toFile"           toFileNix
    , add  Normal   "toJSON"           toJSONNix
    , add  Normal   "toPath"           toPathNix -- Deprecated in Nix: https://github.com/NixOS/nix/pull/2524
    , add  Normal   "toXML"            toXMLNix
    , add2 Normal   "traceVerbose"     traceVerboseNix
    , add0 Normal   "true"             (pure internedTrue)
    , add  Normal   "tryEval"          tryEvalNix
    , add  Normal   "typeOf"           typeOfNix
    , add  Normal   "unsafeDiscardOutputDependency" unsafeDiscardOutputDependencyNix
    , add  Normal   "unsafeDiscardStringContext"    unsafeDiscardStringContextNix
    , add2 Normal   "unsafeGetAttrPos"              unsafeGetAttrPosNix
    , add  Normal   "valueSize"        getRecursiveSizeNix
    , add2 Normal   "warn"             warnNix
    , add2 Normal   "zipAttrsWith"     zipAttrsWithNix
    ]
 where

  arity0 :: a -> Prim m a
  arity0 = Prim . pure

  arity1 :: (a -> b) -> (a -> Prim m b)
  arity1 g = arity0 . g

  arity2 :: (a -> b -> c) -> (a -> b -> Prim m c)
  arity2 f = arity1 . f

  mkBuiltin :: BuiltinType -> VarName -> m (NValue t f m) -> m (Builtin (NValue t f m))
  mkBuiltin t n v = wrap t n <$> mkThunk n v
   where
    wrap :: BuiltinType -> VarName -> v -> Builtin v
    wrap t n f = Builtin t (n, f)

    mkThunk :: VarName -> m (NValue t f m) -> m (NValue t f m)
    mkThunk n = defer . withFrame Info (ErrorCall $ "While calling builtin " <> toString n <> "\n")

  hAdd
    :: ( VarName
      -> fun
      -> m (NValue t f m)
      )
    -> BuiltinType
    -> VarName
    -> fun
    -> m (Builtin (NValue t f m))
  hAdd f t n v = mkBuiltin t n $ f n v

  add0
    :: BuiltinType
    -> VarName
    -> m (NValue t f m)
    -> m (Builtin (NValue t f m))
  add0 = hAdd (\ _ x -> x)

  add
    :: BuiltinType
    -> VarName
    -> ( NValue t f m
      -> m (NValue t f m)
      )
    -> m (Builtin (NValue t f m))
  add = hAdd builtin

  add2
    :: BuiltinType
    -> VarName
    -> ( NValue t f m
      -> NValue t f m
      -> m (NValue t f m)
      )
    -> m (Builtin (NValue t f m))
  add2 = hAdd builtin2

  add3
    :: BuiltinType
    -> VarName
    -> ( NValue t f m
      -> NValue t f m
      -> NValue t f m
      -> m (NValue t f m)
      )
    -> m (Builtin (NValue t f m))
  add3 = hAdd builtin3

  add'
    :: ToBuiltin t f m a
    => BuiltinType
    -> VarName
    -> a
    -> m (Builtin (NValue t f m))
  add' = hAdd (toBuiltin . varNameText)


-- * Exported

-- | Evaluate expression in the default context.
withNixContext
  :: forall e t f m r
   . (MonadNix e t f m, Has e Options)
  => Maybe Path
  -> m r
  -> m r
withNixContext mpath action =
  do
    base <- builtins
    opts <- askOptions

    pushScope
      (one (sIncludes, NVList $ L.fromList $ mkNVStrWithoutContext . fromString . coerce <$> getInclude opts))
      (pushScopes
        base $
        case mpath of
          Nothing -> action
          Just path -> do
            traceM $ "Setting __cur_file = " <> show path
            pushScope (one (sCurFile, NVPath path)) action
      )

builtins
  :: forall e t f m
  . ( MonadNix e t f m
     , Scoped (NValue t f m) m
     )
  => m (Scopes m (NValue t f m))
builtins =
  do
    ref <- defer $ NVSet emptyPositionSet <$> buildMap
    (`pushScope` askScopes) . coerce . A.fromList . ((sBuiltins, ref) :) =<< topLevelBuiltins
 where
  buildMap :: m (AttrSet (NValue t f m))
  buildMap         =  A.fromList . (mapping <$>) <$> builtinsList

  topLevelBuiltins :: m [(VarName, NValue t f m)]
  topLevelBuiltins = mapping <<$>> fullBuiltinsList

  fullBuiltinsList :: m [Builtin (NValue t f m)]
  fullBuiltinsList = nameBuiltins <<$>> builtinsList
   where
    nameBuiltins :: Builtin v -> Builtin v
    nameBuiltins b@(Builtin TopLevel _) = b
    nameBuiltins (Builtin Normal nB) =
      Builtin TopLevel $ first (\n -> mkVarName ("__" <> varNameText n)) nB
