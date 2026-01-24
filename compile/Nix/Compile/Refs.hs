{-# LANGUAGE RecordWildCards #-}

-- | Runtime references for the NExpr to GHC Core compiler.
--
-- This module loads GHC Ids and DataCons for runtime library functions
-- at session initialization. The generated Core code calls these functions
-- directly, avoiding the overhead of name resolution at compile time.
--
-- Loading these refs requires a GHC session with the runtime modules loaded.
--
-- == Important: Module Import Requirements
--
-- For 'lookupType' to find identifiers, the module's interface file must be loaded
-- into the GHC session. This happens when the module is imported via 'setContext'
-- in "Nix.Compile.Driver".'initSession'.
--
-- __If you add a new module to 'loadRuntimeRefs', you MUST also add it to the
-- @runtimeImports@ list in "Nix.Compile.Driver".__
--
-- Without this, 'findModuleIO' will succeed (the module is discoverable), but
-- 'lookupType' will return 'Nothing' because the interface file was never loaded.
--
-- == Important: Module Lookup Semantics
--
-- The 'lookupId' function searches for identifiers by their __defining__ module,
-- NOT by re-exporting modules. This has critical implications for module organization:
--
-- * If @Nix.Compile.Value@ re-exports @emptyContext@ from @Nix.Compile.Value.Context@
--   using @module Nix.Compile.Value.Context@, the re-export works for compile-time
--   imports but NOT for runtime lookups.
--
-- * At runtime, @lookupId hsc "Nix.Compile.Value" "emptyContext"@ will FAIL because
--   @emptyContext@ is defined in @Nix.Compile.Value.Context@, not @Nix.Compile.Value@.
--
-- * You must use @lookupId hsc "Nix.Compile.Value.Context" "emptyContext"@ instead.
--
-- When splitting modules into submodules, ensure all lookups in 'loadRuntimeRefs'
-- reference the actual defining module. Use 'debugLoadRuntimeRefs' to diagnose
-- lookup failures - it prints which lookups succeed or fail.
module Nix.Compile.Refs
  ( -- * Runtime references
    RuntimeRefs(..)
  , loadRuntimeRefs
  , debugLoadRuntimeRefs
    -- * Module and type names
  , valueModuleName
  , contextModuleName
  , primopsModuleName
  , primopsIOModuleName
  , builtinsModuleName
  , nixValueTypeName
  , nixEnvTypeName
  ) where

import Relude hiding (Type)
import Control.Monad.Trans.Maybe (MaybeT(..))
import GHC
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.Make (MkStringIds(..), getMkStringIds)
import GHC.Core.TyCon (TyCon)
import GHC.Core.Type (mkTyConTy, splitFunTy, tyConAppTyCon_maybe)
import GHC.Driver.Env (hsc_dflags, hsc_NC, lookupType)
import GHC.Iface.Env (lookupNameCache)
import GHC.Platform (Platform)
import GHC.Types.Name (mkVarOcc, mkTcOcc, mkDataOcc, nameOccName, occNameString)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Unit.Finder (findImportedModule, FindResult(..))
import GHC.Unit.Module (ModuleName, mkModuleName, moduleNameString)
import GHC.Unit.Types (mkModule, unitString)
import GHC.Types.PkgQual (PkgQual(..))
import System.IO (hPutStrLn)

-- * Module Names
--
-- These must match the __defining__ modules for each identifier, not re-exporting
-- modules. See module header for details on why this matters.
--
-- When adding new submodules (e.g., splitting Primops.hs into Primops\/Arithmetic.hs),
-- add a new module name constant here and update the lookups in 'loadRuntimeRefs'
-- to use the correct defining module.

-- | Module containing NixValue type and constructors.
-- Also re-exports from Context and Error submodules for compile-time convenience,
-- but runtime lookups must use the actual defining modules.
valueModuleName :: ModuleName
valueModuleName = mkModuleName "Nix.Compile.Value"

-- | Module containing context types and operations (emptyContext, unionContext, etc.).
-- These are re-exported by Value.hs but defined here, so runtime lookups must use
-- this module name.
contextModuleName :: ModuleName
contextModuleName = mkModuleName "Nix.Compile.Value.Context"

-- | Module containing primitive operations (re-export hub).
-- Note: Functions are now defined in submodules; use the specific module names
-- for lookupId calls.
primopsModuleName :: ModuleName
primopsModuleName = mkModuleName "Nix.Compile.Primops"

-- | Primops submodule for type coercion.
primopsCoerceModuleName :: ModuleName
primopsCoerceModuleName = mkModuleName "Nix.Compile.Primops.Coerce"

-- | Primops submodule for arithmetic operations.
primopsArithmeticModuleName :: ModuleName
primopsArithmeticModuleName = mkModuleName "Nix.Compile.Primops.Arithmetic"

-- | Primops submodule for comparison operations.
primopsComparisonModuleName :: ModuleName
primopsComparisonModuleName = mkModuleName "Nix.Compile.Primops.Comparison"

-- | Primops submodule for logical operations.
primopsLogicalModuleName :: ModuleName
primopsLogicalModuleName = mkModuleName "Nix.Compile.Primops.Logical"

-- | Primops submodule for string operations.
primopsStringModuleName :: ModuleName
primopsStringModuleName = mkModuleName "Nix.Compile.Primops.String"

-- | Primops submodule for collection operations.
primopsCollectionModuleName :: ModuleName
primopsCollectionModuleName = mkModuleName "Nix.Compile.Primops.Collection"

-- | Primops submodule for control flow operations.
primopsControlModuleName :: ModuleName
primopsControlModuleName = mkModuleName "Nix.Compile.Primops.Control"

-- | Module containing builtin functions (re-export hub).
-- Note: Some functions are defined in submodules; use the specific module names
-- for lookupId calls where needed.
builtinsModuleName :: ModuleName
builtinsModuleName = mkModuleName "Nix.Compile.Builtins"

-- | Builtins submodule for list operations.
builtinsListModuleName :: ModuleName
builtinsListModuleName = mkModuleName "Nix.Compile.Builtins.List"

-- | Builtins submodule for control flow operations.
builtinsControlModuleName :: ModuleName
builtinsControlModuleName = mkModuleName "Nix.Compile.Builtins.Control"

-- | Builtins submodule for attribute set operations.
builtinsAttrSetModuleName :: ModuleName
builtinsAttrSetModuleName = mkModuleName "Nix.Compile.Builtins.AttrSet"

-- | Module containing IO builtins.
builtinsIOModuleName :: ModuleName
builtinsIOModuleName = mkModuleName "Nix.Compile.Builtins.IO"

-- | Module containing IO primitive operations.
primopsIOModuleName :: ModuleName
primopsIOModuleName = mkModuleName "Nix.Compile.Primops.IO"

-- * Type Names

nixValueTypeName :: OccName
nixValueTypeName = mkTcOcc "NixValue"

nixEnvTypeName :: OccName
nixEnvTypeName = mkTcOcc "NixEnv"

-- * Runtime References

-- | All runtime references needed for code generation.
--
-- These are loaded once at session start and reused for all compilations.
-- They reference concrete Ids and DataCons in the runtime library.
data RuntimeRefs = RuntimeRefs
  { -- DynFlags and Platform for Core construction
    refDynFlags :: !DynFlags
  , refPlatform :: !Platform

    -- * NixValue type and constructors
  , refNixValueTyCon :: !TyCon
  , refNixValueType :: !Type

    -- * NixEnv type
  , refNixEnvTyCon :: !TyCon
  , refNixEnvType :: !Type

    -- * VarName type (for list construction in pattern validation)
  , refVarNameTyCon :: !TyCon
  , refVarNameType :: !Type

    -- * NixValue data constructors
  , refVIntCon :: !DataCon
  , refVFloatCon :: !DataCon
  , refVBoolCon :: !DataCon
  , refVNullCon :: !DataCon
  , refVStringCon :: !DataCon
  , refVPathCon :: !DataCon
  , refVListCon :: !DataCon
  , refVAttrsCon :: !DataCon
  , refVClosureCon :: !DataCon
  , refVBuiltinCon :: !DataCon

    -- * Special Ids (not data constructors)
  , refVNullId :: !Id  -- Pre-constructed VNull value

    -- * NixAttrs operations
  , refEmptyAttrsId :: !Id
  , refLookupAttrId :: !Id
  , refInsertAttrId :: !Id
  , refAttrsFromListId :: !Id
  , refUnionAttrsId :: !Id

    -- * Dynamic path operations
  , refInsertAtPathDynamicId :: !Id  -- insertAtPathDynamic :: Vector NixValue -> NixValue -> NixAttrs -> NixAttrs
  , refMkVarNameFromValueId :: !Id   -- mkVarNameFromValue :: NixValue -> VarName

    -- * NixEnv operations
  , refEmptyEnvId :: !Id
  , refMkEnvWithCurrentFileId :: !Id
  , refPushWithScopeId :: !Id
  , refLookupWithScopesId :: !Id

    -- * Context operations
  , refEmptyContextId :: !Id
  , refUnionContextId :: !Id

    -- * Vector operations
  , refVFromListId :: !Id

    -- * Primops - Type coercion
  , refExpectIntId :: !Id
  , refExpectFloatId :: !Id
  , refExpectBoolId :: !Id
  , refExpectStringId :: !Id
  , refExpectPathId :: !Id
  , refExpectListId :: !Id
  , refExpectAttrsId :: !Id
  , refExpectFunctionId :: !Id

    -- * Primops - Type predicates
  , refIsAttrsPrimopId :: !Id

    -- * Primops - Arithmetic
  , refNixAddId :: !Id
  , refNixSubId :: !Id
  , refNixMulId :: !Id
  , refNixDivId :: !Id
  , refNixNegId :: !Id

    -- * Primops - Comparison
  , refNixEqId :: !Id
  , refNixNEqId :: !Id
  , refNixLtId :: !Id
  , refNixLteId :: !Id
  , refNixGtId :: !Id
  , refNixGteId :: !Id

    -- * Primops - Logical
  , refNixNotId :: !Id
  , refNixAndId :: !Id
  , refNixOrId :: !Id
  , refNixImplId :: !Id

    -- * Primops - Collections
  , refNixListConcatId :: !Id
  , refNixUpdateId :: !Id
  , refNixSelectId :: !Id
  , refNixSelectOrId :: !Id
  , refNixSelectPathId :: !Id
  , refNixHasAttrId :: !Id
  , refNixHasAttrPathId :: !Id

    -- * Primops - Control flow
  , refNixApplyId :: !Id
  , refNixAssertId :: !Id
  , refNixThrowId :: !Id
  , refNixCoerceToStringId :: !Id

    -- * Primops - Path resolution
  , refNixResolveEnvPathId :: !Id

    -- * Primops - Pattern validation
  , refNixCheckClosedPatternId :: !Id

    -- * Builtins
  , refBuiltinsAttrSetId :: !Id

    -- * Global builtins (exposed without builtins. prefix)
    -- These are builtins that Nix exposes at the top level in addition to
    -- being available under the builtins attribute set.
  , refGlobalAbortId :: !Id
  , refGlobalBaseNameOfId :: !Id
  , refGlobalDirOfId :: !Id
  , refGlobalFalseId :: !Id
  , refGlobalIsNullId :: !Id
  , refGlobalMapId :: !Id
  , refGlobalNullId :: !Id
  , refGlobalRemoveAttrsId :: !Id
  , refGlobalThrowId :: !Id
  , refGlobalToStringId :: !Id
  , refGlobalTraceId :: !Id
  , refGlobalTrueId :: !Id
  , refGlobalImportId :: !Id
    -- ^ import requires NixEnv, so handled specially in compileVar

    -- * Type construction helpers
  , refMkVarNameStrId :: !Id
  , refMkTextId :: !Id
  , refMkPathId :: !Id

    -- * RuntimeParams and RuntimeVariadic types and constructors
  , refRuntimeParamsTyCon :: !TyCon
  , refRuntimeParamsType :: !Type
  , refRuntimeVariadicTyCon :: !TyCon
  , refRuntimeVariadicType :: !Type
  , refRuntimeParamCon :: !DataCon      -- ^ RuntimeParam !VarName
  , refRuntimeParamSetCon :: !DataCon   -- ^ RuntimeParamSet !(Maybe VarName) !RuntimeVariadic ![(VarName, Bool)]
  , refRuntimeClosedCon :: !DataCon     -- ^ RuntimeClosed (exact match)
  , refRuntimeVariadicCon :: !DataCon   -- ^ RuntimeVariadic (allows extra args)

    -- * Path conversion
  , refStringToPathId :: !Id            -- ^ stringToPath :: NixValue -> NixValue

    -- * IO Primops
  , refNixImportId :: !Id               -- ^ nixImport :: Path -> IO NixValue
  , refNixReadFileId :: !Id             -- ^ nixReadFile :: Path -> IO Text
  , refNixReadDirId :: !Id              -- ^ nixReadDir :: Path -> IO [(Text, FileType)]
  , refNixPathExistsId :: !Id           -- ^ nixPathExists :: Path -> IO Bool
  , refNixGetEnvId :: !Id               -- ^ nixGetEnv :: Text -> IO (Maybe Text)

    -- * GHC primitive operations for string literals
  , refMkStringIds :: !MkStringIds      -- ^ unpackCString# and unpackCStringUtf8# for string creation
  }

-- | Load all runtime references from a GHC session.
--
-- This must be called after the runtime modules have been loaded into
-- the session. Returns Nothing if loading fails (usually means the
-- modules aren't properly compiled/loaded).
--
-- For debugging, use 'debugLoadRuntimeRefs' which prints which lookups fail.
loadRuntimeRefs :: HscEnv -> IO (Maybe RuntimeRefs)
loadRuntimeRefs hsc = runMaybeT $ do
  let dflags = hsc_dflags hsc
      platform = targetPlatform dflags

  -- Load NixValue type
  nixValueTyCon <- MaybeT $ lookupTyCon hsc valueModuleName (mkTcOcc "NixValue")
  let nixValueTy = mkTyConTy nixValueTyCon

  -- Load NixEnv type
  nixEnvTyCon <- MaybeT $ lookupTyCon hsc valueModuleName (mkTcOcc "NixEnv")
  let nixEnvTy = mkTyConTy nixEnvTyCon

  -- Load VarName type by extracting it from mkVarNameStr's return type.
  -- VarName is in a hidden package (hnix-types), but we can get its TyCon
  -- from a function that returns it, which is in the exposed package.
  mkVarNameStrId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "mkVarNameStr")
  let mkVarNameStrType = idType mkVarNameStrId
      -- mkVarNameStr :: String -> VarName, so we need the result type
      -- splitFunTy returns (Mult, argTy, resultTy) in GHC 9.x
      (_, _, varNameTy) = splitFunTy mkVarNameStrType
  varNameTyCon <- MaybeT $ pure $ tyConAppTyCon_maybe varNameTy

  -- Load data constructors
  vIntCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VInt")
  vFloatCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VFloat")
  vBoolCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VBool")
  vNullCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VNull")
  vStringCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VString")
  vPathCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VPath")
  vListCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VList")
  vAttrsCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VAttrs")
  vClosureCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VClosure")
  vBuiltinCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "VBuiltin")

  -- Load Value module Ids
  -- Note: builtinNull is defined in Builtins.hs, not Value.hs
  vNullId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "builtinNull")
  emptyAttrsId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "emptyAttrs")
  lookupAttrId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "lookupAttr")
  insertAttrId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "insertAttr")
  attrsFromListId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "attrsFromList")
  unionAttrsId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "unionAttrs")

  -- Dynamic path operations
  insertAtPathDynamicId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "insertAtPathDynamic")
  mkVarNameFromValueId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "mkVarNameFromValue")

  emptyEnvId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "emptyEnv")
  mkEnvWithCurrentFileId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "mkEnvWithCurrentFile")
  pushWithScopeId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "pushWithScope")
  lookupWithScopesId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "lookupWithScopesOrThrow")
  -- Context operations: defined in Value.Context, NOT Value (despite re-export).
  -- Using valueModuleName here would fail at runtime. See module header.
  emptyContextId <- MaybeT $ lookupId hsc contextModuleName (mkVarOcc "emptyContext")
  unionContextId <- MaybeT $ lookupId hsc contextModuleName (mkVarOcc "unionContext")

  -- Load Vector fromList
  -- Note: This is from Data.Vector, we need to handle it specially
  vFromListId <- MaybeT $ lookupId hsc (mkModuleName "Data.Vector") (mkVarOcc "fromList")

  -- Load Primops - type coercion (from Primops.Coerce)
  expectIntId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectInt")
  expectFloatId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectFloat")
  expectBoolId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectBool")
  expectStringId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectString")
  expectPathId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectPath")
  expectListId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectList")
  expectAttrsId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectAttrs")
  expectFunctionId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "expectFunction")

  -- Type predicates (from Primops.Coerce)
  isAttrsPrimopId <- MaybeT $ lookupId hsc primopsCoerceModuleName (mkVarOcc "isAttrsPrimop")

  -- Arithmetic (from Primops.Arithmetic)
  nixAddId <- MaybeT $ lookupId hsc primopsArithmeticModuleName (mkVarOcc "nixAdd")
  nixSubId <- MaybeT $ lookupId hsc primopsArithmeticModuleName (mkVarOcc "nixSub")
  nixMulId <- MaybeT $ lookupId hsc primopsArithmeticModuleName (mkVarOcc "nixMul")
  nixDivId <- MaybeT $ lookupId hsc primopsArithmeticModuleName (mkVarOcc "nixDiv")
  nixNegId <- MaybeT $ lookupId hsc primopsArithmeticModuleName (mkVarOcc "nixNeg")

  -- Comparison (from Primops.Comparison)
  nixEqId <- MaybeT $ lookupId hsc primopsComparisonModuleName (mkVarOcc "nixEq")
  nixNEqId <- MaybeT $ lookupId hsc primopsComparisonModuleName (mkVarOcc "nixNEq")
  nixLtId <- MaybeT $ lookupId hsc primopsComparisonModuleName (mkVarOcc "nixLt")
  nixLteId <- MaybeT $ lookupId hsc primopsComparisonModuleName (mkVarOcc "nixLte")
  nixGtId <- MaybeT $ lookupId hsc primopsComparisonModuleName (mkVarOcc "nixGt")
  nixGteId <- MaybeT $ lookupId hsc primopsComparisonModuleName (mkVarOcc "nixGte")

  -- Logical (from Primops.Logical)
  nixNotId <- MaybeT $ lookupId hsc primopsLogicalModuleName (mkVarOcc "nixNot")
  nixAndId <- MaybeT $ lookupId hsc primopsLogicalModuleName (mkVarOcc "nixAnd")
  nixOrId <- MaybeT $ lookupId hsc primopsLogicalModuleName (mkVarOcc "nixOr")
  nixImplId <- MaybeT $ lookupId hsc primopsLogicalModuleName (mkVarOcc "nixImpl")

  -- Collection operations (from Primops.Collection)
  nixListConcatId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixListConcat")
  nixUpdateId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixUpdate")
  nixSelectId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixSelect")
  nixSelectOrId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixSelectOr")
  nixSelectPathId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixSelectPath")
  nixHasAttrId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixHasAttr")
  nixHasAttrPathId <- MaybeT $ lookupId hsc primopsCollectionModuleName (mkVarOcc "nixHasAttrPath")

  -- Control flow (from Primops.Control)
  nixApplyId <- MaybeT $ lookupId hsc primopsControlModuleName (mkVarOcc "nixApply")
  nixAssertId <- MaybeT $ lookupId hsc primopsControlModuleName (mkVarOcc "nixAssert")
  nixThrowId <- MaybeT $ lookupId hsc primopsControlModuleName (mkVarOcc "nixThrow")

  -- String operations (from Primops.String)
  nixCoerceToStringId <- MaybeT $ lookupId hsc primopsStringModuleName (mkVarOcc "nixCoerceToString")

  -- Path resolution (from Primops.Control)
  nixResolveEnvPathId <- MaybeT $ lookupId hsc primopsControlModuleName (mkVarOcc "nixResolveEnvPath")

  -- Pattern validation (from Primops.Control)
  nixCheckClosedPatternId <- MaybeT $ lookupId hsc primopsControlModuleName (mkVarOcc "nixCheckClosedPattern")

  -- Load Builtins
  builtinsAttrSetId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "builtinsAttrSet")

  -- Load global builtins (exposed at top level without builtins. prefix)
  -- Some builtins are already NixValue type (VBuiltin), others need wrapped versions.
  -- - builtinTrue, builtinFalse, builtinNull: Already NixValue (constants)
  -- - builtinMap, builtinRemoveAttrs, builtinTrace: Already NixValue (VBuiltin)
  -- - globalToString, globalThrow, globalAbort, globalIsNull, globalBaseNameOf, globalDirOf:
  --   VBuiltin-wrapped versions of function-type builtins
  globalAbortId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "globalAbort")
  globalBaseNameOfId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "globalBaseNameOf")
  globalDirOfId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "globalDirOf")
  globalFalseId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "builtinFalse")
  globalIsNullId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "globalIsNull")
  globalMapId <- MaybeT $ lookupId hsc builtinsListModuleName (mkVarOcc "builtinMap")
  globalNullId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "builtinNull")
  globalRemoveAttrsId <- MaybeT $ lookupId hsc builtinsAttrSetModuleName (mkVarOcc "builtinRemoveAttrs")
  globalThrowId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "globalThrow")
  globalToStringId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "globalToString")
  globalTraceId <- MaybeT $ lookupId hsc builtinsControlModuleName (mkVarOcc "builtinTrace")
  globalTrueId <- MaybeT $ lookupId hsc builtinsModuleName (mkVarOcc "builtinTrue")
  globalImportId <- MaybeT $ lookupId hsc builtinsIOModuleName (mkVarOcc "builtinImport")

  -- Load type construction helpers
  mkVarNameStrId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "mkVarNameStr")
  mkTextId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "mkText")
  mkPathId <- MaybeT $ lookupId hsc valueModuleName (mkVarOcc "mkPath")

  -- Load RuntimeParams and RuntimeVariadic types and constructors
  runtimeParamsTyCon <- MaybeT $ lookupTyCon hsc valueModuleName (mkTcOcc "RuntimeParams")
  let runtimeParamsTy = mkTyConTy runtimeParamsTyCon
  runtimeVariadicTyCon <- MaybeT $ lookupTyCon hsc valueModuleName (mkTcOcc "RuntimeVariadic")
  let runtimeVariadicTy = mkTyConTy runtimeVariadicTyCon
  runtimeParamCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "RuntimeParam")
  runtimeParamSetCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "RuntimeParamSet")
  runtimeClosedCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "RuntimeClosed")
  runtimeVariadicCon <- MaybeT $ lookupDataCon hsc valueModuleName (mkDataOcc "RuntimeVariadic")

  -- Load path conversion (from Primops.String)
  stringToPathId <- MaybeT $ lookupId hsc primopsStringModuleName (mkVarOcc "stringToPath")

  -- Load IO Primops
  nixImportId <- MaybeT $ lookupId hsc primopsIOModuleName (mkVarOcc "nixImport")
  nixReadFileId <- MaybeT $ lookupId hsc primopsIOModuleName (mkVarOcc "nixReadFile")
  nixReadDirId <- MaybeT $ lookupId hsc primopsIOModuleName (mkVarOcc "nixReadDir")
  nixPathExistsId <- MaybeT $ lookupId hsc primopsIOModuleName (mkVarOcc "nixPathExists")
  nixGetEnvId <- MaybeT $ lookupId hsc primopsIOModuleName (mkVarOcc "nixGetEnv")

  -- Load GHC's string unpacking primitives for creating proper String literals
  -- getMkStringIds needs a function to look up Ids by Name
  mkStringIds <- liftIO $ getMkStringIds (lookupBuiltinId hsc)

  pure RuntimeRefs
    { refDynFlags = dflags
    , refPlatform = platform
    , refNixValueTyCon = nixValueTyCon
    , refNixValueType = nixValueTy
    , refNixEnvTyCon = nixEnvTyCon
    , refNixEnvType = nixEnvTy
    , refVarNameTyCon = varNameTyCon
    , refVarNameType = varNameTy
    , refVIntCon = vIntCon
    , refVFloatCon = vFloatCon
    , refVBoolCon = vBoolCon
    , refVNullCon = vNullCon
    , refVStringCon = vStringCon
    , refVPathCon = vPathCon
    , refVListCon = vListCon
    , refVAttrsCon = vAttrsCon
    , refVClosureCon = vClosureCon
    , refVBuiltinCon = vBuiltinCon
    , refVNullId = vNullId
    , refEmptyAttrsId = emptyAttrsId
    , refLookupAttrId = lookupAttrId
    , refInsertAttrId = insertAttrId
    , refAttrsFromListId = attrsFromListId
    , refUnionAttrsId = unionAttrsId
    , refInsertAtPathDynamicId = insertAtPathDynamicId
    , refMkVarNameFromValueId = mkVarNameFromValueId
    , refEmptyEnvId = emptyEnvId
    , refMkEnvWithCurrentFileId = mkEnvWithCurrentFileId
    , refPushWithScopeId = pushWithScopeId
    , refLookupWithScopesId = lookupWithScopesId
    , refEmptyContextId = emptyContextId
    , refUnionContextId = unionContextId
    , refVFromListId = vFromListId
    , refExpectIntId = expectIntId
    , refExpectFloatId = expectFloatId
    , refExpectBoolId = expectBoolId
    , refExpectStringId = expectStringId
    , refExpectPathId = expectPathId
    , refExpectListId = expectListId
    , refExpectAttrsId = expectAttrsId
    , refExpectFunctionId = expectFunctionId
    , refIsAttrsPrimopId = isAttrsPrimopId
    , refNixAddId = nixAddId
    , refNixSubId = nixSubId
    , refNixMulId = nixMulId
    , refNixDivId = nixDivId
    , refNixNegId = nixNegId
    , refNixEqId = nixEqId
    , refNixNEqId = nixNEqId
    , refNixLtId = nixLtId
    , refNixLteId = nixLteId
    , refNixGtId = nixGtId
    , refNixGteId = nixGteId
    , refNixNotId = nixNotId
    , refNixAndId = nixAndId
    , refNixOrId = nixOrId
    , refNixImplId = nixImplId
    , refNixListConcatId = nixListConcatId
    , refNixUpdateId = nixUpdateId
    , refNixSelectId = nixSelectId
    , refNixSelectOrId = nixSelectOrId
    , refNixSelectPathId = nixSelectPathId
    , refNixHasAttrId = nixHasAttrId
    , refNixHasAttrPathId = nixHasAttrPathId
    , refNixApplyId = nixApplyId
    , refNixAssertId = nixAssertId
    , refNixThrowId = nixThrowId
    , refNixCoerceToStringId = nixCoerceToStringId
    , refNixResolveEnvPathId = nixResolveEnvPathId
    , refNixCheckClosedPatternId = nixCheckClosedPatternId
    , refBuiltinsAttrSetId = builtinsAttrSetId
    , refGlobalAbortId = globalAbortId
    , refGlobalBaseNameOfId = globalBaseNameOfId
    , refGlobalDirOfId = globalDirOfId
    , refGlobalFalseId = globalFalseId
    , refGlobalIsNullId = globalIsNullId
    , refGlobalMapId = globalMapId
    , refGlobalNullId = globalNullId
    , refGlobalRemoveAttrsId = globalRemoveAttrsId
    , refGlobalThrowId = globalThrowId
    , refGlobalToStringId = globalToStringId
    , refGlobalTraceId = globalTraceId
    , refGlobalTrueId = globalTrueId
    , refGlobalImportId = globalImportId
    , refMkVarNameStrId = mkVarNameStrId
    , refMkTextId = mkTextId
    , refMkPathId = mkPathId
    , refRuntimeParamsTyCon = runtimeParamsTyCon
    , refRuntimeParamsType = runtimeParamsTy
    , refRuntimeVariadicTyCon = runtimeVariadicTyCon
    , refRuntimeVariadicType = runtimeVariadicTy
    , refRuntimeParamCon = runtimeParamCon
    , refRuntimeParamSetCon = runtimeParamSetCon
    , refRuntimeClosedCon = runtimeClosedCon
    , refRuntimeVariadicCon = runtimeVariadicCon
    , refStringToPathId = stringToPathId
    , refNixImportId = nixImportId
    , refNixReadFileId = nixReadFileId
    , refNixReadDirId = nixReadDirId
    , refNixPathExistsId = nixPathExistsId
    , refNixGetEnvId = nixGetEnvId
    , refMkStringIds = mkStringIds
    }

-- * Internal lookup functions

-- | Find a Module from its ModuleName using GHC's finder.
--
-- Handles the FoundMultiple case gracefully by using the first match
-- and logging a warning. This can happen when both Cabal and Nix package
-- databases contain the same module.
--
-- IMPORTANT: When a module is in a hidden package (transitive dependency),
-- we construct the Module directly using the unit ID from the error info.
-- This allows us to access types from packages that are dependencies of
-- exposed packages but not explicitly exposed themselves.
findModuleIO :: HscEnv -> ModuleName -> IO (Maybe Module)
findModuleIO hsc modName = do
  result <- findImportedModule hsc modName NoPkgQual
  case result of
    Found _ m -> pure (Just m)
    FoundMultiple ((m, _):_) -> do
      -- Warn but continue with first match
      hPutStrLn stderr $ "Warning: Module '" <> moduleNameString modName
        <> "' found in multiple packages, using first"
      pure (Just m)
    FoundMultiple [] -> pure Nothing
    NotFound { fr_pkgs_hidden = pkgsHidden } ->
      -- If the module is in a hidden package, construct the Module directly.
      -- This is the workaround for packages that are loaded as transitive
      -- dependencies and thus hidden from findImportedModule.
      case pkgsHidden of
        (hiddenUnit:_) -> pure $ Just $ mkModule hiddenUnit modName
        [] -> pure Nothing
    _ -> pure Nothing

-- | Look up a TyThing by module and name using the GHC API.
-- Uses the NameCache and HPT/EPS directly without needing an interactive session.
lookupTyThingIO :: HscEnv -> ModuleName -> OccName -> IO (Maybe TyThing)
lookupTyThingIO hsc modName occName = runMaybeT $ do
  -- Step 1: Find the Module from the ModuleName
  mod <- MaybeT $ findModuleIO hsc modName
  -- Step 2: Get Name from NameCache (no GhcMonad needed)
  let nc = hsc_NC hsc
  name <- liftIO $ lookupNameCache nc mod occName
  -- Step 3: Lookup TyThing (pure IO, uses HPT + EPS)
  MaybeT $ lookupType hsc name

-- | Look up a type constructor by module and name.
lookupTyCon :: HscEnv -> ModuleName -> OccName -> IO (Maybe TyCon)
lookupTyCon hsc modName occName = runMaybeT $ do
  tyThing <- MaybeT $ lookupTyThingIO hsc modName occName
  case tyThing of
    ATyCon tc -> pure tc
    _ -> MaybeT $ pure Nothing

-- | Look up a data constructor by module and name.
lookupDataCon :: HscEnv -> ModuleName -> OccName -> IO (Maybe DataCon)
lookupDataCon hsc modName occName = runMaybeT $ do
  tyThing <- MaybeT $ lookupTyThingIO hsc modName occName
  case tyThing of
    AConLike (RealDataCon dc) -> pure dc
    _ -> MaybeT $ pure Nothing

-- | Look up an Id (variable/function) by module and name.
lookupId :: HscEnv -> ModuleName -> OccName -> IO (Maybe Id)
lookupId hsc modName occName = runMaybeT $ do
  tyThing <- MaybeT $ lookupTyThingIO hsc modName occName
  case tyThing of
    AnId ident -> pure ident
    _ -> MaybeT $ pure Nothing

-- | Look up a GHC built-in Id by Name.
-- Used for getMkStringIds which needs to find unpackCString# etc.
lookupBuiltinId :: HscEnv -> Name -> IO Id
lookupBuiltinId hsc name = do
  mTyThing <- lookupType hsc name
  case mTyThing of
    Just (AnId ident) -> pure ident
    Just _ -> error $ "lookupBuiltinId: expected Id for " <> toText (occNameString (nameOccName name))
    Nothing -> error $ "lookupBuiltinId: failed to find " <> toText (occNameString (nameOccName name))

-- | Debug version of loadRuntimeRefs that prints which lookups fail.
debugLoadRuntimeRefs :: HscEnv -> IO ()
debugLoadRuntimeRefs hsc = do
  -- First check if we can find the modules at all
  putStrLn "Checking module discovery..."
  mValue <- findModuleIO hsc valueModuleName
  case mValue of
    Just m -> putStrLn $ "  Found module: " <> moduleNameString (moduleName m)
    Nothing -> putStrLn $ "  FAIL: Cannot find module " <> moduleNameString valueModuleName
  mContext <- findModuleIO hsc contextModuleName
  case mContext of
    Just m -> putStrLn $ "  Found module: " <> moduleNameString (moduleName m)
    Nothing -> putStrLn $ "  FAIL: Cannot find module " <> moduleNameString contextModuleName
  mPrimops <- findModuleIO hsc primopsModuleName
  case mPrimops of
    Just m -> putStrLn $ "  Found module: " <> moduleNameString (moduleName m)
    Nothing -> putStrLn $ "  FAIL: Cannot find module " <> moduleNameString primopsModuleName
  mBuiltins <- findModuleIO hsc builtinsModuleName
  case mBuiltins of
    Just m -> putStrLn $ "  Found module: " <> moduleNameString (moduleName m)
    Nothing -> putStrLn $ "  FAIL: Cannot find module " <> moduleNameString builtinsModuleName
  mPrimopsIO <- findModuleIO hsc primopsIOModuleName
  case mPrimopsIO of
    Just m -> putStrLn $ "  Found module: " <> moduleNameString (moduleName m)
    Nothing -> putStrLn $ "  FAIL: Cannot find module " <> moduleNameString primopsIOModuleName
  mVector <- findModuleIO hsc (mkModuleName "Data.Vector")
  case mVector of
    Just m -> putStrLn $ "  Found module: " <> moduleNameString (moduleName m)
    Nothing -> putStrLn $ "  FAIL: Cannot find module Data.Vector"

  let checkTyCon name modName occ = do
        result <- lookupTyCon hsc modName occ
        case result of
          Just _ -> putStrLn $ "  OK: " <> name
          Nothing -> putStrLn $ "  FAIL: " <> name
      checkDataCon name modName occ = do
        result <- lookupDataCon hsc modName occ
        case result of
          Just _ -> putStrLn $ "  OK: " <> name
          Nothing -> putStrLn $ "  FAIL: " <> name
      checkId name modName occ = do
        result <- lookupId hsc modName occ
        case result of
          Just _ -> putStrLn $ "  OK: " <> name
          Nothing -> putStrLn $ "  FAIL: " <> name

  putStrLn "Checking NixValue type..."
  checkTyCon "NixValue" valueModuleName (mkTcOcc "NixValue")

  putStrLn "Checking data constructors..."
  checkDataCon "VInt" valueModuleName (mkDataOcc "VInt")
  checkDataCon "VFloat" valueModuleName (mkDataOcc "VFloat")

  putStrLn "Checking Value Ids..."
  checkId "emptyAttrs" valueModuleName (mkVarOcc "emptyAttrs")
  checkId "mkVarNameStr" valueModuleName (mkVarOcc "mkVarNameStr")
  checkId "emptyEnv" valueModuleName (mkVarOcc "emptyEnv")

  putStrLn "Checking Context Ids..."
  checkId "emptyContext" contextModuleName (mkVarOcc "emptyContext")

  putStrLn "Checking Primops Ids (from submodules)..."
  checkId "expectInt" primopsCoerceModuleName (mkVarOcc "expectInt")
  checkId "expectFloat" primopsCoerceModuleName (mkVarOcc "expectFloat")
  checkId "expectBool" primopsCoerceModuleName (mkVarOcc "expectBool")
  checkId "nixAdd" primopsArithmeticModuleName (mkVarOcc "nixAdd")
  checkId "nixEq" primopsComparisonModuleName (mkVarOcc "nixEq")
  checkId "isAttrsPrimop" primopsCoerceModuleName (mkVarOcc "isAttrsPrimop")

  putStrLn "Checking Builtins Ids..."
  checkId "builtinNull" builtinsModuleName (mkVarOcc "builtinNull")
  checkId "builtinsAttrSet" builtinsModuleName (mkVarOcc "builtinsAttrSet")

  putStrLn "Checking Global Builtins Ids..."
  checkId "globalToString" builtinsModuleName (mkVarOcc "globalToString")
  checkId "globalThrow" builtinsModuleName (mkVarOcc "globalThrow")
  checkId "globalAbort" builtinsModuleName (mkVarOcc "globalAbort")
  checkId "globalIsNull" builtinsModuleName (mkVarOcc "globalIsNull")
  checkId "globalBaseNameOf" builtinsModuleName (mkVarOcc "globalBaseNameOf")
  checkId "globalDirOf" builtinsModuleName (mkVarOcc "globalDirOf")
  checkId "builtinMap" builtinsListModuleName (mkVarOcc "builtinMap")
  checkId "builtinTrace" builtinsControlModuleName (mkVarOcc "builtinTrace")
  checkId "builtinRemoveAttrs" builtinsAttrSetModuleName (mkVarOcc "builtinRemoveAttrs")
  checkId "builtinTrue" builtinsModuleName (mkVarOcc "builtinTrue")
  checkId "builtinFalse" builtinsModuleName (mkVarOcc "builtinFalse")

  putStrLn "Checking Vector Ids..."
  checkId "Data.Vector.fromList" (mkModuleName "Data.Vector") (mkVarOcc "fromList")

  putStrLn "Checking IO Primops Ids..."
  checkId "nixImport" primopsIOModuleName (mkVarOcc "nixImport")
  checkId "nixReadFile" primopsIOModuleName (mkVarOcc "nixReadFile")
  checkId "nixReadDir" primopsIOModuleName (mkVarOcc "nixReadDir")
  checkId "nixPathExists" primopsIOModuleName (mkVarOcc "nixPathExists")
  checkId "nixGetEnv" primopsIOModuleName (mkVarOcc "nixGetEnv")
