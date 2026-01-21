-- | Pre-interned static VarNames for common variable names.
--
-- This module provides NOINLINE CAFs for frequently-used VarNames,
-- ensuring each name is interned exactly once at program startup rather
-- than on every use. This is similar to Nix C++'s @StaticEvalSymbols@.
--
-- Usage:
--
-- @
-- import Nix.Types.VarName.Static
--
-- -- Instead of:
-- mkVarName "name"
--
-- -- Use:
-- sName
-- @
--
-- The 's' prefix stands for "static" and avoids name collisions.
--
-- Call 'preInternAll' early in main to force evaluation of all static
-- VarNames before evaluation begins.
module Nix.Types.VarName.Static
  ( -- * Initialization
    preInternAll
    -- * Common attribute names
  , sName
  , sValue
  , sKey
  , sType
  , sOutputs
  , sAllOutputs
  , sOut
  , sDrvPath
  , sOutPath
  , sOutputName
  , sAll
  , sMeta
  , sSystem
  , sBuilder
  , sArgs
  , sFile
  , sLine
  , sCol
  , sColumn
  , sPath
  , sUrl
  , sUrls
  , sHash
  , sSha256
  , sSha512
  , sSha1
  , sMd5
  , sHashAlgo
  , sExecutable
  , sUnpack
  , sRecursive
  , sOutputHash
  , sOutputHashAlgo
  , sOutputHashMode
  , sSuccess
  , sPrefix
  , sUri
  , sVersion
  , sFlake
  , sBody
  , sContext
  , sEnv
  , sDerivation
  , sDrv
  , sStart
  , sStartSet
  , sStartColumn
  , sStartLine
  , sEnd
  , sEndColumn
  , sEndLine
  , sOperator
  , sContentAddressed
  , sAllowedReferences
  , sAllowedRequisites
  , sDisallowedReferences
  , sDisallowedRequisites
  , sExportReferencesGraph
  , sImpure
  , sImpureEnvVars
  , sPassAsFile
  , sPreferLocalBuild
  , sRequiredSystemFeatures
  , sAllowSubstitutes
  , sOutputChecks
    -- * Special/reserved names
  , sBuiltins
  , sToString
  , sFunctor
  , sStructuredAttrs
  , sIgnoreNulls
  , sJson
  , sCurPos
  , sCurFile
  , sIncludes
  , sLet
  , sIn
  , sIf
  , sThen
  , sElse
  , sAssert
  , sWith
  , sRec
  , sInherit
  , sOr
  , sTrue
  , sFalse
  , sNull
  , sEllipsis
  , sText
  , sNix
  , sNixStore
  , sNixPath
  , sNixVersion
  , sCurrentSystem
  , sCurrentTime
  , sLangVersion
  , sStoreDir
    -- * Builtin function names
  , sAbort
  , sAdd
  , sAddDrvOutputDependencies
  , sAddErrorContext
  , sAny
  , sAppendContext
  , sAttrNames
  , sAttrValues
  , sBaseNameOf
  , sBitAnd
  , sBitOr
  , sBitXor
  , sBreak
  , sCatAttrs
  , sCeil
  , sCompareVersions
  , sConcatLists
  , sConcatMap
  , sConcatStringsSep
  , sConvertHash
  , sDeepSeq
  , sDirOf
  , sDiv
  , sElem
  , sElemAt
  , sFetchGit
  , sFetchTarball
  , sFetchTree
  , sFetchurl
  , sFilter
  , sFilterSource
  , sFindFile
  , sFloor
  , sFoldl'
  , sFromJSON
  , sFromTOML
  , sFunctionArgs
  , sGenList
  , sGenericClosure
  , sGetAttr
  , sGetContext
  , sGetEnv
  , sGetFlake
  , sGroupBy
  , sHasAttr
  , sHasContext
  , sHashFile
  , sHashString
  , sHead
  , sImport
  , sIntersectAttrs
  , sIsAttrs
  , sIsBool
  , sIsFloat
  , sIsFunction
  , sIsInt
  , sIsList
  , sIsNull
  , sIsPath
  , sIsString
  , sLength
  , sLessThan
  , sListToAttrs
  , sMap
  , sMapAttrs
  , sMatch
  , sMul
  , sParseDrvName
  , sPartition
  , sPathExists
  , sPlaceholder
  , sReadDir
  , sReadFile
  , sReadFileType
  , sRemoveAttrs
  , sReplaceStrings
  , sScopedImport
  , sSeq
  , sSort
  , sSplit
  , sSplitVersion
  , sStorePath
  , sStringLength
  , sSub
  , sSubstring
  , sTail
  , sThrow
  , sToFile
  , sToJSON
  , sToPath
  , sToXML
  , sTrace
  , sTraceVerbose
  , sTryEval
  , sTypeOf
  , sUnsafeDiscardOutputDependency
  , sUnsafeDiscardStringContext
  , sUnsafeGetAttrPos
  , sValueSize
  , sZipAttrsWith
  , sCurrentPos
  , sReadFiletype
  , sFromJSONFile
    -- * Other common names
  , sRight
  , sWrong
  , sRegular
  , sDirectory
  , sSymlink
  , sUnknown
  , sSubPath
  , sNar
  , sFlat
    -- * Git/fetch related names
  , sNixPathVar
  , sNarHash
  , sSubmodules
  , sRev
  , sShortRev
  , sRevCount
  , sLastModified
  , sLastModifiedDate
  , sRef
  , sShallow
  , sAllRefs
  , sDirtyRev
  , sDirtyShortRev
    -- * Re-export for convenience
  , VarName
  , mkVarName
  ) where

import           Relude
import           Control.Exception (evaluate)
import           Nix.Types.VarName (VarName, mkVarName)

-- | Force all static VarNames to be interned at program startup.
-- Call this early in main before evaluation begins.
--
-- This ensures all common VarNames are pre-populated in the intern table,
-- avoiding repeated hash lookups during evaluation.
preInternAll :: IO ()
preInternAll = evaluate $ rnf
  -- Common attribute names
  [ sName, sValue, sKey, sType, sOutputs, sAllOutputs, sOut, sDrvPath, sOutPath
  , sOutputName, sAll, sMeta, sSystem, sBuilder, sArgs, sFile, sLine
  , sCol, sColumn, sPath, sUrl, sUrls, sHash, sSha256, sSha512, sSha1
  , sMd5, sHashAlgo, sExecutable, sUnpack, sRecursive, sOutputHash
  , sOutputHashAlgo, sOutputHashMode, sSuccess, sPrefix, sUri, sVersion
  , sFlake, sBody, sContext, sEnv, sDerivation, sDrv
  , sStart, sStartSet, sStartColumn, sStartLine, sEnd, sEndColumn, sEndLine
  , sOperator, sContentAddressed, sAllowedReferences, sAllowedRequisites
  , sDisallowedReferences, sDisallowedRequisites, sExportReferencesGraph
  , sImpure, sImpureEnvVars, sPassAsFile, sPreferLocalBuild
  , sRequiredSystemFeatures, sAllowSubstitutes, sOutputChecks
  -- Special/reserved names
  , sBuiltins, sToString, sFunctor, sStructuredAttrs, sIgnoreNulls, sJson
  , sCurPos, sCurFile, sIncludes, sLet, sIn, sIf, sThen, sElse, sAssert
  , sWith, sRec, sInherit, sOr, sTrue, sFalse, sNull, sEllipsis, sText
  , sNix, sNixStore, sNixPath, sNixVersion, sCurrentSystem, sCurrentTime
  , sLangVersion, sStoreDir
  -- Builtin function names
  , sAbort, sAdd, sAddDrvOutputDependencies, sAddErrorContext, sAny
  , sAppendContext, sAttrNames, sAttrValues, sBaseNameOf, sBitAnd, sBitOr
  , sBitXor, sBreak, sCatAttrs, sCeil, sCompareVersions, sConcatLists
  , sConcatMap, sConcatStringsSep, sConvertHash, sDeepSeq, sDirOf, sDiv
  , sElem, sElemAt, sFetchGit, sFetchTarball, sFetchTree, sFetchurl, sFilter
  , sFilterSource, sFindFile, sFloor, sFoldl', sFromJSON, sFromTOML
  , sFunctionArgs, sGenList, sGenericClosure, sGetAttr, sGetContext, sGetEnv
  , sGetFlake, sGroupBy, sHasAttr, sHasContext, sHashFile, sHashString, sHead
  , sImport, sIntersectAttrs, sIsAttrs, sIsBool, sIsFloat, sIsFunction, sIsInt
  , sIsList, sIsNull, sIsPath, sIsString, sLength, sLessThan, sListToAttrs
  , sMap, sMapAttrs, sMatch, sMul, sParseDrvName, sPartition, sPathExists
  , sPlaceholder, sReadDir, sReadFile, sReadFileType, sRemoveAttrs
  , sReplaceStrings, sScopedImport, sSeq, sSort, sSplit, sSplitVersion
  , sStorePath, sStringLength, sSub, sSubstring, sTail, sThrow, sToFile
  , sToJSON, sToPath, sToXML, sTrace, sTraceVerbose, sTryEval, sTypeOf
  , sUnsafeDiscardOutputDependency, sUnsafeDiscardStringContext
  , sUnsafeGetAttrPos, sValueSize, sZipAttrsWith, sCurrentPos, sReadFiletype
  , sFromJSONFile
  -- Other common names
  , sRight, sWrong, sRegular, sDirectory, sSymlink, sUnknown, sSubPath
  , sNar, sFlat
  -- Git/fetch related names
  , sNixPathVar, sNarHash, sSubmodules, sRev, sShortRev, sRevCount
  , sLastModified, sLastModifiedDate, sRef, sShallow, sAllRefs
  , sDirtyRev, sDirtyShortRev
  ]

------------------------------------------------------------------------
-- Common attribute names
------------------------------------------------------------------------

sName :: VarName
sName = mkVarName "name"
{-# NOINLINE sName #-}

sValue :: VarName
sValue = mkVarName "value"
{-# NOINLINE sValue #-}

sKey :: VarName
sKey = mkVarName "key"
{-# NOINLINE sKey #-}

sType :: VarName
sType = mkVarName "type"
{-# NOINLINE sType #-}

sOutputs :: VarName
sOutputs = mkVarName "outputs"
{-# NOINLINE sOutputs #-}

sAllOutputs :: VarName
sAllOutputs = mkVarName "allOutputs"
{-# NOINLINE sAllOutputs #-}

sOut :: VarName
sOut = mkVarName "out"
{-# NOINLINE sOut #-}

sDrvPath :: VarName
sDrvPath = mkVarName "drvPath"
{-# NOINLINE sDrvPath #-}

sOutPath :: VarName
sOutPath = mkVarName "outPath"
{-# NOINLINE sOutPath #-}

sOutputName :: VarName
sOutputName = mkVarName "outputName"
{-# NOINLINE sOutputName #-}

sAll :: VarName
sAll = mkVarName "all"
{-# NOINLINE sAll #-}

sMeta :: VarName
sMeta = mkVarName "meta"
{-# NOINLINE sMeta #-}

sSystem :: VarName
sSystem = mkVarName "system"
{-# NOINLINE sSystem #-}

sBuilder :: VarName
sBuilder = mkVarName "builder"
{-# NOINLINE sBuilder #-}

sArgs :: VarName
sArgs = mkVarName "args"
{-# NOINLINE sArgs #-}

sFile :: VarName
sFile = mkVarName "file"
{-# NOINLINE sFile #-}

sLine :: VarName
sLine = mkVarName "line"
{-# NOINLINE sLine #-}

sCol :: VarName
sCol = mkVarName "col"
{-# NOINLINE sCol #-}

sColumn :: VarName
sColumn = mkVarName "column"
{-# NOINLINE sColumn #-}

sPath :: VarName
sPath = mkVarName "path"
{-# NOINLINE sPath #-}

sUrl :: VarName
sUrl = mkVarName "url"
{-# NOINLINE sUrl #-}

sUrls :: VarName
sUrls = mkVarName "urls"
{-# NOINLINE sUrls #-}

sHash :: VarName
sHash = mkVarName "hash"
{-# NOINLINE sHash #-}

sSha256 :: VarName
sSha256 = mkVarName "sha256"
{-# NOINLINE sSha256 #-}

sSha512 :: VarName
sSha512 = mkVarName "sha512"
{-# NOINLINE sSha512 #-}

sSha1 :: VarName
sSha1 = mkVarName "sha1"
{-# NOINLINE sSha1 #-}

sMd5 :: VarName
sMd5 = mkVarName "md5"
{-# NOINLINE sMd5 #-}

sHashAlgo :: VarName
sHashAlgo = mkVarName "hashAlgo"
{-# NOINLINE sHashAlgo #-}

sExecutable :: VarName
sExecutable = mkVarName "executable"
{-# NOINLINE sExecutable #-}

sUnpack :: VarName
sUnpack = mkVarName "unpack"
{-# NOINLINE sUnpack #-}

sRecursive :: VarName
sRecursive = mkVarName "recursive"
{-# NOINLINE sRecursive #-}

sOutputHash :: VarName
sOutputHash = mkVarName "outputHash"
{-# NOINLINE sOutputHash #-}

sOutputHashAlgo :: VarName
sOutputHashAlgo = mkVarName "outputHashAlgo"
{-# NOINLINE sOutputHashAlgo #-}

sOutputHashMode :: VarName
sOutputHashMode = mkVarName "outputHashMode"
{-# NOINLINE sOutputHashMode #-}

sSuccess :: VarName
sSuccess = mkVarName "success"
{-# NOINLINE sSuccess #-}

sPrefix :: VarName
sPrefix = mkVarName "prefix"
{-# NOINLINE sPrefix #-}

sUri :: VarName
sUri = mkVarName "uri"
{-# NOINLINE sUri #-}

sVersion :: VarName
sVersion = mkVarName "version"
{-# NOINLINE sVersion #-}

sFlake :: VarName
sFlake = mkVarName "flake"
{-# NOINLINE sFlake #-}

sBody :: VarName
sBody = mkVarName "body"
{-# NOINLINE sBody #-}

sContext :: VarName
sContext = mkVarName "context"
{-# NOINLINE sContext #-}

sEnv :: VarName
sEnv = mkVarName "env"
{-# NOINLINE sEnv #-}

sDerivation :: VarName
sDerivation = mkVarName "derivation"
{-# NOINLINE sDerivation #-}

sDrv :: VarName
sDrv = mkVarName "drv"
{-# NOINLINE sDrv #-}

sStart :: VarName
sStart = mkVarName "start"
{-# NOINLINE sStart #-}

sStartSet :: VarName
sStartSet = mkVarName "startSet"
{-# NOINLINE sStartSet #-}

sStartColumn :: VarName
sStartColumn = mkVarName "startColumn"
{-# NOINLINE sStartColumn #-}

sStartLine :: VarName
sStartLine = mkVarName "startLine"
{-# NOINLINE sStartLine #-}

sEnd :: VarName
sEnd = mkVarName "end"
{-# NOINLINE sEnd #-}

sEndColumn :: VarName
sEndColumn = mkVarName "endColumn"
{-# NOINLINE sEndColumn #-}

sEndLine :: VarName
sEndLine = mkVarName "endLine"
{-# NOINLINE sEndLine #-}

sOperator :: VarName
sOperator = mkVarName "operator"
{-# NOINLINE sOperator #-}

sContentAddressed :: VarName
sContentAddressed = mkVarName "__contentAddressed"
{-# NOINLINE sContentAddressed #-}

sAllowedReferences :: VarName
sAllowedReferences = mkVarName "allowedReferences"
{-# NOINLINE sAllowedReferences #-}

sAllowedRequisites :: VarName
sAllowedRequisites = mkVarName "allowedRequisites"
{-# NOINLINE sAllowedRequisites #-}

sDisallowedReferences :: VarName
sDisallowedReferences = mkVarName "disallowedReferences"
{-# NOINLINE sDisallowedReferences #-}

sDisallowedRequisites :: VarName
sDisallowedRequisites = mkVarName "disallowedRequisites"
{-# NOINLINE sDisallowedRequisites #-}

sExportReferencesGraph :: VarName
sExportReferencesGraph = mkVarName "exportReferencesGraph"
{-# NOINLINE sExportReferencesGraph #-}

sImpure :: VarName
sImpure = mkVarName "__impure"
{-# NOINLINE sImpure #-}

sImpureEnvVars :: VarName
sImpureEnvVars = mkVarName "impureEnvVars"
{-# NOINLINE sImpureEnvVars #-}

sPassAsFile :: VarName
sPassAsFile = mkVarName "passAsFile"
{-# NOINLINE sPassAsFile #-}

sPreferLocalBuild :: VarName
sPreferLocalBuild = mkVarName "preferLocalBuild"
{-# NOINLINE sPreferLocalBuild #-}

sRequiredSystemFeatures :: VarName
sRequiredSystemFeatures = mkVarName "requiredSystemFeatures"
{-# NOINLINE sRequiredSystemFeatures #-}

sAllowSubstitutes :: VarName
sAllowSubstitutes = mkVarName "allowSubstitutes"
{-# NOINLINE sAllowSubstitutes #-}

sOutputChecks :: VarName
sOutputChecks = mkVarName "__outputChecks"
{-# NOINLINE sOutputChecks #-}

------------------------------------------------------------------------
-- Special/reserved names
------------------------------------------------------------------------

sBuiltins :: VarName
sBuiltins = mkVarName "builtins"
{-# NOINLINE sBuiltins #-}

sToString :: VarName
sToString = mkVarName "__toString"
{-# NOINLINE sToString #-}

sFunctor :: VarName
sFunctor = mkVarName "__functor"
{-# NOINLINE sFunctor #-}

sStructuredAttrs :: VarName
sStructuredAttrs = mkVarName "__structuredAttrs"
{-# NOINLINE sStructuredAttrs #-}

sIgnoreNulls :: VarName
sIgnoreNulls = mkVarName "__ignoreNulls"
{-# NOINLINE sIgnoreNulls #-}

sJson :: VarName
sJson = mkVarName "__json"
{-# NOINLINE sJson #-}

sCurPos :: VarName
sCurPos = mkVarName "__curPos"
{-# NOINLINE sCurPos #-}

sCurFile :: VarName
sCurFile = mkVarName "__cur_file"
{-# NOINLINE sCurFile #-}

sIncludes :: VarName
sIncludes = mkVarName "__includes"
{-# NOINLINE sIncludes #-}

sLet :: VarName
sLet = mkVarName "let"
{-# NOINLINE sLet #-}

sIn :: VarName
sIn = mkVarName "in"
{-# NOINLINE sIn #-}

sIf :: VarName
sIf = mkVarName "if"
{-# NOINLINE sIf #-}

sThen :: VarName
sThen = mkVarName "then"
{-# NOINLINE sThen #-}

sElse :: VarName
sElse = mkVarName "else"
{-# NOINLINE sElse #-}

sAssert :: VarName
sAssert = mkVarName "assert"
{-# NOINLINE sAssert #-}

sWith :: VarName
sWith = mkVarName "with"
{-# NOINLINE sWith #-}

sRec :: VarName
sRec = mkVarName "rec"
{-# NOINLINE sRec #-}

sInherit :: VarName
sInherit = mkVarName "inherit"
{-# NOINLINE sInherit #-}

sOr :: VarName
sOr = mkVarName "or"
{-# NOINLINE sOr #-}

sTrue :: VarName
sTrue = mkVarName "true"
{-# NOINLINE sTrue #-}

sFalse :: VarName
sFalse = mkVarName "false"
{-# NOINLINE sFalse #-}

sNull :: VarName
sNull = mkVarName "null"
{-# NOINLINE sNull #-}

sEllipsis :: VarName
sEllipsis = mkVarName "..."
{-# NOINLINE sEllipsis #-}

sText :: VarName
sText = mkVarName "text"
{-# NOINLINE sText #-}

sNix :: VarName
sNix = mkVarName "nix"
{-# NOINLINE sNix #-}

sNixStore :: VarName
sNixStore = mkVarName "nixStore"
{-# NOINLINE sNixStore #-}

sNixPath :: VarName
sNixPath = mkVarName "nixPath"
{-# NOINLINE sNixPath #-}

sNixVersion :: VarName
sNixVersion = mkVarName "nixVersion"
{-# NOINLINE sNixVersion #-}

sCurrentSystem :: VarName
sCurrentSystem = mkVarName "currentSystem"
{-# NOINLINE sCurrentSystem #-}

sCurrentTime :: VarName
sCurrentTime = mkVarName "currentTime"
{-# NOINLINE sCurrentTime #-}

sLangVersion :: VarName
sLangVersion = mkVarName "langVersion"
{-# NOINLINE sLangVersion #-}

sStoreDir :: VarName
sStoreDir = mkVarName "storeDir"
{-# NOINLINE sStoreDir #-}

------------------------------------------------------------------------
-- Builtin function names
------------------------------------------------------------------------

sAbort :: VarName
sAbort = mkVarName "abort"
{-# NOINLINE sAbort #-}

sAdd :: VarName
sAdd = mkVarName "add"
{-# NOINLINE sAdd #-}

sAddDrvOutputDependencies :: VarName
sAddDrvOutputDependencies = mkVarName "addDrvOutputDependencies"
{-# NOINLINE sAddDrvOutputDependencies #-}

sAddErrorContext :: VarName
sAddErrorContext = mkVarName "addErrorContext"
{-# NOINLINE sAddErrorContext #-}

sAny :: VarName
sAny = mkVarName "any"
{-# NOINLINE sAny #-}

sAppendContext :: VarName
sAppendContext = mkVarName "appendContext"
{-# NOINLINE sAppendContext #-}

sAttrNames :: VarName
sAttrNames = mkVarName "attrNames"
{-# NOINLINE sAttrNames #-}

sAttrValues :: VarName
sAttrValues = mkVarName "attrValues"
{-# NOINLINE sAttrValues #-}

sBaseNameOf :: VarName
sBaseNameOf = mkVarName "baseNameOf"
{-# NOINLINE sBaseNameOf #-}

sBitAnd :: VarName
sBitAnd = mkVarName "bitAnd"
{-# NOINLINE sBitAnd #-}

sBitOr :: VarName
sBitOr = mkVarName "bitOr"
{-# NOINLINE sBitOr #-}

sBitXor :: VarName
sBitXor = mkVarName "bitXor"
{-# NOINLINE sBitXor #-}

sBreak :: VarName
sBreak = mkVarName "break"
{-# NOINLINE sBreak #-}

sCatAttrs :: VarName
sCatAttrs = mkVarName "catAttrs"
{-# NOINLINE sCatAttrs #-}

sCeil :: VarName
sCeil = mkVarName "ceil"
{-# NOINLINE sCeil #-}

sCompareVersions :: VarName
sCompareVersions = mkVarName "compareVersions"
{-# NOINLINE sCompareVersions #-}

sConcatLists :: VarName
sConcatLists = mkVarName "concatLists"
{-# NOINLINE sConcatLists #-}

sConcatMap :: VarName
sConcatMap = mkVarName "concatMap"
{-# NOINLINE sConcatMap #-}

sConcatStringsSep :: VarName
sConcatStringsSep = mkVarName "concatStringsSep"
{-# NOINLINE sConcatStringsSep #-}

sConvertHash :: VarName
sConvertHash = mkVarName "convertHash"
{-# NOINLINE sConvertHash #-}

sDeepSeq :: VarName
sDeepSeq = mkVarName "deepSeq"
{-# NOINLINE sDeepSeq #-}

sDirOf :: VarName
sDirOf = mkVarName "dirOf"
{-# NOINLINE sDirOf #-}

sDiv :: VarName
sDiv = mkVarName "div"
{-# NOINLINE sDiv #-}

sElem :: VarName
sElem = mkVarName "elem"
{-# NOINLINE sElem #-}

sElemAt :: VarName
sElemAt = mkVarName "elemAt"
{-# NOINLINE sElemAt #-}

sFetchGit :: VarName
sFetchGit = mkVarName "fetchGit"
{-# NOINLINE sFetchGit #-}

sFetchTarball :: VarName
sFetchTarball = mkVarName "fetchTarball"
{-# NOINLINE sFetchTarball #-}

sFetchTree :: VarName
sFetchTree = mkVarName "fetchTree"
{-# NOINLINE sFetchTree #-}

sFetchurl :: VarName
sFetchurl = mkVarName "fetchurl"
{-# NOINLINE sFetchurl #-}

sFilter :: VarName
sFilter = mkVarName "filter"
{-# NOINLINE sFilter #-}

sFilterSource :: VarName
sFilterSource = mkVarName "filterSource"
{-# NOINLINE sFilterSource #-}

sFindFile :: VarName
sFindFile = mkVarName "findFile"
{-# NOINLINE sFindFile #-}

sFloor :: VarName
sFloor = mkVarName "floor"
{-# NOINLINE sFloor #-}

sFoldl' :: VarName
sFoldl' = mkVarName "foldl'"
{-# NOINLINE sFoldl' #-}

sFromJSON :: VarName
sFromJSON = mkVarName "fromJSON"
{-# NOINLINE sFromJSON #-}

sFromTOML :: VarName
sFromTOML = mkVarName "fromTOML"
{-# NOINLINE sFromTOML #-}

sFunctionArgs :: VarName
sFunctionArgs = mkVarName "functionArgs"
{-# NOINLINE sFunctionArgs #-}

sGenList :: VarName
sGenList = mkVarName "genList"
{-# NOINLINE sGenList #-}

sGenericClosure :: VarName
sGenericClosure = mkVarName "genericClosure"
{-# NOINLINE sGenericClosure #-}

sGetAttr :: VarName
sGetAttr = mkVarName "getAttr"
{-# NOINLINE sGetAttr #-}

sGetContext :: VarName
sGetContext = mkVarName "getContext"
{-# NOINLINE sGetContext #-}

sGetEnv :: VarName
sGetEnv = mkVarName "getEnv"
{-# NOINLINE sGetEnv #-}

sGetFlake :: VarName
sGetFlake = mkVarName "getFlake"
{-# NOINLINE sGetFlake #-}

sGroupBy :: VarName
sGroupBy = mkVarName "groupBy"
{-# NOINLINE sGroupBy #-}

sHasAttr :: VarName
sHasAttr = mkVarName "hasAttr"
{-# NOINLINE sHasAttr #-}

sHasContext :: VarName
sHasContext = mkVarName "hasContext"
{-# NOINLINE sHasContext #-}

sHashFile :: VarName
sHashFile = mkVarName "hashFile"
{-# NOINLINE sHashFile #-}

sHashString :: VarName
sHashString = mkVarName "hashString"
{-# NOINLINE sHashString #-}

sHead :: VarName
sHead = mkVarName "head"
{-# NOINLINE sHead #-}

sImport :: VarName
sImport = mkVarName "import"
{-# NOINLINE sImport #-}

sIntersectAttrs :: VarName
sIntersectAttrs = mkVarName "intersectAttrs"
{-# NOINLINE sIntersectAttrs #-}

sIsAttrs :: VarName
sIsAttrs = mkVarName "isAttrs"
{-# NOINLINE sIsAttrs #-}

sIsBool :: VarName
sIsBool = mkVarName "isBool"
{-# NOINLINE sIsBool #-}

sIsFloat :: VarName
sIsFloat = mkVarName "isFloat"
{-# NOINLINE sIsFloat #-}

sIsFunction :: VarName
sIsFunction = mkVarName "isFunction"
{-# NOINLINE sIsFunction #-}

sIsInt :: VarName
sIsInt = mkVarName "isInt"
{-# NOINLINE sIsInt #-}

sIsList :: VarName
sIsList = mkVarName "isList"
{-# NOINLINE sIsList #-}

sIsNull :: VarName
sIsNull = mkVarName "isNull"
{-# NOINLINE sIsNull #-}

sIsPath :: VarName
sIsPath = mkVarName "isPath"
{-# NOINLINE sIsPath #-}

sIsString :: VarName
sIsString = mkVarName "isString"
{-# NOINLINE sIsString #-}

sLength :: VarName
sLength = mkVarName "length"
{-# NOINLINE sLength #-}

sLessThan :: VarName
sLessThan = mkVarName "lessThan"
{-# NOINLINE sLessThan #-}

sListToAttrs :: VarName
sListToAttrs = mkVarName "listToAttrs"
{-# NOINLINE sListToAttrs #-}

sMap :: VarName
sMap = mkVarName "map"
{-# NOINLINE sMap #-}

sMapAttrs :: VarName
sMapAttrs = mkVarName "mapAttrs"
{-# NOINLINE sMapAttrs #-}

sMatch :: VarName
sMatch = mkVarName "match"
{-# NOINLINE sMatch #-}

sMul :: VarName
sMul = mkVarName "mul"
{-# NOINLINE sMul #-}

sParseDrvName :: VarName
sParseDrvName = mkVarName "parseDrvName"
{-# NOINLINE sParseDrvName #-}

sPartition :: VarName
sPartition = mkVarName "partition"
{-# NOINLINE sPartition #-}

sPathExists :: VarName
sPathExists = mkVarName "pathExists"
{-# NOINLINE sPathExists #-}

sPlaceholder :: VarName
sPlaceholder = mkVarName "placeholder"
{-# NOINLINE sPlaceholder #-}

sReadDir :: VarName
sReadDir = mkVarName "readDir"
{-# NOINLINE sReadDir #-}

sReadFile :: VarName
sReadFile = mkVarName "readFile"
{-# NOINLINE sReadFile #-}

sReadFileType :: VarName
sReadFileType = mkVarName "readFileType"
{-# NOINLINE sReadFileType #-}

sRemoveAttrs :: VarName
sRemoveAttrs = mkVarName "removeAttrs"
{-# NOINLINE sRemoveAttrs #-}

sReplaceStrings :: VarName
sReplaceStrings = mkVarName "replaceStrings"
{-# NOINLINE sReplaceStrings #-}

sScopedImport :: VarName
sScopedImport = mkVarName "scopedImport"
{-# NOINLINE sScopedImport #-}

sSeq :: VarName
sSeq = mkVarName "seq"
{-# NOINLINE sSeq #-}

sSort :: VarName
sSort = mkVarName "sort"
{-# NOINLINE sSort #-}

sSplit :: VarName
sSplit = mkVarName "split"
{-# NOINLINE sSplit #-}

sSplitVersion :: VarName
sSplitVersion = mkVarName "splitVersion"
{-# NOINLINE sSplitVersion #-}

sStorePath :: VarName
sStorePath = mkVarName "storePath"
{-# NOINLINE sStorePath #-}

sStringLength :: VarName
sStringLength = mkVarName "stringLength"
{-# NOINLINE sStringLength #-}

sSub :: VarName
sSub = mkVarName "sub"
{-# NOINLINE sSub #-}

sSubstring :: VarName
sSubstring = mkVarName "substring"
{-# NOINLINE sSubstring #-}

sTail :: VarName
sTail = mkVarName "tail"
{-# NOINLINE sTail #-}

sThrow :: VarName
sThrow = mkVarName "throw"
{-# NOINLINE sThrow #-}

sToFile :: VarName
sToFile = mkVarName "toFile"
{-# NOINLINE sToFile #-}

sToJSON :: VarName
sToJSON = mkVarName "toJSON"
{-# NOINLINE sToJSON #-}

sToPath :: VarName
sToPath = mkVarName "toPath"
{-# NOINLINE sToPath #-}

sToXML :: VarName
sToXML = mkVarName "toXML"
{-# NOINLINE sToXML #-}

sTrace :: VarName
sTrace = mkVarName "trace"
{-# NOINLINE sTrace #-}

sTraceVerbose :: VarName
sTraceVerbose = mkVarName "traceVerbose"
{-# NOINLINE sTraceVerbose #-}

sTryEval :: VarName
sTryEval = mkVarName "tryEval"
{-# NOINLINE sTryEval #-}

sTypeOf :: VarName
sTypeOf = mkVarName "typeOf"
{-# NOINLINE sTypeOf #-}

sUnsafeDiscardOutputDependency :: VarName
sUnsafeDiscardOutputDependency = mkVarName "unsafeDiscardOutputDependency"
{-# NOINLINE sUnsafeDiscardOutputDependency #-}

sUnsafeDiscardStringContext :: VarName
sUnsafeDiscardStringContext = mkVarName "unsafeDiscardStringContext"
{-# NOINLINE sUnsafeDiscardStringContext #-}

sUnsafeGetAttrPos :: VarName
sUnsafeGetAttrPos = mkVarName "unsafeGetAttrPos"
{-# NOINLINE sUnsafeGetAttrPos #-}

sValueSize :: VarName
sValueSize = mkVarName "valueSize"
{-# NOINLINE sValueSize #-}

sZipAttrsWith :: VarName
sZipAttrsWith = mkVarName "zipAttrsWith"
{-# NOINLINE sZipAttrsWith #-}

sCurrentPos :: VarName
sCurrentPos = mkVarName "__curPos"
{-# NOINLINE sCurrentPos #-}

sReadFiletype :: VarName
sReadFiletype = mkVarName "readFiletype"
{-# NOINLINE sReadFiletype #-}

sFromJSONFile :: VarName
sFromJSONFile = mkVarName "fromJSONFile"
{-# NOINLINE sFromJSONFile #-}

------------------------------------------------------------------------
-- Other common names
------------------------------------------------------------------------

sRight :: VarName
sRight = mkVarName "right"
{-# NOINLINE sRight #-}

sWrong :: VarName
sWrong = mkVarName "wrong"
{-# NOINLINE sWrong #-}

sRegular :: VarName
sRegular = mkVarName "regular"
{-# NOINLINE sRegular #-}

sDirectory :: VarName
sDirectory = mkVarName "directory"
{-# NOINLINE sDirectory #-}

sSymlink :: VarName
sSymlink = mkVarName "symlink"
{-# NOINLINE sSymlink #-}

sUnknown :: VarName
sUnknown = mkVarName "unknown"
{-# NOINLINE sUnknown #-}

sSubPath :: VarName
sSubPath = mkVarName "subPath"
{-# NOINLINE sSubPath #-}

sNar :: VarName
sNar = mkVarName "nar"
{-# NOINLINE sNar #-}

sFlat :: VarName
sFlat = mkVarName "flat"
{-# NOINLINE sFlat #-}

------------------------------------------------------------------------
-- Git/fetch related names
------------------------------------------------------------------------

sNixPathVar :: VarName
sNixPathVar = mkVarName "__nixPath"
{-# NOINLINE sNixPathVar #-}

sNarHash :: VarName
sNarHash = mkVarName "narHash"
{-# NOINLINE sNarHash #-}

sSubmodules :: VarName
sSubmodules = mkVarName "submodules"
{-# NOINLINE sSubmodules #-}

sRev :: VarName
sRev = mkVarName "rev"
{-# NOINLINE sRev #-}

sShortRev :: VarName
sShortRev = mkVarName "shortRev"
{-# NOINLINE sShortRev #-}

sRevCount :: VarName
sRevCount = mkVarName "revCount"
{-# NOINLINE sRevCount #-}

sLastModified :: VarName
sLastModified = mkVarName "lastModified"
{-# NOINLINE sLastModified #-}

sLastModifiedDate :: VarName
sLastModifiedDate = mkVarName "lastModifiedDate"
{-# NOINLINE sLastModifiedDate #-}

sRef :: VarName
sRef = mkVarName "ref"
{-# NOINLINE sRef #-}

sShallow :: VarName
sShallow = mkVarName "shallow"
{-# NOINLINE sShallow #-}

sAllRefs :: VarName
sAllRefs = mkVarName "allRefs"
{-# NOINLINE sAllRefs #-}

sDirtyRev :: VarName
sDirtyRev = mkVarName "dirtyRev"
{-# NOINLINE sDirtyRev #-}

sDirtyShortRev :: VarName
sDirtyShortRev = mkVarName "dirtyShortRev"
{-# NOINLINE sDirtyShortRev #-}
