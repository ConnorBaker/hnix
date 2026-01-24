{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStrict #-}

-- | Built-in functions for the compiled Nix runtime.
--
-- This module implements the ~100 built-in functions available in Nix.
-- Each builtin is a Haskell function of type @NixValue -> NixValue@ (for
-- single-argument builtins) or curried for multi-argument ones.
--
-- Many builtins are partial applications that return VBuiltin for currying.
-- For example, @builtins.map f@ returns a VBuiltin that, when applied to
-- a list, returns the mapped list.
--
-- Implementations are organized into submodules by category:
-- - Nix.Compile.Builtins.Arithmetic - add, sub, mul, div, etc.
-- - Nix.Compile.Builtins.AttrSet - attrNames, attrValues, hasAttr, etc.
-- - Nix.Compile.Builtins.Control - throw, abort, tryEval, seq, etc.
-- - Nix.Compile.Builtins.IO - import, readFile, readDir, etc.
-- - Nix.Compile.Builtins.List - length, head, tail, map, filter, etc.
-- - Nix.Compile.Builtins.String - substring, replaceStrings, match, etc.
-- - Nix.Compile.Builtins.System - currentSystem, langVersion, etc.
-- - Nix.Compile.Builtins.Type - isNull, isInt, typeOf, toJSON, etc.
module Nix.Compile.Builtins
  ( -- * The builtins attribute set
    builtinsAttrSet
    -- * List builtins (re-exported from Builtins.List)
  , builtinLength
  , builtinHead
  , builtinTail
  , builtinElemAt
  , builtinElem
  , builtinFilter
  , builtinMap
  , builtinFoldl
  , builtinConcatLists
  , builtinGenList
  , builtinSort
  , builtinAll
  , builtinAny
  , builtinPartition
  , builtinGroupBy
  , builtinConcatMap
  , builtinListToAttrs
  , builtinReverse
  , builtinGenAttrs
    -- * Attribute set builtins (re-exported from Builtins.AttrSet)
  , builtinAttrNames
  , builtinAttrValues
  , builtinHasAttr
  , builtinGetAttr
  , builtinRemoveAttrs
  , builtinMapAttrs
  , builtinIntersectAttrs
  , builtinCatAttrs
  , builtinFunctionArgs
  , builtinZipAttrsWith
    -- * Type predicates (re-exported from Builtins.Type)
  , builtinIsNull
  , builtinIsInt
  , builtinIsFloat
  , builtinIsBool
  , builtinIsString
  , builtinIsList
  , builtinIsAttrs
  , builtinIsFunction
  , builtinIsPath
  , builtinTypeOf
    -- * Type conversion (re-exported from Builtins.Type)
  , builtinToString
  , builtinToInt
  , builtinToFloat
  , builtinToPath
  , builtinToJSON
  , builtinFromJSON
    -- * Path builtins (defined here)
  , builtinBaseNameOf
  , builtinDirOf
    -- * String builtins (re-exported from Builtins.String)
  , builtinSubstring
  , builtinStringLength
  , builtinReplaceStrings
  , builtinSplit
  , builtinMatch
  , builtinConcatStringsSep
  , builtinHashString
  , builtinCompareVersions
  , builtinSplitVersion
  , builtinUnsafeDiscardStringContext
  , builtinAddContextFrom
  , builtinHasContext
  , builtinGetContext
  , builtinAppendContext
    -- * Arithmetic builtins (re-exported from Builtins.Arithmetic)
  , builtinAdd
  , builtinSub
  , builtinMul
  , builtinDiv
  , builtinFloor
  , builtinCeil
  , builtinBitAnd
  , builtinBitOr
  , builtinBitXor
  , builtinLessThan
    -- * Control flow (re-exported from Builtins.Control)
  , builtinThrow
  , builtinAbort
  , builtinTryEval
  , builtinSeq
  , builtinDeepSeq
  , builtinTrace
    -- * Special values (defined here)
  , builtinTrue
  , builtinFalse
  , builtinNull
    -- * System info (re-exported from Builtins.System)
  , builtinCurrentSystem
  , builtinLangVersion
  , builtinNixVersion
    -- * IO builtins (re-exported from Builtins.IO)
  , builtinImport
  , builtinImportGlobal
  , builtinReadFile
  , builtinReadDir
  , builtinPathExists
  , builtinGetEnv
  , builtinDerivation
  , builtinToFile
  , builtinFilterSource
  , builtinPath
  , builtinFetchurl
  , builtinFetchTarball
  , builtinFetchGit
    -- * Global builtins (VBuiltin-wrapped for top-level exposure)
  , globalToString
  , globalThrow
  , globalAbort
  , globalIsNull
  , globalBaseNameOf
  , globalDirOf
  , globalImport
  ) where

import Relude
import Nix.Types.Path (Path, takeFileName, takeDirectory)
import Nix.Compile.Value

-- Import all submodule implementations
import Nix.Compile.Builtins.Arithmetic
import Nix.Compile.Builtins.AttrSet hiding (builtinListToAttrs, builtinGenAttrs)
import Nix.Compile.Builtins.Control
import Nix.Compile.Builtins.IO
import Nix.Compile.Builtins.List
import Nix.Compile.Builtins.String
import Nix.Compile.Builtins.System
import Nix.Compile.Builtins.Type

-- * Constants

builtinTrue :: NixValue
builtinTrue = VBool True
{-# NOINLINE builtinTrue #-}

builtinFalse :: NixValue
builtinFalse = VBool False
{-# NOINLINE builtinFalse #-}

builtinNull :: NixValue
builtinNull = VNull
{-# NOINLINE builtinNull #-}

-- * Path builtins
--
-- These are defined here because they don't belong to any specific submodule.

-- | Get the base name of a path or string (like basename(1)).
-- Returns the filename component of the path.
builtinBaseNameOf :: NixValue -> NixValue
builtinBaseNameOf (VPath p) = VString (toText (takeFileName p)) emptyContext
builtinBaseNameOf (VString s _) =
  let p = fromString (toString s) :: Path
  in VString (toText (takeFileName p)) emptyContext
builtinBaseNameOf v = throwNixError $ TypeError "a path or string" (valueTypeName v)
{-# INLINE builtinBaseNameOf #-}

-- | Get the directory component of a path or string (like dirname(1)).
-- For paths, returns a path. For strings, returns a string.
builtinDirOf :: NixValue -> NixValue
builtinDirOf (VPath p) = VPath (takeDirectory p)
builtinDirOf (VString s _) =
  let p = fromString (toString s) :: Path
  in VString (toText (takeDirectory p)) emptyContext
builtinDirOf v = throwNixError $ TypeError "a path or string" (valueTypeName v)
{-# INLINE builtinDirOf #-}

-- * Global builtins (VBuiltin-wrapped versions for top-level exposure)
--
-- These are the builtins that are exposed at the top level (without the
-- builtins. prefix) in Nix. They need to be wrapped as VBuiltin so they
-- can be used as values (passed to higher-order functions, etc.).

-- | Global toString builtin
globalToString :: NixValue
globalToString = VBuiltin "toString" builtinToString
{-# NOINLINE globalToString #-}

-- | Global throw builtin
globalThrow :: NixValue
globalThrow = VBuiltin "throw" builtinThrow
{-# NOINLINE globalThrow #-}

-- | Global abort builtin
globalAbort :: NixValue
globalAbort = VBuiltin "abort" builtinAbort
{-# NOINLINE globalAbort #-}

-- | Global isNull builtin
globalIsNull :: NixValue
globalIsNull = VBuiltin "isNull" builtinIsNull
{-# NOINLINE globalIsNull #-}

-- | Global baseNameOf builtin
globalBaseNameOf :: NixValue
globalBaseNameOf = VBuiltin "baseNameOf" builtinBaseNameOf
{-# NOINLINE globalBaseNameOf #-}

-- | Global dirOf builtin
globalDirOf :: NixValue
globalDirOf = VBuiltin "dirOf" builtinDirOf
{-# NOINLINE globalDirOf #-}

-- | Global import builtin
-- Note: This uses builtinImportGlobal which is a stub.
-- The compiler generates special code for import that handles the NixEnv.
globalImport :: NixValue
globalImport = builtinImportGlobal
{-# NOINLINE globalImport #-}

-- * The builtins attribute set

-- | The complete builtins attribute set.
-- This is exposed as @builtins@ in the Nix evaluation environment.
builtinsAttrSet :: NixValue
builtinsAttrSet = VAttrs $ attrsFromList
  [ -- List operations
    ("length", VBuiltin "length" builtinLength)
  , ("head", VBuiltin "head" builtinHead)
  , ("tail", VBuiltin "tail" builtinTail)
  , ("elemAt", builtinElemAt)
  , ("elem", builtinElem)
  , ("filter", builtinFilter)
  , ("map", builtinMap)
  , ("foldl'", builtinFoldl)
  , ("concatLists", VBuiltin "concatLists" builtinConcatLists)
  , ("genList", builtinGenList)
  , ("sort", builtinSort)
  , ("all", builtinAll)
  , ("any", builtinAny)
  , ("partition", builtinPartition)
  , ("groupBy", builtinGroupBy)
  , ("concatMap", builtinConcatMap)
  , ("listToAttrs", VBuiltin "listToAttrs" builtinListToAttrs)
  , ("reverse", VBuiltin "reverse" builtinReverse)
  , ("genAttrs", builtinGenAttrs)
    -- Attribute set operations
  , ("attrNames", VBuiltin "attrNames" builtinAttrNames)
  , ("attrValues", VBuiltin "attrValues" builtinAttrValues)
  , ("hasAttr", builtinHasAttr)
  , ("getAttr", builtinGetAttr)
  , ("removeAttrs", builtinRemoveAttrs)
  , ("mapAttrs", builtinMapAttrs)
  , ("intersectAttrs", builtinIntersectAttrs)
  , ("catAttrs", builtinCatAttrs)
  , ("functionArgs", VBuiltin "functionArgs" builtinFunctionArgs)
  , ("zipAttrsWith", builtinZipAttrsWith)
    -- Type predicates
  , ("isNull", VBuiltin "isNull" builtinIsNull)
  , ("isInt", VBuiltin "isInt" builtinIsInt)
  , ("isFloat", VBuiltin "isFloat" builtinIsFloat)
  , ("isBool", VBuiltin "isBool" builtinIsBool)
  , ("isString", VBuiltin "isString" builtinIsString)
  , ("isList", VBuiltin "isList" builtinIsList)
  , ("isAttrs", VBuiltin "isAttrs" builtinIsAttrs)
  , ("isFunction", VBuiltin "isFunction" builtinIsFunction)
  , ("isPath", VBuiltin "isPath" builtinIsPath)
  , ("typeOf", VBuiltin "typeOf" builtinTypeOf)
    -- Type conversions
  , ("toString", VBuiltin "toString" builtinToString)
  , ("toInt", VBuiltin "toInt" builtinToInt)
  , ("toFloat", VBuiltin "toFloat" builtinToFloat)
  , ("toPath", VBuiltin "toPath" builtinToPath)
  , ("toJSON", VBuiltin "toJSON" builtinToJSON)
  , ("fromJSON", VBuiltin "fromJSON" builtinFromJSON)
    -- Path operations
  , ("baseNameOf", VBuiltin "baseNameOf" builtinBaseNameOf)
  , ("dirOf", VBuiltin "dirOf" builtinDirOf)
    -- String operations
  , ("substring", builtinSubstring)
  , ("stringLength", VBuiltin "stringLength" builtinStringLength)
  , ("replaceStrings", builtinReplaceStrings)
  , ("split", builtinSplit)
  , ("match", builtinMatch)
  , ("concatStringsSep", builtinConcatStringsSep)
  , ("hashString", builtinHashString)
  , ("compareVersions", builtinCompareVersions)
  , ("splitVersion", VBuiltin "splitVersion" builtinSplitVersion)
  , ("unsafeDiscardStringContext", VBuiltin "unsafeDiscardStringContext" builtinUnsafeDiscardStringContext)
  , ("addContextFrom", builtinAddContextFrom)
  , ("hasContext", VBuiltin "hasContext" builtinHasContext)
  , ("getContext", VBuiltin "getContext" builtinGetContext)
  , ("appendContext", builtinAppendContext)
    -- Arithmetic
  , ("add", builtinAdd)
  , ("sub", builtinSub)
  , ("mul", builtinMul)
  , ("div", builtinDiv)
  , ("floor", VBuiltin "floor" builtinFloor)
  , ("ceil", VBuiltin "ceil" builtinCeil)
  , ("bitAnd", builtinBitAnd)
  , ("bitOr", builtinBitOr)
  , ("bitXor", builtinBitXor)
  , ("lessThan", builtinLessThan)
    -- Control flow
  , ("throw", VBuiltin "throw" builtinThrow)
  , ("abort", VBuiltin "abort" builtinAbort)
  , ("tryEval", VBuiltin "tryEval" builtinTryEval)
  , ("seq", builtinSeq)
  , ("deepSeq", builtinDeepSeq)
  , ("trace", builtinTrace)
    -- Constants
  , ("true", builtinTrue)
  , ("false", builtinFalse)
  , ("null", builtinNull)
    -- System info
  , ("currentSystem", builtinCurrentSystem)
  , ("langVersion", builtinLangVersion)
  , ("nixVersion", builtinNixVersion)
    -- Store paths
  , ("storeDir", VString "/nix/store" emptyContext)
    -- IO operations
  , ("import", builtinImportGlobal)
  , ("readFile", builtinReadFile)
  , ("readDir", builtinReadDir)
  , ("pathExists", builtinPathExists)
  , ("getEnv", builtinGetEnv)
    -- Store operations (stubs)
  , ("derivation", builtinDerivation)
  , ("toFile", builtinToFile)
  , ("filterSource", builtinFilterSource)
  , ("path", builtinPath)
    -- Fetch operations (stubs)
  , ("fetchurl", builtinFetchurl)
  , ("fetchTarball", builtinFetchTarball)
  , ("fetchGit", builtinFetchGit)
  ]
{-# NOINLINE builtinsAttrSet #-}
