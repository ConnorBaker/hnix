{-# LANGUAGE NoStrict #-}

-- | String operations for the compiled Nix runtime.
--
-- These functions implement Nix string operations including concatenation,
-- type coercion, and path conversion. Error handling uses Haskell exceptions
-- (NixError) which propagate through the generated code.
--
-- Design principles:
-- * Match Nix semantics exactly (context handling, type coercion rules)
-- * Throw NixError on type mismatches rather than returning Maybe
-- * INLINE aggressively - these are hot paths
-- * No thunk management - GHC handles that
module Nix.Compile.Primops.String
  ( -- * String operations
    nixStringConcat
  , nixCoerceToString
  , stringToPath
  ) where

import Relude hiding (empty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import Nix.Compile.Value
import Nix.Compile.Primops.Coerce (expectString, expectFunction)

-- * String operations

-- | String concatenation. Combines contexts.
nixStringConcat :: NixValue -> NixValue -> NixValue
nixStringConcat v1 v2 =
  let (t1, ctx1) = expectString v1
      (t2, ctx2) = expectString v2
  in VString (t1 <> t2) (unionContext ctx1 ctx2)
{-# INLINE nixStringConcat #-}

-- | Coerce a value to string for string interpolation and builtins.toString.
-- Follows Nix's coercion rules:
-- * Strings: unchanged
-- * Paths: convert to absolute path string (preserving path as context)
-- * Integers/Floats: convert to decimal representation
-- * Booleans: true -> "1", false -> ""
-- * null: empty string ""
-- * Lists: space-separated coerced elements
-- * Sets with __toString: call __toString on the set
-- * Sets with outPath: coerce outPath
-- * Other sets/functions: error (not coercible)
nixCoerceToString :: NixValue -> NixValue
nixCoerceToString v@(VString _ _) = v
nixCoerceToString (VPath p) =
  let pathText = toText p
  in VString pathText (pathContext pathText)
nixCoerceToString (VInt n) = VString (show n) emptyContext
nixCoerceToString (VFloat n) = VString (show n) emptyContext  -- TODO: proper Nix float formatting
nixCoerceToString (VBool True) = VString "1" emptyContext
nixCoerceToString (VBool False) = VString "" emptyContext
nixCoerceToString VNull = VString "" emptyContext
nixCoerceToString (VList lst) =
  -- Coerce each element and join with spaces
  let coerceElement x =
        let coerced = nixCoerceToString x
        in case coerced of
             VString t _ -> t
             _ -> throwNixError $ CoercionError (valueTypeName x) "a string"
      parts = V.map coerceElement lst
  in VString (T.intercalate " " (V.toList parts)) emptyContext
nixCoerceToString (VAttrs as) =
  -- Try __toString first, then outPath
  case lookupAttr (mkVarNameStr "__toString") as of
    Just fn ->
      -- __toString is a function that takes the set itself
      let result = expectFunction fn (VAttrs as)
      in nixCoerceToString result
    Nothing ->
      case lookupAttr (mkVarNameStr "outPath") as of
        Just op -> nixCoerceToString op
        Nothing -> throwNixError $ CoercionError "a set" "a string (no __toString or outPath)"
nixCoerceToString v = throwNixError $ CoercionError (valueTypeName v) "a string"
{-# INLINE nixCoerceToString #-}

-- | Convert a string to a path.
-- Used for path interpolation expressions like ./foo/${bar}.
stringToPath :: NixValue -> NixValue
stringToPath (VString s _) = VPath (fromString (toString s))
stringToPath v = throwNixError $ TypeError "a string" (valueTypeName v)
{-# INLINE stringToPath #-}
