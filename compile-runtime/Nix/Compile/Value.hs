{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoStrict #-}

-- | Runtime value types for the NExpr to GHC Core compiler.
--
-- This module defines the runtime representation of Nix values used by
-- generated GHC Core code. Unlike the interpreter's NValue type which uses
-- a free monad over a base functor with custom thunk handling, this
-- representation is designed to work directly with GHC's native thunking
-- and garbage collection.
--
-- The key difference: GHC handles all thunk creation, forcing, and memory
-- management. We just define a simple sum type that Core can manipulate.
--
-- IMPORTANT: This module is intentionally self-contained and does NOT depend
-- on hnix-core or any Backpack signatures. This makes hnix-compile-runtime
-- a definite library that GHC can load at runtime.
module Nix.Compile.Value
  ( -- * Value types
    NixValue(..)
  , NixAttrs(..)
  , NixContext
  , NixEnv(..)
    -- * Context types
  , ContextFlavor(..)
  , StringContext(..)
    -- * Parameter types (self-contained, no Backpack dependency)
  , RuntimeParams(..)
  , RuntimeVariadic(..)
    -- * NIX_PATH types
  , NixPathEntry(..)
  , NixPath(..)
  , emptyNixPath
  , parseNixPath
    -- * Errors (re-exported from Nix.Compile.Value.Error)
  , module Nix.Compile.Value.Error
    -- * NixAttrs operations
  , emptyAttrs
  , lookupAttr
  , insertAttr
  , insertAtPathDynamic
  , deleteAttr
  , unionAttrs
  , unionAttrsWithKey
  , attrKeys
  , attrValues
  , attrToList
  , attrsFromList
  , attrsSize
  , attrsNull
    -- * NixEnv operations
  , emptyEnv
  , mkEnvWithCurrentFile
  , pushWithScope
  , lookupWithScopes
  , lookupWithScopesOrThrow
    -- * Context operations (re-exported from Nix.Compile.Value.Context)
  , module Nix.Compile.Value.Context
    -- * Value-level context check (defined here, uses NixValue)
  , hasContextValue
    -- * Value predicates
  , isNull
  , isInt
  , isFloat
  , isBool
  , isString
  , isList
  , isAttrs
  , isFunction
  , isPath
    -- * Value type names
  , valueTypeName
    -- * VarName helpers for generated code
  , mkVarNameStr
  , mkVarNameFromValue
    -- * NonEmpty helpers for generated code
  , mkNonEmptyVarName
    -- * Text helpers for generated code
  , mkText
    -- * Path helpers for generated code
  , mkPath
  ) where

import Relude hiding (empty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Text.Show as Show
import Nix.Types.VarName (VarName)
import Nix.Types.Path (Path)
import Nix.Compile.Value.Context
import Nix.Compile.Value.Error

-- * RuntimeParams (self-contained, no Backpack dependency)

-- | Whether a parameter set accepts extra arguments (...).
data RuntimeVariadic
  = RuntimeClosed
    -- ^ Exact match required: { x, y }
  | RuntimeVariadic
    -- ^ Extra arguments allowed: { x, y, ... }
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- | Simplified parameter representation for closures.
--
-- This is a self-contained type that captures the essential parameter
-- information without depending on Backpack signatures. Unlike the full
-- Params type in hnix-core which uses AttrSet, this stores parameters
-- as a simple list.
data RuntimeParams
  = RuntimeParam !VarName
    -- ^ Single parameter function: @x: body@
  | RuntimeParamSet !(Maybe VarName) !RuntimeVariadic ![(VarName, Bool)]
    -- ^ Pattern parameter function: @{ x, y ? default, ... }@name: body@
    -- The Maybe VarName is the optional @-binding (e.g., @args@{ ... }@)
    -- The Bool indicates whether each parameter has a default value
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- * NIX_PATH types

-- | A single entry in NIX_PATH.
data NixPathEntry
  = NixPathPrefix !Text !Path
    -- ^ "prefix=path" mapping (e.g., "nixpkgs=/nix/store/...")
  | NixPathPlain !Path
    -- ^ Plain path (searched by name)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | The NIX_PATH environment, a list of search paths.
newtype NixPath = NixPath [NixPathEntry]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Empty NIX_PATH
emptyNixPath :: NixPath
emptyNixPath = NixPath []
{-# NOINLINE emptyNixPath #-}

-- | Parse NIX_PATH environment variable format.
-- Format: "prefix1=/path1:prefix2=/path2:/plain/path"
parseNixPath :: Text -> NixPath
parseNixPath t = NixPath $ mapMaybe parseEntry (T.splitOn ":" t)
  where
    parseEntry :: Text -> Maybe NixPathEntry
    parseEntry entry
      | T.null entry = Nothing
      | T.isInfixOf "=" entry =
          let (prefix, rest) = T.breakOn "=" entry
              path = T.drop 1 rest  -- Drop the '='
          in if T.null path
             then Nothing
             else Just $ NixPathPrefix prefix (fromString $ toString path)
      | otherwise = Just $ NixPathPlain (fromString $ toString entry)

-- * NixValue

-- | Runtime representation of Nix values.
--
-- This is the sum type that all evaluated Nix expressions reduce to.
-- GHC handles thunking natively - any NixValue field can be a thunk
-- that gets forced on demand.
--
-- Design notes:
-- * Int64 and Double are unpacked for cache efficiency
-- * Bool uses the native Haskell Bool (not boxed)
-- * Strings carry context for store path tracking
-- * Closures capture their function directly - GHC handles closure conversion
-- * Builtins are named functions for error messages
data NixValue
  = VInt {-# UNPACK #-} !Int64
    -- ^ 64-bit signed integer with checked arithmetic (per Nix semantics)
  | VFloat {-# UNPACK #-} !Double
    -- ^ IEEE 754 double-precision floating point
  | VBool !Bool
    -- ^ Boolean value (true/false)
  | VNull
    -- ^ The null value
  | VString !Text !NixContext
    -- ^ String with derivation context for store path tracking
  | VPath !Path
    -- ^ Filesystem path (absolute or relative to source file)
  | VList !(Vector NixValue)
    -- ^ Lazy list of values. Vector gives O(1) indexing and length.
  | VAttrs !NixAttrs
    -- ^ Attribute set (record/dictionary)
  | VClosure !RuntimeParams !(NixValue -> NixValue)
    -- ^ Lambda closure. RuntimeParams stores the parameter pattern for error messages.
    -- The function is a regular Haskell function - GHC handles closure capture.
  | VBuiltin !VarName !(NixValue -> NixValue)
    -- ^ Built-in function with name for error messages.
    -- Partial applications of multi-arg builtins create nested VBuiltins.
  deriving (Generic)

-- | Show instance for debugging. Functions show their arity/name only.
instance Show NixValue where
  showsPrec d v = Show.showParen (d > 10) $ case v of
    VInt n -> Show.showString "VInt " . Show.showsPrec 11 n
    VFloat n -> Show.showString "VFloat " . Show.showsPrec 11 n
    VBool b -> Show.showString "VBool " . Show.showsPrec 11 b
    VNull -> Show.showString "VNull"
    VString t ctx ->
      let ctxSize = HS.size ctx
          ctxInfo = case ctxSize of
            0 -> ""
            1 -> " (1 context: " ++ show (HS.toList ctx) ++ ")"
            n -> " (" ++ show n ++ " contexts)"
      in Show.showString "VString " . Show.showsPrec 11 t . Show.showString ctxInfo
    VPath p -> Show.showString "VPath " . Show.showsPrec 11 p
    VList xs -> Show.showString "VList " . Show.showsPrec 11 (V.toList xs)
    VAttrs as -> Show.showString "VAttrs " . Show.showsPrec 11 as
    VClosure params _ -> Show.showString "VClosure " . Show.showsPrec 11 params
    VBuiltin name _ -> Show.showString "VBuiltin " . Show.showsPrec 11 name

-- | NFData for forced evaluation. Functions can't be fully normalized.
instance NFData NixValue where
  rnf (VInt n) = rnf n
  rnf (VFloat n) = rnf n
  rnf (VBool b) = rnf b
  rnf VNull = ()
  rnf (VString t ctx) = rnf t `seq` rnf ctx
  rnf (VPath p) = rnf p
  rnf (VList v) = rnf v
  rnf (VAttrs as) = rnf as
  rnf (VClosure params _) = rnf params
  rnf (VBuiltin name _) = rnf name

-- * NixAttrs

-- | Attribute set implemented using HashMap for O(1) operations.
--
-- This gives us:
-- * O(1) lookup, insert, delete (average case)
-- * O(n) iteration (for attrNames, mapAttrs, etc.)
-- * Preserves key names for error messages and attrNames
newtype NixAttrs = NixAttrs (HashMap VarName NixValue)
  deriving stock (Generic)

instance Show NixAttrs where
  showsPrec d (NixAttrs m) = Show.showParen (d > 10) $
    Show.showString "NixAttrs {" .
    foldr (.) id
      [ Show.showsPrec 0 k . Show.showString " = " . Show.showsPrec 0 v . Show.showString "; "
      | (k, v) <- HM.toList m
      ] .
    Show.showString "}"

instance NFData NixAttrs where
  rnf (NixAttrs m) = rnf (HM.toList m)

instance Eq NixAttrs where
  NixAttrs m1 == NixAttrs m2 =
    HM.size m1 == HM.size m2 &&
    all (\(k, v1) -> case HM.lookup k m2 of
           Just v2 -> nixEqValue v1 v2
           Nothing -> False)
        (HM.toList m1)

-- | Value equality check (avoiding Show constraint)
nixEqValue :: NixValue -> NixValue -> Bool
nixEqValue (VInt a) (VInt b) = a == b
nixEqValue (VFloat a) (VFloat b) = a == b
nixEqValue (VBool a) (VBool b) = a == b
nixEqValue VNull VNull = True
nixEqValue (VString t1 _) (VString t2 _) = t1 == t2
nixEqValue (VPath p1) (VPath p2) = p1 == p2
nixEqValue (VList l1) (VList l2) = V.length l1 == V.length l2 && V.and (V.zipWith nixEqValue l1 l2)
nixEqValue (VAttrs a1) (VAttrs a2) = a1 == a2
nixEqValue _ _ = False  -- Functions and different types are not equal

-- | Empty attribute set
emptyAttrs :: NixAttrs
emptyAttrs = NixAttrs HM.empty
{-# NOINLINE emptyAttrs #-}

-- | Lookup an attribute by name
lookupAttr :: VarName -> NixAttrs -> Maybe NixValue
lookupAttr name (NixAttrs m) = HM.lookup name m
{-# INLINE lookupAttr #-}

-- | Insert an attribute
insertAttr :: VarName -> NixValue -> NixAttrs -> NixAttrs
insertAttr name val (NixAttrs m) = NixAttrs (HM.insert name val m)
{-# INLINE insertAttr #-}

-- | Delete an attribute
deleteAttr :: VarName -> NixAttrs -> NixAttrs
deleteAttr name (NixAttrs m) = NixAttrs (HM.delete name m)
{-# INLINE deleteAttr #-}

-- | Union two attribute sets. Right-biased (second argument wins on conflict).
unionAttrs :: NixAttrs -> NixAttrs -> NixAttrs
unionAttrs (NixAttrs m1) (NixAttrs m2) = NixAttrs (HM.union m2 m1)  -- Note: HM.union prefers first arg
{-# INLINE unionAttrs #-}

-- | Union with a combining function
unionAttrsWithKey :: (VarName -> NixValue -> NixValue -> NixValue) -> NixAttrs -> NixAttrs -> NixAttrs
unionAttrsWithKey f (NixAttrs m1) (NixAttrs m2) =
  NixAttrs (HM.unionWithKey f m1 m2)

-- | Get all attribute names (sorted for determinism)
attrKeys :: NixAttrs -> [VarName]
attrKeys (NixAttrs m) = sort $ HM.keys m

-- | Get all attribute values (in key-sorted order)
attrValues :: NixAttrs -> [NixValue]
attrValues attrs = map snd (attrToList attrs)

-- | Convert to association list (sorted by key for determinism)
attrToList :: NixAttrs -> [(VarName, NixValue)]
attrToList (NixAttrs m) = sortOn fst $ HM.toList m

-- | Create from association list
attrsFromList :: [(VarName, NixValue)] -> NixAttrs
attrsFromList pairs = NixAttrs (HM.fromList pairs)
{-# INLINE attrsFromList #-}

-- | Insert a value at a dynamic path within an attribute set.
-- The path is a Vector of NixValues (expected to be strings) that define
-- the nested attribute path. Used for patterns like { a.${k}.c = 1; }.
insertAtPathDynamic :: Vector NixValue -> NixValue -> NixAttrs -> NixAttrs
insertAtPathDynamic pathVec val attrs =
  let path = V.toList pathVec
  in case path of
    [] -> throwNixError $ ThrownError "insertAtPathDynamic: empty path"
    [k] ->
      -- Base case: single key, just insert
      let keyName = mkVarNameFromValue k
      in insertAttr keyName val attrs
    (k:ks) ->
      -- Recursive case: navigate/create nested attrset
      let keyName = mkVarNameFromValue k
          inner = case lookupAttr keyName attrs of
            Just (VAttrs existing) ->
              -- Existing attrset: recurse into it
              VAttrs $ insertAtPathDynamic (V.fromList ks) val existing
            Just other ->
              -- Not an attrset: error
              throwNixError $ TypeError "a set" (valueTypeName other)
            Nothing ->
              -- Missing: create new nested attrset
              VAttrs $ insertAtPathDynamic (V.fromList ks) val emptyAttrs
      in insertAttr keyName inner attrs
{-# NOINLINE insertAtPathDynamic #-}

-- | Number of attributes
attrsSize :: NixAttrs -> Int
attrsSize (NixAttrs m) = HM.size m
{-# INLINE attrsSize #-}

-- | Check if attribute set is empty
attrsNull :: NixAttrs -> Bool
attrsNull (NixAttrs m) = HM.null m
{-# INLINE attrsNull #-}

-- * Context types
-- Re-exported from Nix.Compile.Value.Context

-- | Check if a value has string context.
hasContextValue :: NixValue -> Bool
hasContextValue (VString _ ctx) = hasContext ctx
hasContextValue _ = False
{-# INLINE hasContextValue #-}

-- * NixEnv

-- | Dynamic scoping environment for 'with' expressions and NIX_PATH.
--
-- Nix has lexical scoping for normal variables, but 'with' introduces
-- dynamic scoping: @with x; y@ brings x's attributes into scope for y.
-- Multiple 'with' expressions stack, with inner scopes shadowing outer.
--
-- The environment also carries the NIX_PATH for resolving environment
-- paths like @\<nixpkgs\>@, and the current file path for resolving
-- relative imports.
data NixEnv = NixEnv
  { envWithScopes :: ![NixAttrs]
    -- ^ Stack of 'with' scopes, innermost first
  , envNixPath :: !NixPath
    -- ^ NIX_PATH for resolving environment paths
  , envCurrentFile :: !(Maybe Path)
    -- ^ Current file being evaluated, for resolving relative imports.
    -- Nothing if evaluating from REPL or --expr.
  }
  deriving stock (Show, Generic)

instance NFData NixEnv where
  rnf (NixEnv scopes nixPath currentFile) = rnf scopes `seq` rnf nixPath `seq` rnf currentFile

-- | Empty environment (no 'with' scopes, empty NIX_PATH, no current file)
emptyEnv :: NixEnv
emptyEnv = NixEnv [] emptyNixPath Nothing
{-# NOINLINE emptyEnv #-}

-- | Create an environment with a current file path set.
-- Used for evaluating files where relative imports should resolve correctly.
mkEnvWithCurrentFile :: Path -> NixEnv
mkEnvWithCurrentFile path = NixEnv [] emptyNixPath (Just path)
{-# NOINLINE mkEnvWithCurrentFile #-}

-- | Push a new 'with' scope
pushWithScope :: NixAttrs -> NixEnv -> NixEnv
pushWithScope attrs env = env { envWithScopes = attrs : envWithScopes env }
{-# INLINE pushWithScope #-}

-- | Look up a variable in the 'with' scope stack.
-- Returns Nothing if not found in any scope.
lookupWithScopes :: VarName -> NixEnv -> Maybe NixValue
lookupWithScopes name env = go (envWithScopes env)
  where
    go [] = Nothing
    go (scope : rest) =
      case lookupAttr name scope of
        Just v -> Just v
        Nothing -> go rest
{-# INLINE lookupWithScopes #-}

-- | Like 'lookupWithScopes' but throws 'UndefinedVariable' on missing.
-- Used by generated code where we need NixValue, not Maybe NixValue.
lookupWithScopesOrThrow :: VarName -> NixEnv -> NixValue
lookupWithScopesOrThrow name env =
  case lookupWithScopes name env of
    Just v -> v
    Nothing -> throwNixError (UndefinedVariable name)
{-# INLINE lookupWithScopesOrThrow #-}

-- * NixError
-- Re-exported from Nix.Compile.Value.Error

-- * Value predicates

isNull :: NixValue -> Bool
isNull VNull = True
isNull _ = False
{-# INLINE isNull #-}

isInt :: NixValue -> Bool
isInt (VInt _) = True
isInt _ = False
{-# INLINE isInt #-}

isFloat :: NixValue -> Bool
isFloat (VFloat _) = True
isFloat _ = False
{-# INLINE isFloat #-}

isBool :: NixValue -> Bool
isBool (VBool _) = True
isBool _ = False
{-# INLINE isBool #-}

isString :: NixValue -> Bool
isString (VString _ _) = True
isString _ = False
{-# INLINE isString #-}

isList :: NixValue -> Bool
isList (VList _) = True
isList _ = False
{-# INLINE isList #-}

isAttrs :: NixValue -> Bool
isAttrs (VAttrs _) = True
isAttrs _ = False
{-# INLINE isAttrs #-}

isFunction :: NixValue -> Bool
isFunction (VClosure _ _) = True
isFunction (VBuiltin _ _) = True
isFunction _ = False
{-# INLINE isFunction #-}

isPath :: NixValue -> Bool
isPath (VPath _) = True
isPath _ = False
{-# INLINE isPath #-}

-- | Get the type name of a value for error messages.
valueTypeName :: NixValue -> Text
valueTypeName = \case
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VBool _ -> "a boolean"
  VNull -> "null"
  VString _ ctx
    | HS.null ctx -> "a string"
    | otherwise -> "a string with context"
  VPath _ -> "a path"
  VList _ -> "a list"
  VAttrs _ -> "a set"
  VClosure _ _ -> "a function"
  VBuiltin _ _ -> "a built-in function"

-- * VarName helpers for generated code

-- | Create a VarName from a String.
-- This is used by generated Core code which produces String literals.
-- Unlike mkVarName (which takes Text), this takes String directly.
-- Uses the IsString instance for VarName.
mkVarNameStr :: String -> VarName
mkVarNameStr = fromString
{-# INLINE mkVarNameStr #-}

-- | Create a VarName from a NixValue by coercing to string.
-- This is used for dynamic attribute keys like { ${expr} = val; }.
-- Only strings are accepted; other types throw TypeError.
mkVarNameFromValue :: NixValue -> VarName
mkVarNameFromValue (VString t _) = fromString (toString t)
mkVarNameFromValue v = throwNixError $ TypeError "a string" (valueTypeName v)
{-# INLINE mkVarNameFromValue #-}

-- * NonEmpty helpers for generated code

-- | Create a NonEmpty VarName from head and tail.
-- This is used by generated Core code to construct NonEmpty lists
-- for nixHasAttrPath and nixSelectPath calls.
mkNonEmptyVarName :: VarName -> [VarName] -> NonEmpty VarName
mkNonEmptyVarName h t = h :| t
{-# INLINE mkNonEmptyVarName #-}

-- * Text helpers for generated code

-- | Convert a String to Text.
-- This is used by generated Core code to convert string literals (which
-- GHC represents as String after unpackCString#) to Text for VString.
mkText :: String -> Text
mkText = fromString
{-# INLINE mkText #-}

-- * Path helpers for generated code

-- | Convert a String to Path.
-- This is used by generated Core code to convert string literals to Path
-- for VPath construction.
mkPath :: String -> Path
mkPath = fromString
{-# INLINE mkPath #-}
