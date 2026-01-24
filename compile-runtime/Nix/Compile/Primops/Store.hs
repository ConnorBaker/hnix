{-# LANGUAGE NoStrict #-}

-- | Store operation primitive operations for the compiled Nix runtime.
--
-- These primops interact with the Nix store for derivations, adding paths,
-- and fetching. Currently stubbed - full implementation requires overlay store.
--
-- Design principles:
-- * Match Nix semantics for store operations
-- * Return fake store paths for now (stub implementation)
-- * Throw NixError on type mismatches
-- * NOINLINE to prevent optimization issues with unsafePerformIO
module Nix.Compile.Primops.Store
  ( -- * Store path types
    StorePath(..)
    -- * Derivation operations (stubs)
  , nixDerivation
    -- * Path operations (stubs)
  , nixAddPath
  , nixToFile
  , nixFilterSource
    -- * Fetch operations (stubs)
  , nixFetchUrl
  , nixFetchTarball
  , nixFetchGit
  ) where

import Relude
import qualified Data.Text as T
import Nix.Types.VarName (VarName, mkVarName)
import Nix.Types.Path (Path)
import Nix.Compile.Value
import Nix.Compile.Value.Context (pathContext)

-- | A store path (e.g., /nix/store/abc123-foo)
newtype StorePath = StorePath { unStorePath :: Path }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Stub: Create a derivation.
-- In real Nix, this:
-- 1. Validates the derivation attrs
-- 2. Computes the output hash
-- 3. Writes the .drv file to the store
-- 4. Returns an attrset with outPath, drvPath, etc.
nixDerivation :: NixValue -> NixValue
nixDerivation args =
  let attrs = expectAttrsStore args
      -- Get required "name" attribute
      name = case lookupAttr (mkVarName "name") attrs of
        Just (VString n _) -> n
        Just v -> throwNixError $ TypeError "a string" (valueTypeName v)
        Nothing -> throwNixError $ AttrMissing (mkVarName "name")

      -- Fake output path for now
      fakeHash = "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
      outPath = "/nix/store/" <> fakeHash <> "-" <> name
      drvPath = outPath <> ".drv"

      -- Create output attrset with context
      outPathCtx = pathContext outPath
  in VAttrs $ attrsFromList
       [ (mkVarName "outPath", VString outPath outPathCtx)
       , (mkVarName "drvPath", VString drvPath outPathCtx)
       , (mkVarName "name", VString name emptyContext)
       , (mkVarName "type", VString "derivation" emptyContext)
       ]
{-# NOINLINE nixDerivation #-}

-- | Stub: Add a path to the store.
-- In real Nix, this copies the path to /nix/store with content-addressing.
nixAddPath :: NixValue -> NixValue
nixAddPath pathVal =
  let path = expectPathStore pathVal
      -- Just return the path as a string with context for now
      pathText = toText path
  in VString pathText (pathContext pathText)
{-# NOINLINE nixAddPath #-}

-- | Stub: Create a file in the store with given contents.
-- builtins.toFile name contents
nixToFile :: NixValue -> NixValue -> NixValue
nixToFile nameVal contentsVal =
  let (name, _) = expectStringStore nameVal
      (_contents, _) = expectStringStore contentsVal
      -- Fake store path
      fakeHash = "ffffffffffffffffffffffffffffffff"
      storePath = "/nix/store/" <> fakeHash <> "-" <> name
  in VString storePath (pathContext storePath)
{-# NOINLINE nixToFile #-}

-- | Stub: Filter a source path.
-- builtins.filterSource filter path
nixFilterSource :: NixValue -> NixValue -> NixValue
nixFilterSource _filterFn pathVal =
  -- For now, just return the path unchanged
  let path = expectPathStore pathVal
      pathText = toText path
  in VString pathText (pathContext pathText)
{-# NOINLINE nixFilterSource #-}

-- | Stub: Fetch a URL.
-- builtins.fetchurl { url, sha256?, name? }
nixFetchUrl :: NixValue -> NixValue
nixFetchUrl args = case args of
  VString _url _ ->
    -- Simple form: fetchurl "http://..."
    let fakeHash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        storePath = "/nix/store/" <> fakeHash <> "-download"
    in VString storePath (pathContext storePath)
  VAttrs attrs ->
    -- Attrset form: fetchurl { url = "..."; sha256 = "..."; }
    let name = case lookupAttr (mkVarName "name") attrs of
          Just (VString n _) -> n
          Nothing -> "download"  -- Default name
          Just v -> throwNixError $ TypeError "a string" (valueTypeName v)
        fakeHash = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        storePath = "/nix/store/" <> fakeHash <> "-" <> name
    in VString storePath (pathContext storePath)
  v -> throwNixError $ TypeError "a string or attrset" (valueTypeName v)
{-# NOINLINE nixFetchUrl #-}

-- | Stub: Fetch a tarball.
-- builtins.fetchTarball { url, sha256?, name? }
nixFetchTarball :: NixValue -> NixValue
nixFetchTarball args = case args of
  VString _url _ ->
    let fakeHash = "cccccccccccccccccccccccccccccccc"
        storePath = "/nix/store/" <> fakeHash <> "-source"
    in VPath (fromString (toString storePath))
  VAttrs attrs ->
    let name = case lookupAttr (mkVarName "name") attrs of
          Just (VString n _) -> n
          Nothing -> "source"
          Just v -> throwNixError $ TypeError "a string" (valueTypeName v)
        fakeHash = "dddddddddddddddddddddddddddddddd"
        storePath = "/nix/store/" <> fakeHash <> "-" <> name
    in VPath (fromString (toString storePath))
  v -> throwNixError $ TypeError "a string or attrset" (valueTypeName v)
{-# NOINLINE nixFetchTarball #-}

-- | Stub: Fetch a git repository.
-- builtins.fetchGit { url, ref?, rev?, submodules?, shallow?, allRefs? }
nixFetchGit :: NixValue -> NixValue
nixFetchGit args =
  let attrs = expectAttrsStore args
      fakeHash = "gggggggggggggggggggggggggggggggg"
      fakeRev = "0000000000000000000000000000000000000000"
      storePath = "/nix/store/" <> fakeHash <> "-source"
  in VAttrs $ attrsFromList
       [ (mkVarName "outPath", VPath (fromString (toString storePath)))
       , (mkVarName "rev", VString fakeRev emptyContext)
       , (mkVarName "shortRev", VString (T.take 7 fakeRev) emptyContext)
       , (mkVarName "revCount", VInt 0)
       , (mkVarName "submodules", VBool False)
       ]
{-# NOINLINE nixFetchGit #-}

-- * Local helper functions
-- These are defined locally to avoid importing from Coerce module,
-- keeping this module more self-contained.

-- | Extract Path from a NixValue, throwing TypeError on mismatch.
expectPathStore :: NixValue -> Path
expectPathStore (VPath p) = p
expectPathStore v = throwNixError $ TypeError "a path" (valueTypeName v)
{-# INLINE expectPathStore #-}

-- | Extract NixAttrs from a NixValue, throwing TypeError on mismatch.
expectAttrsStore :: NixValue -> NixAttrs
expectAttrsStore (VAttrs as) = as
expectAttrsStore v = throwNixError $ TypeError "a set" (valueTypeName v)
{-# INLINE expectAttrsStore #-}

-- | Extract Text and context from a NixValue, throwing TypeError on mismatch.
expectStringStore :: NixValue -> (Text, NixContext)
expectStringStore (VString t ctx) = (t, ctx)
expectStringStore v = throwNixError $ TypeError "a string" (valueTypeName v)
{-# INLINE expectStringStore #-}
