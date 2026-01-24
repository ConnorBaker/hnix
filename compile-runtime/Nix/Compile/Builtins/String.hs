{-# LANGUAGE NoStrict #-}

-- | String-related built-in functions for the compiled Nix runtime.
--
-- This module implements string manipulation, hashing, version comparison,
-- and string context handling built-ins available in Nix.
module Nix.Compile.Builtins.String
  ( -- * String manipulation
    builtinSubstring
  , builtinStringLength
  , builtinReplaceStrings
  , builtinSplit
  , builtinMatch
  , builtinConcatStringsSep
    -- * Hashing and version operations
  , builtinHashString
  , builtinCompareVersions
  , builtinSplitVersion
    -- * String context operations
  , builtinUnsafeDiscardStringContext
  , builtinAddContextFrom
  , builtinHasContext
  , builtinGetContext
  , builtinAppendContext
  ) where

import Relude
import qualified Data.Text as T
import qualified Data.List as List
import Data.Char (isDigit)
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Regex.TDFA ((=~), Regex, makeRegexOpts, matchOnceText, defaultCompOpt, defaultExecOpt, CompOption(..))
import Text.Regex.TDFA.Text ()  -- instances for Text
import Data.Array (elems)
import Crypto.Hash (Digest, MD5, SHA1, SHA256, SHA512, hash)
import qualified Data.ByteArray.Encoding as BA
import Nix.Types.VarName (VarName, mkVarName, varNameText)
import Nix.Compile.Value
import Nix.Compile.Primops

-- * String builtins

-- | Substring. builtins.substring start len str
builtinSubstring :: NixValue
builtinSubstring = VBuiltin "substring" $ \startVal ->
  VBuiltin "substring len" $ \lenVal ->
    VBuiltin "substring str" $ \v ->
      let start = fromIntegral $ expectInt startVal
          len = fromIntegral $ expectInt lenVal
          (t, ctx) = expectString v
          -- Nix semantics:
          -- - Negative start: start from beginning (clamp to 0)
          -- - Negative length: take rest of string from start
          actualStart = max 0 start
          result = if len < 0
                   then T.drop actualStart t  -- Negative length = rest of string
                   else T.take len $ T.drop actualStart t
      in VString result ctx
{-# NOINLINE builtinSubstring #-}

-- | String length.
builtinStringLength :: NixValue -> NixValue
builtinStringLength v =
  let (t, _) = expectString v
  in VInt $ fromIntegral $ T.length t
{-# INLINE builtinStringLength #-}

-- | String replacement. builtins.replaceStrings from to str
-- Special case: empty "from" pattern inserts replacement between every character.
-- E.g., replaceStrings [""] ["."] "ab" = ".a.b."
builtinReplaceStrings :: NixValue
builtinReplaceStrings = VBuiltin "replaceStrings" $ \fromVal ->
  VBuiltin "replaceStrings to" $ \toVal ->
    VBuiltin "replaceStrings str" $ \v ->
      let fromList = V.toList $ V.map (fst . expectString) $ expectList fromVal
          toList = V.toList $ V.map (fst . expectString) $ expectList toVal
          (t, ctx) = expectString v
          -- Apply replacements left-to-right with special handling for empty pattern
          result = foldl' applyReplacement t (zip fromList toList)
      in VString result ctx  -- Context should be merged, simplified here
  where
    -- | Apply a single from->to replacement, handling empty from specially
    applyReplacement :: Text -> (Text, Text) -> Text
    applyReplacement s (from, to)
      | T.null from = insertBetween to s  -- Empty pattern: insert between every char
      | otherwise = T.replace from to s

    -- | Insert text between every character (and at start/end)
    -- insertBetween "." "ab" = ".a.b."
    insertBetween :: Text -> Text -> Text
    insertBetween sep s = T.intercalate sep ("" : T.chunksOf 1 s <> [""])
{-# NOINLINE builtinReplaceStrings #-}

-- | builtins.match: Match entire string against regex.
-- Returns null if no match, or a list of capture groups.
-- The regex must match the ENTIRE string (implicit ^...$).
-- For optional capture groups that don't participate, returns null (not empty string).
builtinMatch :: NixValue
builtinMatch = VBuiltin "match" $ \regexArg ->
  VBuiltin "match str" $ \strArg ->
    let (pattern, _) = expectString regexArg
        (str, _) = expectString strArg
    in case T.null pattern of
         -- Special case: empty regex "" matches only empty string ""
         -- Returns empty list (no capture groups)
         True -> if T.null str
                   then VList V.empty
                   else VNull
         False ->
           let -- Use POSIX ERE semantics: . matches newlines, ^/$ match string boundaries only
               nixCompOpt = defaultCompOpt { multiline = False }
               re = makeRegexOpts nixCompOpt defaultExecOpt pattern :: Regex
               -- mkMatch: convert a capture group to NixValue
               -- offset -1 means the group didn't participate in the match (null)
               -- offset >= 0 means the group participated, even if empty (returns the text)
               mkMatch :: (Text, (Int, Int)) -> NixValue
               mkMatch (t, (offset, _len)) =
                 if offset < 0
                   then VNull
                   else VString t emptyContext
           in case matchOnceText re str of
                -- matchOnceText returns Maybe (before, MatchText, after)
                -- We need the entire string to match (before and after must be empty)
                Just ("", sarr, "") ->
                  let submatches = elems sarr
                  in case submatches of
                       [] -> VList V.empty
                       [_] -> VList V.empty  -- single element means no capture groups
                       (_:xs) -> VList $ V.fromList $ map mkMatch xs  -- drop full match, keep groups
                _ -> VNull  -- No match or partial match
{-# NOINLINE builtinMatch #-}

-- | builtins.split: Split string by regex.
-- Returns a list alternating between non-matched parts and lists of capture groups.
-- Example: split "a" "xaxax" returns ["x" [] "x" [] "x"]
-- Example: split "(a)" "xaxax" returns ["x" ["a"] "x" ["a"] "x"]
-- Special case: empty regex splits between every character.
builtinSplit :: NixValue
builtinSplit = VBuiltin "split" $ \regexArg ->
  VBuiltin "split str" $ \strArg ->
    let (regex, _) = expectString regexArg
        (str, strCtx) = expectString strArg
    in VList $ V.fromList $ nixSplit regex str strCtx
  where
    nixSplit :: Text -> Text -> NixContext -> [NixValue]
    nixSplit regex str ctx
      -- Special case: empty regex pattern splits between every character
      | T.null regex = splitEmpty str ctx
      | otherwise = go str
      where
        go s
          | T.null s = [VString "" ctx]
          | otherwise =
              let result :: (Text, Text, Text, [Text])
                  result = s =~ regex
              in case result of
                   (before, match, after, groups)
                     | T.null match -> [VString s ctx]  -- No more matches
                     | otherwise ->
                         let beforeVal = VString before ctx
                             groupsVal = VList $ V.fromList [VString g emptyContext | g <- groups]
                         in beforeVal : groupsVal : go after

    -- | Handle empty regex: split between every character
    -- split "" "ab" = ["" [] "a" [] "b" [] ""]
    splitEmpty :: Text -> NixContext -> [NixValue]
    splitEmpty s ctx = go' ("" : T.chunksOf 1 s <> [""])
      where
        emptyGroups = VList V.empty
        go' [] = []
        go' [x] = [VString x ctx]
        go' (x:xs) = VString x ctx : emptyGroups : go' xs
{-# NOINLINE builtinSplit #-}

-- | Concatenate strings with separator.
builtinConcatStringsSep :: NixValue
builtinConcatStringsSep = VBuiltin "concatStringsSep" $ \sepVal ->
  VBuiltin "concatStringsSep list" $ \v ->
    let (sep, sepCtx) = expectString sepVal
        lst = V.toList $ expectList v
        parts = map expectString lst
        result = T.intercalate sep (map fst parts)
        ctx = foldl' unionContext sepCtx (map snd parts)
    in VString result ctx
{-# NOINLINE builtinConcatStringsSep #-}

-- | Hash a string with the specified algorithm.
-- Supports "md5", "sha1", "sha256", "sha512".
builtinHashString :: NixValue
builtinHashString = VBuiltin "hashString" $ \algoArg ->
  VBuiltin "hashString str" $ \strArg ->
    let (algo, _) = expectString algoArg
        (str, _) = expectString strArg
        bytes = encodeUtf8 str
    in VString (computeHash algo bytes) emptyContext
  where
    -- | Compute hash of bytes with the specified algorithm
    computeHash :: Text -> ByteString -> Text
    computeHash algo bytes = case algo of
      "md5"    -> toHex (hash bytes :: Digest MD5)
      "sha1"   -> toHex (hash bytes :: Digest SHA1)
      "sha256" -> toHex (hash bytes :: Digest SHA256)
      "sha512" -> toHex (hash bytes :: Digest SHA512)
      _        -> throwNixError $ ThrownError $ "builtins.hashString: unknown hash algorithm '" <> algo <> "'"

    -- | Convert digest to hex string
    toHex :: Digest a -> Text
    toHex d = decodeUtf8 (BA.convertToBase BA.Base16 d :: ByteString)
{-# NOINLINE builtinHashString #-}

-- | Compare version strings.
-- Splits versions into components (numeric and non-numeric alternating).
-- Numeric components are compared as integers.
-- Non-numeric components are compared lexicographically.
-- "pre" sorts before all other strings; numbers sort after strings.
builtinCompareVersions :: NixValue
builtinCompareVersions = VBuiltin "compareVersions" $ \v1 ->
  VBuiltin "compareVersions v2" $ \v2 ->
    let (t1, _) = expectString v1
        (t2, _) = expectString v2
        result = compareVersions t1 t2
    in VInt $ case result of
         LT -> -1
         EQ -> 0
         GT -> 1
{-# NOINLINE builtinCompareVersions #-}

-- | Split version string into components.
-- Splits on numeric/non-numeric boundaries, treating '.' and '-' as separators.
-- Example: "1.0pre1" -> ["1", "0", "pre", "1"]
builtinSplitVersion :: NixValue -> NixValue
builtinSplitVersion v =
  let (t, _) = expectString v
      components = splitVersion t
  in VList $ V.fromList $ map (componentToValue) components
{-# INLINE builtinSplitVersion #-}

-- | Convert a VersionComponent to a NixValue (string representation).
componentToValue :: VersionComponent -> NixValue
componentToValue = \case
  VersionComponentPre      -> VString "pre" emptyContext
  VersionComponentString s -> VString s emptyContext
  VersionComponentNumber n -> VString (show n) emptyContext

-- * Version comparison internals

-- | A version component, following Nix semantics.
-- The derived Ord instance gives: Pre < String < Number
-- This matches Nix behavior where:
--   - "pre" sorts before all other strings
--   - Numbers sort after strings
data VersionComponent
  = VersionComponentPre      -- ^ The string "pre"
  | VersionComponentString !Text  -- ^ A string other than "pre"
  | VersionComponentNumber !Integer  -- ^ A number
  deriving (Read, Eq, Ord)

-- | Split a version string into components.
-- Based on Nix's splitVersion implementation:
-- - Consecutive digits form a VersionComponentNumber
-- - "pre" becomes VersionComponentPre
-- - Other character sequences become VersionComponentString
-- - Separators ('.' and '-') are discarded
splitVersion :: Text -> [VersionComponent]
splitVersion s = case T.uncons s of
  Nothing -> []
  Just (c, _)
    -- Try to parse as a number first
    | Right (n, rest) <- decimal @Integer s ->
        VersionComponentNumber n : splitVersion rest
    -- Skip separators
    | c `elem` separators -> splitVersion (T.drop 1 s)
    -- Otherwise collect non-digit, non-separator chars
    | otherwise ->
        let (charsSpan, rest) = T.span (\x -> not (isDigit x) && x `notElem` separators) s
            component = case charsSpan of
              "pre" -> VersionComponentPre
              xs    -> VersionComponentString xs
        in component : splitVersion rest
  where
    separators :: String
    separators = ".-"

-- | Compare two version strings using Nix semantics.
-- Compares components pairwise, treating missing components as empty strings.
compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 = go (splitVersion s1) (splitVersion s2)
  where
    -- Filler value for missing components
    filler :: VersionComponent
    filler = VersionComponentString mempty

    go :: [VersionComponent] -> [VersionComponent] -> Ordering
    go [] [] = EQ
    go [] (y:ys) = compare filler y <> go [] ys
    go (x:xs) [] = compare x filler <> go xs []
    go (x:xs) (y:ys) = compare x y <> go xs ys

-- * String context operations

-- | Discard string context (unsafe).
builtinUnsafeDiscardStringContext :: NixValue -> NixValue
builtinUnsafeDiscardStringContext v =
  let (t, _) = expectString v
  in VString t emptyContext
{-# INLINE builtinUnsafeDiscardStringContext #-}

-- | Add context from one string to another.
builtinAddContextFrom :: NixValue
builtinAddContextFrom = VBuiltin "addContextFrom" $ \ctxSource ->
  VBuiltin "addContextFrom str" $ \v ->
    let (_, ctx) = expectString ctxSource
        (t, ctx2) = expectString v
    in VString t (unionContext ctx ctx2)
{-# NOINLINE builtinAddContextFrom #-}

-- | Check if a string has context.
builtinHasContext :: NixValue -> NixValue
builtinHasContext v = VBool (hasContextValue v)
{-# INLINE builtinHasContext #-}

-- | Get the context of a string as an attribute set.
-- Returns an attrset where:
-- * Keys are store paths
-- * Values are attrsets with optional "path", "allOutputs", and "outputs" keys
--
-- This matches Nix's builtins.getContext semantics:
-- * { path = true; } for DirectPath
-- * { allOutputs = true; } for AllOutputs
-- * { outputs = ["out" "dev"]; } for specific outputs
builtinGetContext :: NixValue -> NixValue
builtinGetContext v =
  let (_, ctx) = expectString v
      ctxList = contextToList ctx
      -- Group contexts by path
      grouped = groupContextsByPath ctxList
      -- Build the result attrset
      pairs = [(mkVarName path, buildContextAttrs contexts)
              | (path, contexts) <- grouped]
  in VAttrs $ attrsFromList pairs
  where
    -- Group contexts by their path
    groupContextsByPath :: [StringContext] -> [(Text, [StringContext])]
    groupContextsByPath cs =
      let sorted = List.sortOn scPath cs
          grouped = List.groupBy (\a b -> scPath a == scPath b) sorted
      in [(scPath (List.head g), g) | g <- grouped]

    -- Build the context info attrset for a single path
    buildContextAttrs :: [StringContext] -> NixValue
    buildContextAttrs contexts =
      let hasPath = any (\c -> scFlavor c == DirectPath) contexts
          hasAllOutputs = any (\c -> scFlavor c == AllOutputs) contexts
          outputs = [out | StringContext (DerivationOutput out) _ <- contexts]
          pairs = catMaybes
            [ if hasPath then Just ("path", VBool True) else Nothing
            , if hasAllOutputs then Just ("allOutputs", VBool True) else Nothing
            , if not (null outputs)
              then Just ("outputs", VList $ V.fromList $ map (\o -> VString o emptyContext) outputs)
              else Nothing
            ]
      in VAttrs $ attrsFromList pairs
{-# INLINE builtinGetContext #-}

-- | Append context to a string. builtins.appendContext str contextAttrSet
-- The context attrset has the same format as builtins.getContext output.
builtinAppendContext :: NixValue
builtinAppendContext = VBuiltin "appendContext" $ \strVal ->
  VBuiltin "appendContext context" $ \ctxVal ->
    let (t, existingCtx) = expectString strVal
        ctxAttrs = expectAttrs ctxVal
        newCtx = parseContextAttrs ctxAttrs
    in VString t (unionContext existingCtx newCtx)
  where
    parseContextAttrs :: NixAttrs -> NixContext
    parseContextAttrs attrs =
      let pairs = attrToList attrs
          contexts = concatMap parsePathEntry pairs
      in contextFromList contexts

    parsePathEntry :: (VarName, NixValue) -> [StringContext]
    parsePathEntry (pathName, infoVal) =
      let pathText = varNameText pathName
          info = expectAttrs infoVal
          -- Check for "path" = true
          pathCtx = case lookupAttr (mkVarName "path") info of
            Just (VBool True) -> [StringContext DirectPath pathText]
            _ -> []
          -- Check for "allOutputs" = true
          allOutCtx = case lookupAttr (mkVarName "allOutputs") info of
            Just (VBool True) -> [StringContext AllOutputs pathText]
            _ -> []
          -- Check for "outputs" = [...]
          outputsCtx = case lookupAttr (mkVarName "outputs") info of
            Just (VList outs) ->
              [StringContext (DerivationOutput (fst $ expectString o)) pathText
              | o <- V.toList outs]
            _ -> []
      in pathCtx ++ allOutCtx ++ outputsCtx
{-# NOINLINE builtinAppendContext #-}
