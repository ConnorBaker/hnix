{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | String manipulation builtins.
--
-- This module contains builtins that operate on strings:
-- hashString, match, split, substring, replaceStrings,
-- compareVersions, splitVersion, parseDrvName, and toString.
module Nix.Builtins.String
  ( -- * String manipulation
    hashStringNix
  , matchNix
  , splitNix
  , substringNix
  , replaceStringsNix
    -- * Version handling
  , compareVersionsNix
  , splitVersionNix
  , parseDrvNameNix
    -- * Coercion
  , toStringNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Crypto.Hash                   as Hash
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Builder
import           Data.Array                     ( elems )
import           Text.Regex.TDFA                ( Regex
                                                , makeRegexOpts
                                                , matchOnceText
                                                , matchAllText
                                                , defaultCompOpt
                                                , defaultExecOpt
                                                , CompOption(..)
                                                )
import           Nix.Atoms                      ( NAtom(..) )
import qualified Nix.Core.AttrSet              as A
import           Nix.Expr.Types                 ( AttrSet, mkVarName )
import qualified Nix.Core.List                 as L
import           Nix.Builtins.Internal          ( Prim(..)
                                                , splitVersion
                                                , compareVersions
                                                , splitDrvName
                                                , splitMatches
                                                )
import           Nix.Convert
import           Nix.Exec
import           Nix.Frames
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value


-- * Coercion

-- | Coerce a value to a string.
toStringNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toStringNix = toValue <=< coerceAnyToNixString callFunc DontCopyToStore


-- * Version handling

-- | Split a version string into components.
splitVersionNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
splitVersionNix v =
  do
    version <- fromStringNoContext =<< fromValue v
    pure $
      NVList $
        L.nlFromList $
          mkNVStrWithoutContext . show <$>
            splitVersion version

-- | Compare two version strings.
-- Returns -1 if first is less, 0 if equal, 1 if greater.
compareVersionsNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
compareVersionsNix t1 t2 =
  do
    s1 <- mkText t1
    s2 <- mkText t2

    let
      cmpVers =
        case compareVersions s1 s2 of
          LT -> -1
          EQ -> 0
          GT -> 1

    pure $ NVConstant $ NInt cmpVers

 where
  mkText = fromStringNoContext <=< fromValue

-- | Parse a derivation name into name and version components.
parseDrvNameNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
parseDrvNameNix drvname =
  do
    s <- fromStringNoContext =<< fromValue drvname

    let
      (name :: Text, version :: Text) = splitDrvName s

    toValue @(AttrSet (NValue t f m)) $
      A.fromList
        [ ( mkVarName "name"
          , mkNVStr name
          )
        , ( mkVarName "version"
          , mkNVStr version
          )
        ]

 where
  mkNVStr = mkNVStrWithoutContext


-- * Pattern matching

-- | Match a string against a regular expression.
-- Returns null if no match, or a list of captured groups.
matchNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
matchNix pat str =
  do
    p <- fromStringNoContext =<< fromValue pat
    ns <- fromValue str

    -- NOTE: 2018-11-19: Currently prim_match in nix/src/libexpr/primops.cc
    -- ignores the context of its second argument. This is probably a bug but we're
    -- going to preserve the behavior here until it is fixed upstream.
    -- Relevant issue: https://github.com/NixOS/nix/issues/2547
    let
      s  = ignoreContext ns
      -- Use POSIX ERE semantics: . matches newlines, ^/$ match string boundaries only
      nixCompOpt = defaultCompOpt { multiline = False }
      re = makeRegexOpts nixCompOpt defaultExecOpt p :: Regex
      -- mkMatch: convert a capture group to NValue
      -- offset -1 means the group didn't participate in the match (null)
      -- offset >= 0 means the group participated, even if empty (returns the text)
      mkMatch (t, (offset, _len)) =
        if offset < 0
          then pure NVNull
          else toValue $ mkNixStringWithoutContext t

    case matchOnceText re s of
      Just ("", sarr, "") ->
        do
          let submatches = elems sarr
          (NVList . L.nlFromList) <$>
            traverse
              mkMatch
              (case submatches of
                 [] -> mempty
                 [_] -> mempty  -- single element means no capture groups, return empty list
                 _:xs -> xs -- return only the matched groups, drop the full string
              )
      _ -> pure NVNull

-- | Split a string by a regular expression.
-- Returns a list alternating between unmatched strings and lists of captured groups.
splitNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
splitNix pat str =
  do
    p <- fromStringNoContext =<< fromValue pat
    ns <- fromValue str
        -- NOTE: Currently prim_split in nix/src/libexpr/primops.cc ignores the
        -- context of its second argument. This is probably a bug but we're
        -- going to preserve the behavior here until it is fixed upstream.
        -- Relevant issue: https://github.com/NixOS/nix/issues/2547
    let
      s = ignoreContext ns
      -- Use POSIX ERE semantics: . matches newlines, ^/$ match string boundaries only
      nixCompOpt = defaultCompOpt { multiline = False }
      regex = makeRegexOpts nixCompOpt defaultExecOpt p :: Regex
      haystack = encodeUtf8 s

    pure $ NVList $ L.nlFromList $ splitMatches 0 (elems <$> matchAllText regex haystack) haystack


-- * Substring operations

-- | Extract a substring from a string.
substringNix :: forall e t f m. MonadNix e t f m => Int -> Int -> NixString -> Prim m NixString
substringNix start len str =
  Prim $
    if start >= 0
      then pure $ modifyNixContents (take . Text.drop start) str
      else throwError $ ErrorCall $ "builtins.substring: negative start position: " <> show start
 where
  take =
    if len >= 0
      then Text.take len
      else id  --NOTE: negative values of 'len' are OK, and mean "take everything"


-- * String replacement

-- | Replace substrings in a string.
-- The first two arguments are lists of patterns and replacements.
--
-- Example:
-- builtins.replaceStrings ["ll" "e"] [" " "i"] "Hello world" == "Hi o world".
replaceStringsNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
replaceStringsNix tfrom tto ts =
  do
    -- NixStrings have context - remember
    (fromKeys :: [NixString]) <- fromValue (Deeper tfrom)
    (toVals   :: [NixString]) <- fromValue (Deeper tto)
    (string   ::  NixString ) <- fromValue ts

    when (length fromKeys /= length toVals) $ throwError $ ErrorCall "builtins.replaceStrings: Arguments `from`&`to` construct a key-value map, so the number of their elements must always match."

    let
      --  2021-02-18: NOTE: if there is no match - the process does not changes the context, simply slides along the string.
      --  So it isbe more effective to pass the context as the first argument.
      --  And moreover, the `passOneCharNgo` passively passes the context, to context can be removed from it and inherited directly.
      --  Then the solution would've been elegant, but the Nix bug prevents elegant implementation.
      go ctx input output =
        case maybePrefixMatch of
          -- Passively pass the chars
          Nothing -> passOneChar
          Just match -> replace match
        where
          -- When prefix matched something - returns (match, replacement, remainder)
          maybePrefixMatch :: Maybe (Text, NixString, Text)
          maybePrefixMatch =
            formMatchReplaceTailInfo <$> find ((`Text.isPrefixOf` input) . fst) fromKeysToValsMap
            where
              formMatchReplaceTailInfo (m, r) =
                (m, r, Text.drop (Text.length m) input)

              fromKeysToValsMap = zip (ignoreContext <$> fromKeys) toVals

          -- Not passing args => It is constant that gets embedded into `go` => It is simple `go` tail recursion
          passOneChar =
            case Text.uncons input of
              Nothing -> finish ctx output  -- The base case - there is no chars left to process -> finish
              Just (c, i) -> go ctx i (output <> Builder.singleton c) -- If there are chars - pass one char & continue

          --  2021-02-18: NOTE: rly?: toStrict . toLazyText
          --  Maybe `text-builder`, `text-show`?
          finish ctx output = mkNixString ctx (toStrict $ Builder.toLazyText output)

          replace (key, replacementNS, unprocessedInput) =
            replaceWithNixBug unprocessedInput updatedOutput
            where
              replaceWithNixBug =
                if isNixBugCase
                  -- Allowing match on "" is a inherited bug of Nix,
                  -- when "" is checked - it always matches. And so - when it checks - it always insers a replacement, and then process simply passesthrough the char that was under match.
                  --
                  -- repl> builtins.replaceStrings ["" "e"] [" " "i"] "Hello world"
                  -- " H e l l o   w o r l d "
                  -- repl> builtins.replaceStrings ["ll" ""] [" " "i"] "Hello world"
                  -- "iHie ioi iwioirilidi"
                  --  2021-02-18: NOTE: There is no tests for this
                  then bugPassOneChar  -- augmented recursion
                  else go updatedCtx  -- tail recursion

              isNixBugCase = key == mempty

              updatedOutput  = output <> replacement
              updatedCtx     = ctx <> replacementCtx

              replacement    = Builder.fromText $ ignoreContext replacementNS
              replacementCtx = getStringContext replacementNS

              -- The bug modifies the content => bug demands `pass` to be a real function =>
              -- `go` calls `pass` function && `pass` calls `go` function
              -- => mutual recusion case, so placed separately.
              bugPassOneChar input output =
                case Text.uncons input of
                  Nothing -> finish updatedCtx output  -- The base case - there is no chars left to process -> finish
                  Just (c, i) -> go updatedCtx i $ output <> Builder.singleton c -- If there are chars - pass one char & continue

    toValue $ go (getStringContext string) (ignoreContext string) mempty


-- * Hashing

-- fail if context in the algo arg
-- propagate context from the s arg
-- | The result coming out of hashString is base16 encoded
hashStringNix
  :: forall e t f m. MonadNix e t f m => NixString -> NixString -> Prim m NixString
hashStringNix nsAlgo ns =
  Prim $
    do
      algo <- fromStringNoContext nsAlgo
      let
        f g = pure $ modifyNixContents g ns

      case algo of
        --  2021-03-04: Pattern can not be taken-out because hashes represented as different types
        "md5"    -> f (show . mkHash @Hash.MD5)
        "sha1"   -> f (show . mkHash @Hash.SHA1)
        "sha256" -> f (show . mkHash @Hash.SHA256)
        "sha512" -> f (show . mkHash @Hash.SHA512)

        _ -> throwError $ ErrorCall $ "builtins.hashString: expected \"md5\", \"sha1\", \"sha256\", or \"sha512\", got " <> show algo

       where
        -- This intermidiary `a` is only needed because of the type application
        mkHash :: (Show a, Hash.HashAlgorithm a) => Text -> Hash.Digest a
        mkHash s = Hash.hash (encodeUtf8 s :: ByteString)
