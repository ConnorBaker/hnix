{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | String manipulation builtins.
--
-- This module contains builtins that operate on strings:
-- hashString, hashFile, convertHash, match, split, substring, replaceStrings,
-- compareVersions, splitVersion, parseDrvName, placeholder, and toString.
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
    -- * Hash conversion
  , hashFileNix
  , convertHashNix
  , placeHolderNix
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Crypto.Hash                   as Hash
import           Data.ByteArray.Encoding        ( Base(Base16, Base64)
                                                , convertFromBase
                                                , convertToBase
                                                )
import qualified Data.ByteString               as B
import           Data.ByteString.Base16        as Base16
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
                                                , attrsetGet
                                                )
import           Nix.Convert
import           Nix.Exec
import           Nix.Frames
import           Nix.Render                     ( readFile )
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Interned             ( internedNull )
import           Nix.Value.Monad
import           System.Nix.Base32             as Base32


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
        L.fromList $
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
          then pure internedNull
          else toValue $ mkNixStringWithoutContext t

    case matchOnceText re s of
      Just ("", sarr, "") ->
        do
          let submatches = elems sarr
          (NVList . L.fromList) <$>
            traverse
              mkMatch
              (case submatches of
                 [] -> mempty
                 [_] -> mempty  -- single element means no capture groups, return empty list
                 _:xs -> xs -- return only the matched groups, drop the full string
              )
      _ -> pure internedNull

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

    pure $ NVList $ L.fromList $ splitMatches 0 (elems <$> matchAllText regex haystack) haystack


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

-- | hashFileNix
-- use hashStringNix to hash file content
hashFileNix
  :: forall e t f m . MonadNix e t f m => NixString -> Path -> Prim m NixString
hashFileNix nsAlgo nvfilepath = Prim $ hash =<< fileContent
 where
  hash = outPrim . hashStringNix nsAlgo
  outPrim (Prim x) = x
  fileContent :: m NixString
  fileContent = mkNixStringWithoutContext <$> Nix.Render.readFile nvfilepath


-- * Hash conversion

data HashAlgoName
  = HashAlgoMD5
  | HashAlgoSHA1
  | HashAlgoSHA256
  | HashAlgoSHA512
  deriving (Eq, Show)

data HashFormatName
  = HashFormatBase16
  | HashFormatNix32
  | HashFormatBase64
  | HashFormatSRI
  deriving (Eq, Show)

hashAlgoFromText :: Text -> Maybe HashAlgoName
hashAlgoFromText =
  \case
    "md5"    -> Just HashAlgoMD5
    "sha1"   -> Just HashAlgoSHA1
    "sha256" -> Just HashAlgoSHA256
    "sha512" -> Just HashAlgoSHA512
    _        -> Nothing

hashAlgoToText :: HashAlgoName -> Text
hashAlgoToText =
  \case
    HashAlgoMD5    -> "md5"
    HashAlgoSHA1   -> "sha1"
    HashAlgoSHA256 -> "sha256"
    HashAlgoSHA512 -> "sha512"

hashAlgoDigestLength :: HashAlgoName -> Int
hashAlgoDigestLength =
  \case
    HashAlgoMD5    -> 16
    HashAlgoSHA1   -> 20
    HashAlgoSHA256 -> 32
    HashAlgoSHA512 -> 64

parseHashFormat :: Text -> Either ErrorCall HashFormatName
parseHashFormat =
  \case
    "base16" -> Right HashFormatBase16
    "nix32"  -> Right HashFormatNix32
    "base32" -> Right HashFormatNix32
    "base64" -> Right HashFormatBase64
    "sri"    -> Right HashFormatSRI
    x        -> Left $ ErrorCall $ "builtins.convertHash: unknown hash format " <> show x

decodeBase16 :: Text -> Either String B.ByteString
decodeBase16 t = convertFromBase Base16 (encodeUtf8 t :: B.ByteString)

decodeBase64 :: Text -> Either String B.ByteString
decodeBase64 t = convertFromBase Base64 (encodeUtf8 t :: B.ByteString)

decodeNix32 :: Text -> Either String B.ByteString
decodeNix32 = Base32.decode

encodeBase16 :: B.ByteString -> Text
encodeBase16 bs = decodeUtf8 (convertToBase Base16 bs :: B.ByteString)

encodeBase64 :: B.ByteString -> Text
encodeBase64 bs = decodeUtf8 (convertToBase Base64 bs :: B.ByteString)

convertHashNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
convertHashNix nv =
  do
    attrs <- fromValue @(AttrSet (NValue t f m)) =<< demand nv

    hashText <-
      fromStringNoContext
        =<< fromValue
        =<< demand
        =<< attrsetGet "hash" attrs

    mAlgoText <-
      traverse
        (fromStringNoContext <=< fromValue <=< demand)
        (A.lookup (mkVarName "hashAlgo") attrs)

    mAlgo <-
      case mAlgoText of
        Nothing -> pure Nothing
        Just t ->
          case hashAlgoFromText t of
            Just a  -> pure (Just a)
            Nothing -> throwError $ ErrorCall $ "builtins.convertHash: unknown hash algorithm " <> show t

    toHashFormatText <-
      fromStringNoContext
        =<< fromValue
        =<< demand
        =<< attrsetGet "toHashFormat" attrs

    toFormat <-
      case parseHashFormat toHashFormatText of
        Left err -> throwError err
        Right v -> pure v

    (algo, bytes) <- parseInputHash mAlgo hashText

    let
      rendered =
        case toFormat of
          HashFormatBase16 -> encodeBase16 bytes
          HashFormatNix32  -> Base32.encode bytes
          HashFormatBase64 -> encodeBase64 bytes
          HashFormatSRI    -> hashAlgoToText algo <> "-" <> encodeBase64 bytes

    toValue $ mkNixStringWithoutContext rendered

 where
  parseInputHash
    :: Maybe HashAlgoName
    -> Text
    -> m (HashAlgoName, B.ByteString)
  parseInputHash mAlgo input =
    do
      let
        (algoFromHash, body, mFormat) = parseHashPrefix input

      algo <-
        case (mAlgo, algoFromHash) of
          (Just a, Just b) | a /= b ->
            throwError $ ErrorCall $ "builtins.convertHash: hashAlgo " <> show (hashAlgoToText a)
              <> " does not match hash prefix " <> show (hashAlgoToText b)
          (Just a, _) -> pure a
          (Nothing, Just b) -> pure b
          (Nothing, Nothing) ->
            throwError $ ErrorCall "builtins.convertHash: missing hashAlgo"

      bytes <- decodeHash algo mFormat body
      pure (algo, bytes)

  parseHashPrefix :: Text -> (Maybe HashAlgoName, Text, Maybe HashFormatName)
  parseHashPrefix t =
    case Text.breakOn "-" t of
      (algoTxt, rest)
        | Just algo <- hashAlgoFromText algoTxt
        , not (Text.null rest) ->
            (Just algo, Text.drop 1 rest, Just HashFormatBase64)
      _ ->
        case Text.breakOn ":" t of
          (algoTxt, rest)
            | Just algo <- hashAlgoFromText algoTxt
            , not (Text.null rest) ->
                (Just algo, Text.drop 1 rest, Nothing)
          _ -> (Nothing, t, Nothing)

  decodeHash
    :: HashAlgoName
    -> Maybe HashFormatName
    -> Text
    -> m B.ByteString
  decodeHash algo mFormat body =
    do
      let expectedLen = hashAlgoDigestLength algo

          tryDecode fmt =
            case fmt of
              HashFormatBase16 -> decodeBase16 body
              HashFormatNix32  -> decodeNix32 body
              HashFormatBase64 -> decodeBase64 body
              HashFormatSRI    -> decodeBase64 body

          accept bs =
            if B.length bs == expectedLen
              then Just bs
              else Nothing

          formats =
            case mFormat of
              Just fmt -> [fmt]
              Nothing  -> [HashFormatBase16, HashFormatNix32, HashFormatBase64]

          tryFormats [] = Nothing
          tryFormats (fmt:rest) =
            case tryDecode fmt of
              Right bs ->
                case accept bs of
                  Just ok -> Just ok
                  Nothing ->
                    case mFormat of
                      Just _ ->
                        Nothing
                      Nothing ->
                        tryFormats rest
              Left _ -> tryFormats rest

      case tryFormats formats of
        Just bs -> pure bs
        Nothing -> throwError $ ErrorCall $ "builtins.convertHash: could not decode hash " <> show body


placeHolderNix :: forall t f m e . MonadNix e t f m => NValue t f m -> m (NValue t f m)
placeHolderNix p =
  do
    t <- fromStringNoContext =<< fromValue p
    h <-
      coerce @(Prim m NixString) @(m NixString) $
        (hashStringNix `on` mkNixStringWithoutContext)
          "sha256"
          ("nix-output:" <> t)
    toValue
      $ mkNixStringWithoutContext
      $ Text.cons '/'
      $ Base32.encode
      -- Please, stop Text -> Bytestring here after migration to Text
      $ case Base16.decode (bytes h) of -- The result coming out of hashString is base16 encoded
#if MIN_VERSION_base16_bytestring(1,0,0)
        -- Please, stop Text -> String here after migration to Text
        Left e -> error $ "Couldn't Base16 decode the text: '" <> body h <> "'.\nThe Left fail content: '" <> show e <> "'."
        Right d -> d
#else
        (d, "") -> d
        (_, e) -> error $ "Couldn't Base16 decode the text: '" <> body h <> "'.\nUndecodable remainder: '" <> show e <> "'."
#endif
    where
      bytes :: NixString -> ByteString
      bytes = encodeUtf8 . body

      body = ignoreContext
