{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language MonoLocalBinds #-}
{-# language PartialTypeSignatures #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-name-shadowing #-}


-- | Code that implements Nix builtins. Lists the functions that are built into the Nix expression evaluator. Some built-ins (aka `derivation`), are always in the scope, so they can be accessed by the name. To keap the namespace clean, most built-ins are inside the `builtins` scope - a set that contains all what is a built-in.
module Nix.Builtins
  ( withNixContext
  , builtins
  )
where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Crypto.Hash                   as Hash
import qualified Data.Aeson                    as A
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key                as AKM
import qualified Data.Aeson.KeyMap             as AKM
#endif
import           Data.ByteArray.Encoding        ( Base(Base16, Base64)
                                                , convertFromBase
                                                , convertToBase
                                                )
import qualified Data.ByteString               as B
import           Data.ByteString.Base16        as Base16
import           Data.Fix                       ( foldFix )
import qualified Data.HashSet                  as HS
import qualified Nix.Core.AttrSet              as A
import           Data.Scientific
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as Text
import qualified Data.Time.Clock.POSIX         as Time
import qualified Data.Time.Calendar            as Time
import qualified Data.Time.LocalTime           as Time
import qualified Data.Time.Format              as Time
import           Text.Printf                    ( printf )
import           Data.Fixed                     ( Pico )
import           NeatInterpolation              ( text )
import           Nix.Atoms
import           Nix.Builtins.Arithmetic
import           Nix.Builtins.AttrSet
import           Nix.Builtins.Control
import           Nix.Builtins.Internal
import           Nix.Builtins.List
import           Nix.Builtins.String
import           Nix.Builtins.Type
import           Nix.Convert
import           Nix.Core.List                  ( NixList )
import qualified Nix.Core.List                 as L
import qualified Data.Vector                   as V
import           Nix.Effects
import           Nix.Effects.Basic              ( fetchTarball
                                                , fetchGit
                                                , fetchTree
                                                )
import qualified System.IO.Temp                as Temp
import qualified System.Directory              as Directory
import qualified System.FilePath               as FP
import qualified System.PosixCompat.Files      as Posix
import qualified Data.ByteString               as BS
import           Nix.Exec
import           Nix.Config.Singleton           ( HasProvCfg )
import           Nix.Context                    ( CtxCfg )
import           Nix.Expr.Types
import qualified Nix.Eval                      as Eval
import           Nix.Frames
import           Nix.Json
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Render
import           Nix.FileType
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Monad
import           Nix.XML
import           Data.Dependent.Sum             ( DSum((:=>)) )
import qualified System.Nix.Hash               as StoreHash
import qualified System.Nix.Store.ReadOnly     as StoreRO
import qualified System.Nix.StorePath          as Store
import           System.Nix.Base32             as Base32
import           System.Nix.FileContentAddress  ( FileIngestionMethod(..) )
import           System.Nix.ContentAddress      ( ContentAddressMethod(..) )
import qualified Toml

-- This is a big module. There is recursive reuse:
-- @builtins -> builtinsList -> scopedImport -> withNixContext -> builtins@,
-- since @builtins@ is self-recursive: aka we ship @builtins.builtins.builtins...@.

-- ** Builtin functions

derivationNix
  :: forall e t f m. (MonadNix e t f m, Scoped (NValue t f m) m, HasProvCfg (CtxCfg e))
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

nixPathNix :: forall e t f m . MonadNix e t f m => m (NValue t f m)
nixPathNix =
  fmap
    NVList
    $ foldNixPath mempty $
        \p mn ty rest ->
          pure $
            L.nlSingleton
              (NVSet
                mempty
                (A.fromList
                  [case ty of
                    PathEntryPath -> (mkVarName "path", NVPath  p)
                    PathEntryURI  -> (mkVarName "uri", mkNVStrWithoutContext $ fromString $ coerce p)

                  , (mkVarName "prefix", mkNVStrWithoutContext $ maybeToMonoid mn)
                  ]
                )
              )
            <> rest

-- | Check if a string has context. Returns interned boolean.
hasContextNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
hasContextNix nv = do
  ns <- fromValue nv
  askInternedBool $ hasContext ns

unsafeDiscardOutputDependencyNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
unsafeDiscardOutputDependencyNix nv =
  do
    (nc, ns) <- (getStringContext &&& ignoreContext) <$> fromValue nv
    toValue $ mkNixString (HS.map discard nc) ns
 where
  discard :: StringContext -> StringContext
  discard (StringContext AllOutputs a) = StringContext DirectPath a
  discard x                            = x

addDrvOutputDependenciesNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
addDrvOutputDependenciesNix nv =
  do
    ns <- fromValue =<< demand nv
    let
      ctx = getStringContext ns
      contents = ignoreContext ns
      ctxSize = HS.size ctx

    sc <-
      case HS.toList ctx of
        [single] -> pure single
        _ ->
          throwError $
            ErrorCall $
              "builtins.addDrvOutputDependencies: string context must have exactly one element, but has "
              <> show ctxSize

    let
      path = getStringContextPath sc
      pathText = varNameText path
      ensureDrv =
        when (not (".drv" `Text.isSuffixOf` pathText)) $
          throwError $ ErrorCall $ "builtins.addDrvOutputDependencies: path '" <> show pathText <> "' is not a derivation"

    case getStringContextFlavor sc of
      DirectPath -> do
        ensureDrv
        toValue $ mkNixString (one $ StringContext AllOutputs path) contents
      AllOutputs ->
        pure $ NVStr ns
      DerivationOutput out ->
        throwError $
          ErrorCall $
            "builtins.addDrvOutputDependencies: cannot act on derivation output '" <> show out <> "'"

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
          Nothing -> pure NVNull
          Just v -> toValue v
      _xy -> throwError $ ErrorCall $ "Invalid types for builtins.unsafeGetAttrPosNix: " <> show _xy

baseNameOfNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
baseNameOfNix x =
  do
    ns <- coerceStringlikeToNixString DontCopyToStore x
    pure $
      NVStr $
        modifyNixContents
          (fromString . nixBaseNameOf . toString)
          ns
  where
    -- Nix strips exactly one trailing slash before getting basename.
    -- Multiple trailing slashes are left alone (resulting in empty basename).
    nixBaseNameOf :: String -> String
    nixBaseNameOf s = case reverse s of
      '/':c:rest | c /= '/' -> FP.takeFileName (reverse (c:rest))
      _ -> FP.takeFileName s

builtinsBuiltinNix
  :: forall e t f m
   . MonadNix e t f m
  => m (NValue t f m)
builtinsBuiltinNix = throwError $ ErrorCall "HNix does not provide builtins.builtins at the moment. Using builtins directly should be preferred"

-- a safer version of `attrsetGet`
attrGetOr
  :: forall e t f m v a
   . (MonadNix e t f m, FromValue v m (NValue t f m))
  => a
  -> (v -> m a)
  -> VarName
  -> AttrSet (NValue t f m)
  -> m a
attrGetOr fallback fun name attrs =
  case A.lookup name attrs of
    Nothing -> pure fallback
    Just v -> fun =<< fromValue v


--  NOTE: It is a part of the implementation taken from:
--  https://github.com/haskell-nix/hnix/pull/755
--  look there for `sha256` and/or `filterSource`
pathNix :: forall e t f m. MonadNix e t f m => NValue t f m -> m (NValue t f m)
pathNix arg =
  do
    attrs <- fromValue @(AttrSet (NValue t f m)) arg
    path      <- fmap (coerce . toString) $ fromStringNoContext =<< coerceToPath =<< attrsetGet "path" attrs

    -- TODO: Fail on extra args
    -- XXX: This is a very common pattern, we could factor it out
    name      <- toText <$> attrGetOr (takeFileName path) (fmap (coerce . toString) . fromStringNoContext) "name" attrs
    recursive <- attrGetOr True pure "recursive" attrs

    Right (mkVarName . toText . coerce @StorePath @String -> s) <- addToStore name (NarFile path) recursive False
    -- TODO: Ensure that s matches sha256 when not empty
    pure $ NVStr $ mkNixStrDirectPath s
 where
  coerceToPath = coerceToString callFunc DontCopyToStore CoerceAny

-- | Implementation of builtins.filterSource
-- Signature: filterSource :: (path -> type -> bool) -> path -> path
-- The filter function is called for each file/directory with:
--   path: the full path to the file (string)
--   type: one of "regular", "directory", "symlink", "unknown"
-- Returns true to include the file, false to exclude it.
filterSourceNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
filterSourceNix filterFun nvpath = do
  srcPath <- absolutePathFromValue =<< demand nvpath
  let name = toText $ takeFileName srcPath

  -- Create temp directory
  tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
  tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-filterSource"
  let tmpDirPath = coerce tmpDir :: Path

  -- Copy files that pass the filter to temp directory
  copyFiltered srcPath tmpDirPath srcPath

  -- Add temp directory to store
  res <- addToStore name (NarFile tmpDirPath) True False
  storePath <- case res of
    Left err -> throwError err
    Right v -> pure v
  let s = mkVarName . toText . coerce @StorePath @String $ storePath

  -- Clean up temp directory
  liftIO $ Directory.removeDirectoryRecursive tmpDir

  pure $ NVStr $ mkNixStrDirectPath s
 where
  -- Convert FileType to the string that Nix uses
  fileTypeToString :: FileType -> Text
  fileTypeToString = \case
    FileTypeRegular   -> "regular"
    FileTypeDirectory -> "directory"
    FileTypeSymlink   -> "symlink"
    FileTypeUnknown   -> "unknown"

  -- Call the user's filter function
  applyFilter :: Path -> FileType -> m Bool
  applyFilter path fileType = do
    pathArg <- toValue $ mkNixStringWithoutContext $ toText path
    typeArg <- toValue $ mkNixStringWithoutContext $ fileTypeToString fileType
    result <- callFunc filterFun pathArg >>= (`callFunc` typeArg)
    fromValue result

  -- Recursively copy files that pass the filter
  copyFiltered :: Path -> Path -> Path -> m ()
  copyFiltered srcRoot destRoot currentPath = do
    status <- getSymbolicLinkStatus currentPath
    let fileType = fileTypeFromStatus status
        relPath = FP.makeRelative (coerce srcRoot) (coerce currentPath)
        destPath = coerce destRoot FP.</> relPath

    -- Check if this path passes the filter
    include <- applyFilter currentPath fileType

    when include $ do
      case fileType of
        FileTypeDirectory -> do
          liftIO $ Directory.createDirectoryIfMissing True destPath
          items <- listDirectory currentPath
          traverse_ (copyFiltered srcRoot destRoot . (currentPath </>)) items

        FileTypeSymlink -> do
          liftIO $ Directory.createDirectoryIfMissing True (FP.takeDirectory destPath)
          target <- liftIO $ Posix.readSymbolicLink (coerce currentPath)
          liftIO $ Posix.createSymbolicLink target destPath

        FileTypeRegular -> do
          liftIO $ Directory.createDirectoryIfMissing True (FP.takeDirectory destPath)
          liftIO $ do
            contents <- BS.readFile (coerce currentPath)
            BS.writeFile destPath contents
            Posix.setFileMode destPath (Posix.fileMode status)

        FileTypeUnknown ->
          -- Skip unknown file types (matches Nix behavior)
          pass

dirOfNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
dirOfNix nvdir =
  do
    dir <- demand nvdir

    case dir of
      NVStr ns -> pure $ NVStr $ modifyNixContents (fromString . nixDirOf . toString) ns
      NVPath path -> pure $ NVPath $ takeDirectory path
      v -> throwError $ ErrorCall $ "dirOf: expected string or path, got " <> show v
  where
    -- Nix's dirOf for strings: return everything before the last '/'.
    -- If no '/' exists, return ".". If result would be empty, return "/".
    nixDirOf :: String -> String
    nixDirOf s = case findLastSlash s of
      Nothing -> "."
      Just i  -> case take i s of
        "" -> "/"
        r  -> r
    findLastSlash :: String -> Maybe Int
    findLastSlash s = case reverse s of
      [] -> Nothing
      xs -> case length s - 1 - findIndex xs of
        n | n < 0 -> Nothing
        n -> Just n
      where
        findIndex ('/':_) = 0
        findIndex (_:rest) = 1 + findIndex rest
        findIndex [] = length s  -- no slash found

unsafeDiscardStringContextNix
  :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
unsafeDiscardStringContextNix =
  inHask (mkNixStringWithoutContext . ignoreContext)

toFileNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
toFileNix name s =
  do
    name' <- fromStringNoContext =<< fromValue name
    s'    <- fromValue s
    mres  <-
      toFile_
        (coerce $ toString name')
        (ignoreContext s')

    let storepath = mkVarName $ toText $ coerce @StorePath @String mres
    toValue $ mkNixStrDirectPath storepath

toPathNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toPathNix = inHask @Path id

storeDirText :: Options -> Text
storeDirText opts = Text.dropWhileEnd (== '/') $ toText $ getStoreDir opts

storeDirPrefix :: Options -> Text
storeDirPrefix opts = storeDirText opts <> "/"

isStorePath :: Options -> Path -> Bool
isStorePath opts path =
  let
    pathText = toText path
    dirText = storeDirText opts
  in pathText == dirText || storeDirPrefix opts `Text.isPrefixOf` pathText

storePathNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
storePathNix nvpath =
  do
    path <- absolutePathFromValue =<< demand nvpath
    let
      pathText = toText path
      varPath = mkVarName pathText
    opts <- askOptions
    when (not (storeDirPrefix opts `Text.isPrefixOf` pathText)) $
      throwError $ ErrorCall $ "builtins.storePath: path '" <> show pathText <> "' is not in the Nix store (" <> show (storeDirText opts) <> ")"
    toValue $ mkNixStrDirectPath varPath

-- | Check if a path exists. Returns interned boolean.
pathExistsNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
pathExistsNix nvpath =
  do
    v <- demand nvpath
    opts <- askOptions
    path <- case v of
      NVPath p  -> pure p
      NVStr  ns -> pure $ coerce $ toString $ ignoreContext ns
      _v -> throwError $ ErrorCall $ "builtins.pathExists: expected path, got " <> show _v
    exists <-
      if isStorePath opts path
        then storePathExists path
        else doesPathExist path
    askInternedBool exists

-- | Implementation of Nix @import@ clause.
--
-- Because Nix @import@s work strictly
-- (import gets fully evaluated befor bringing it into the scope it was called from)
-- - that property raises a requirement for execution phase of the interpreter go into evaluation phase
-- & then also go into parsing phase on the imports.
-- So it is not possible (more precise - not practical) to do a full parse Nix code phase fully & then go into evaluation phase.
-- As it is not possible to "import them lazily", as import is strict & it is not possible to establish
-- what imports whould be needed up until where it would be determined & they import strictly
--
importNix
  :: forall e t f m . (MonadNix e t f m, HasProvCfg (CtxCfg e)) => NValue t f m -> m (NValue t f m)
importNix = scopedImportNix $ NVSet emptyPositionSet mempty

-- | @scopedImport scope path@
-- An undocumented secret powerful function.
--
-- At the same time it is strongly forbidden to be used, as prolonged use of it would bring devastating consequences.
-- As it is essentially allows rewriting(redefinition) paradigm.
--
-- Allows to import the environment into the scope of a file expression that gets imported.
-- It is as if the contents at @path@ were given to @import@ wrapped as: @with scope; path@
-- meaning:
--
-- > -- Nix pseudocode:
-- > import (with scope; path)
--
-- For example, it allows to use itself as:
-- > bar = scopedImport pkgs ./bar.nix;
-- > -- & declare @./bar.nix@ without a header, so as:
-- > stdenv.mkDerivation { ... buildInputs = [ libfoo ]; }
--
-- But that breaks the evaluation/execution sharing of the @import@s.
--
-- Function also allows to redefine or extend the builtins.
--
-- For instance, to trace all calls to function ‘map’:
--
-- >  let
-- >    overrides = {
-- >      map = f: xs: builtins.trace "call of map!" (map f xs);
--
-- >      # Propagate override by calls to import&scopedImport.
-- >      import = fn: scopedImport overrides fn;
-- >      scopedImport = attrs: fn: scopedImport (overrides // attrs) fn;
--
-- >      # Update ‘builtins’.
-- >      builtins = builtins // overrides;
-- >    };
-- >  in scopedImport overrides ./bla.nix
--
-- In the related matter the function can be added and passed around as builtin.
scopedImportNix
  :: forall e t f m
   . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
scopedImportNix asetArg pathArg =
  do
    (coerce -> scope) <- fromValue @(AttrSet (NValue t f m)) asetArg
    p <- fromValue pathArg

    path  <- pathToDefaultNix @t @f @m p
    path' <-
      do
        mres <- lookupVar "__cur_file"
        case mres of
          Nothing -> do
            traceM "No known current directory"
            pure path
          Just res -> do
            p' <- fromValue @Path =<< demand res

            traceM $ "Current file being evaluated is: " <> show p'
            pure $ takeDirectory p' </> path

    clearScopes @(NValue t f m)
      $ withNixContext (pure path')
      $ pushScope scope
      $ importPath @t @f @m path'

getEnvNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
getEnvNix v =
  (toValue . mkNixStringWithoutContext . maybeToMonoid) =<< getEnvVar =<< fromStringNoContext =<< fromValue v

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

readFileNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
readFileNix nvpath = do
  path <- absolutePathFromValue =<< demand nvpath
  opts <- askOptions
  contents <-
    if isStorePath opts path
      then do
        res <- readStoreFile path
        bytes <- case res of
          Left err -> throwError err
          Right v -> pure v
        pure $ decodeUtf8 bytes
      else
        Nix.Render.readFile path
  toValue contents

readFileTypeNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
readFileTypeNix nvpath =
  do
    path <- absolutePathFromValue =<< demand nvpath
    opts <- askOptions
    t <-
      if isStorePath opts path
        then do
          res <- readStoreFileType path
          case res of
            Left err -> throwError err
            Right v -> pure v
        else fileTypeFromStatus <$> getSymbolicLinkStatus path

    toValue t

findFileNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
findFileNix nvaset nvfilepath =
  do
    aset <- demand nvaset
    filePath <- demand nvfilepath

    case (aset, filePath) of
      (NVList x, NVStr ns) ->
        do
          mres <- findPath @t @f @m x $ coerce $ toString $ ignoreContext ns

          pure $ NVPath mres

      (NVList _, _y     ) -> throwError $ ErrorCall $ "expected a string, got " <> show _y
      (_x      , NVStr _) -> throwError $ ErrorCall $ "expected a list, got " <> show _x
      (_x      , _y     ) -> throwError $ ErrorCall $ "Invalid types for builtins.findFile: " <> show (_x, _y)

readDirNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
readDirNix nvpath =
  do
    path           <- absolutePathFromValue =<< demand nvpath
    opts <- askOptions

    let
      -- | Function indeed binds filepaths as keys ('VarNames') in Nix attrset.
      detectFileTypes :: Path -> m (VarName, FileType)
      detectFileTypes item =
        do
          s <- getSymbolicLinkStatus $ path </> item
          let t = fileTypeFromStatus s

          pure (mkVarName $ toText item, t)

    itemsWithTypes <-
      if isStorePath opts path
        then do
          res <- readStoreDir path
          entries <- case res of
            Left err -> throwError err
            Right v -> pure v
          pure $
            (\(p, t) -> (mkVarName $ fromString $ coerce p, t)) <$> entries
        else do
          items <- listDirectory path
          traverse detectFileTypes items

    (coerce :: CoerceDeeperToNValue t f m) <$> toValue (A.fromList itemsWithTypes)

outputOfNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
outputOfNix nvDrvRef nvOutputName =
  do
    drvRef <- fromValue =<< demand nvDrvRef
    outputName <- fromStringNoContext =<< fromValue =<< demand nvOutputName

    let
      contents = ignoreContext drvRef
      ctx = getStringContext drvRef

    drvPath <-
      case HS.toList ctx of
        [sc] -> do
          let p = getStringContextPath sc
          ensureDrvPath p
          pure p
        [] -> do
          let p = mkVarName contents
          ensureDrvText contents
          pure p
        _ ->
          throwError $
            ErrorCall $
              "builtins.outputOf: string context must have exactly one element, but has "
              <> show (HS.size ctx)

    toValue $ mkNixString (one $ StringContext (DerivationOutput outputName) drvPath) contents
 where
  ensureDrvText :: Text -> m ()
  ensureDrvText p =
    when (not (".drv" `Text.isSuffixOf` p)) $
      throwError $ ErrorCall $ "builtins.outputOf: path '" <> show p <> "' is not a derivation"

  ensureDrvPath :: VarName -> m ()
  ensureDrvPath = ensureDrvText . varNameText

fromJSONNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fromJSONNix nvjson =
  do
    j <- demand nvjson
    jText <- fromStringNoContext =<< fromValue j

    case A.eitherDecodeStrict' @A.Value $ encodeUtf8 jText of
      Left jsonError -> throwError $ ErrorCall $ "builtins.fromJSON: " <> jsonError
      Right value -> jsonToNValue value

 where
  jsonToNValue :: (A.Value -> m (NValue t f m))
  jsonToNValue =
    \case
      A.Object m ->
        traverseToNValue
          (NVSet emptyPositionSet)
#if MIN_VERSION_aeson(2,0,0)
          (A.fromList [(mkVarName (AKM.toText k), v) | (k, v) <- AKM.toList m])
#else
          (A.fromList [(mkVarName k, v) | (k, v) <- HM.toList m])
#endif
      A.Array  l -> NVList . L.nlFromList <$> traverse jsonToNValue (toList l)
      A.String s -> pure $ mkNVStrWithoutContext s
      A.Number n ->
        pure $
          NVConstant $
            case floatingOrInteger n of
              Left f -> NFloat f
              Right i -> NInt i
      A.Bool   b -> pure $ NVBool b
      A.Null     -> pure NVNull
   where
    traverseToNValue :: Traversable t0 => (t0 (NValue t f m) -> b) -> t0 A.Value -> m b
    traverseToNValue f v = f <$> traverse jsonToNValue v

fromTOMLNix
  :: forall e t f m . MonadNix e t f m
  => NValue t f m -> m (NValue t f m)
fromTOMLNix nvtoml = do
  tomlText <- fromStringNoContext =<< fromValue =<< demand nvtoml
  case Toml.parse tomlText of
    Left err -> throwError $ ErrorCall $ "builtins.fromTOML: " <> err
    Right table -> tableToNValue table
 where
  tableToNValue :: Toml.Table' Toml.Position -> m (NValue t f m)
  tableToNValue (Toml.MkTable m) =
    fmap (NVSet emptyPositionSet) . traverse tomlToNValue $
      A.fromList [(mkVarName k, v) | (k, (_, v)) <- M.toList m]

  tomlToNValue :: Toml.Value' Toml.Position -> m (NValue t f m)
  tomlToNValue = \case
    Toml.Integer' _ n
      | n > fromIntegral (maxBound :: Int64) -> throwError $ ErrorCall $ "builtins.fromTOML: integer too large: " <> show n
      | n < fromIntegral (minBound :: Int64) -> throwError $ ErrorCall $ "builtins.fromTOML: integer too small: " <> show n
      | otherwise -> pure $ NVConstant $ NInt (fromIntegral n)
    Toml.Double' _ d  -> pure $ NVConstant $ NFloat (realToFrac d)
    Toml.Bool' _ b    -> pure $ NVConstant $ NBool b
    Toml.Text' _ t    -> pure $ mkNVStrWithoutContext t
    Toml.List' _ xs   -> NVList <$> traverse tomlToNValue (L.nlFromList xs)
    Toml.Table' _ t   -> tableToNValue t
    -- Date/time types: convert to { _type = "timestamp"; value = "..."; }
    Toml.Day' _ d         -> mkTimestamp $ formatDay d
    Toml.TimeOfDay' _ t   -> mkTimestamp $ formatTimeOfDay t
    Toml.LocalTime' _ lt  -> mkTimestamp $ formatLocalTime lt
    Toml.ZonedTime' _ zt  -> mkTimestamp $ formatZonedTime zt

  mkTimestamp :: Text -> m (NValue t f m)
  mkTimestamp value = pure $ NVSet emptyPositionSet $ A.fromList
    [ (mkVarName "_type", mkNVStrWithoutContext "timestamp")
    , (mkVarName "value", mkNVStrWithoutContext value)
    ]

  formatDay :: Time.Day -> Text
  formatDay = toText . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

  formatTimeOfDay :: Time.TimeOfDay -> Text
  formatTimeOfDay tod = toText $ formatTimeOfDayWithPrecision tod

  formatLocalTime :: Time.LocalTime -> Text
  formatLocalTime lt = toText $
    Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" (Time.localDay lt) <>
    "T" <> formatTimeOfDayWithPrecision (Time.localTimeOfDay lt)

  formatZonedTime :: Time.ZonedTime -> Text
  formatZonedTime zt = toText $
    Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" (Time.localDay (Time.zonedTimeToLocalTime zt)) <>
    "T" <> formatTimeOfDayWithPrecision (Time.localTimeOfDay (Time.zonedTimeToLocalTime zt)) <>
    formatTimeZone (Time.zonedTimeZone zt)

  -- Format TimeOfDay with correct precision for fractional seconds
  formatTimeOfDayWithPrecision :: Time.TimeOfDay -> String
  formatTimeOfDayWithPrecision (Time.TimeOfDay h m s) =
    let baseTime = printf "%02d:%02d:%02d" h m (floor s :: Int)
        frac = s - fromIntegral (floor s :: Int)
    in if frac == 0
       then baseTime
       else baseTime <> formatFractionalSeconds frac

  -- Format fractional seconds using Nix's precision tiers:
  -- - 3 digits for milliseconds (when us and ns digits are zero)
  -- - 6 digits for microseconds (when ns digits are zero)
  -- - 9 digits for nanoseconds (otherwise)
  formatFractionalSeconds :: Pico -> String
  formatFractionalSeconds pico =
    let -- Convert to nanoseconds (9 decimal places max)
        nanos = round (pico * 1e9) :: Integer
    in case () of
      _ | nanos `mod` 1000000 == 0 ->
            -- Millisecond precision: 3 digits
            printf ".%03d" (nanos `div` 1000000)
        | nanos `mod` 1000 == 0 ->
            -- Microsecond precision: 6 digits
            printf ".%06d" (nanos `div` 1000)
        | otherwise ->
            -- Nanosecond precision: 9 digits
            printf ".%09d" nanos

  formatTimeZone :: Time.TimeZone -> String
  formatTimeZone tz
    | Time.timeZoneMinutes tz == 0 = "Z"
    | otherwise =
        -- Convert "+0000" format to "+00:00" format
        let s = Time.formatTime Time.defaultTimeLocale "%z" tz
        in take 3 s <> ":" <> drop 3 s

toJSONNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toJSONNix = (fmap NVStr . toJSONNixString) <=< demand

toXMLNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toXMLNix = (fmap (NVStr . toXML) . normalForm) <=< demand

execNix
  :: forall e t f m . (MonadNix e t f m, HasProvCfg (CtxCfg e)) => NValue t f m -> m (NValue t f m)
execNix xs = do
  -- 2018-11-19: NOTE: Still need to do something with the context here
  -- See prim_exec in nix/src/libexpr/primops.cc
  -- Requires the implementation of EvalState::realiseContext
  v <- fromValue @(NixList (NValue t f m)) xs
  strs <- L.nlMapM (coerceStringlikeToNixString DontCopyToStore) v
  exec $ V.fromList $ ignoreContext <$> L.nlToList strs

-- | Compute fixed-output store path using hnix-store-readonly.
makeFixedOutputPathLocal
  :: Store.StoreDir
  -> FileIngestionMethod
  -> Hash.Digest Hash.SHA256
  -> Store.StorePathName
  -> Store.StorePath
makeFixedOutputPathLocal storeDir' method digest name =
  let caMethod = case method of
        FileIngestionMethod_NixArchive -> ContentAddressMethod_NixArchive
        FileIngestionMethod_Flat -> ContentAddressMethod_Flat
  in StoreRO.makeFixedOutputPath storeDir' caMethod (StoreHash.HashAlgo_SHA256 :=> digest) mempty name

fetchurlNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fetchurlNix =
  (\case
    NVSet _ s -> do
      let mUrlsVal = A.lookup (mkVarName "urls") s <|> A.lookup (mkVarName "url") s
      urlsVal <- case mUrlsVal of
        Nothing -> throwError $ ErrorCall "builtins.fetchurl: missing url(s)"
        Just v -> pure v
      urls <- extractUrls =<< demand urlsVal
      mHashVal <- traverse (fromValue <=< demand) (A.lookup (mkVarName "hash") s)
      mShaVal <- traverse (fromValue <=< demand) (A.lookup (mkVarName "sha256") s)
      mNameVal <- traverse (fromValue <=< demand) (A.lookup (mkVarName "name") s)
      mExecVal <- traverse (fromValue <=< demand) (A.lookup (mkVarName "executable") s)
      fetchUrls mHashVal mShaVal mNameVal
        (case mExecVal of
          Nothing -> False
          Just v -> v
        )
        urls
    v@NVStr{} -> fetchUrls Nothing Nothing Nothing False =<< extractUrls v
    v@NVList{} -> fetchUrls Nothing Nothing Nothing False =<< extractUrls v
    v -> throwError $ ErrorCall $ "builtins.fetchurl: Expected URI or set, got " <> show v
  ) <=< demand

 where
  fetchUrls :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> [Text] -> m (NValue t f m)
  fetchUrls mHashRaw mShaRaw mNameRaw executable urls = do
    opts <- askOptions
    let urlsExpanded = concatMap expandUrl urls
    let mHash = nonEmptyText mHashRaw
    let mSha = nonEmptyText mShaRaw
    let mName = nonEmptyText mNameRaw

    when (null urlsExpanded) $
      throwError $ ErrorCall "builtins.fetchurl: empty URL list"

    let defaultName = case urls of
          (u:_) -> baseNameOf u
          [] -> "source"
    let name =
          case mName of
            Nothing -> defaultName
            Just v -> v

    storeName <- case Store.mkStorePathName name of
      Left err ->
        throwError $ ErrorCall $ "builtins.fetchurl: invalid store path name '" <> show name <> "': " <> show err
      Right n -> pure n

    -- Use the configured store directory for path computation.
    -- When using overlay mode, addToStore uses this directory.
    -- When using remote mode (nix-daemon), /nix/store is always used anyway.
    let storeDir = Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts

    mDigest <- case (mHash, mSha) of
      (Just h, _) ->
        case mkDigestFromHash h of
          Left err -> throwError $ ErrorCall $ "builtins.fetchurl: " <> err
          Right d -> pure $ Just d
      (Nothing, Just sha) ->
        case StoreHash.mkNamedDigest "sha256" sha of
          Left err -> throwError $ ErrorCall $ "builtins.fetchurl: " <> err
          Right d -> pure $ Just d
      _ -> pure Nothing

    let ingestionMethod =
          if executable
            then FileIngestionMethod_NixArchive
            else FileIngestionMethod_Flat
    let mExpected = case mDigest of
          Just (StoreHash.HashAlgo_SHA256 :=> digest) ->
            -- Use local implementation that correctly handles flat vs recursive hashes
            Just $ toStorePathWithDir storeDir $
              makeFixedOutputPathLocal
                storeDir
                ingestionMethod
                digest
                storeName
          _ -> Nothing

    case mExpected of
      Just expectedPath -> do
        exists <- storePathExists (coerce expectedPath)
        if exists
          then do
            when (getVerbosity opts >= Talkative) $
              traceEffect @t @f @m $ "fetchurl: using existing " <> show expectedPath
            toValue expectedPath
          else go opts mExpected name executable urlsExpanded
      Nothing ->
        go opts mExpected name executable urlsExpanded
    where
      go _opts _mExpected _name _exec [] =
        throwError $ ErrorCall "builtins.fetchurl: empty URL list"
      go opts mExpected name exec (u:us) = do
        when (getVerbosity opts >= Talkative) $
          traceEffect @t @f @m $ "fetchurl: " <> toString u
        res <- fetchURLWithNameAndExecutable u name exec
        case res of
          Right sp ->
            case mExpected of
              Just expected
                | sp /= expected ->
                    if null us
                      then throwError $ ErrorCall $
                        "builtins.fetchurl: hash mismatch for " <> show u
                          <> "\n  expected: " <> show expected
                          <> "\n  actual:   " <> show sp
                      else go opts mExpected name exec us
              _ -> toValue sp
          Left err ->
            if null us
              then throwError err
              else go opts mExpected name exec us

  -- Heuristic fallback for GNU FTP URLs that time out in some environments.
  expandUrl :: Text -> [Text]
  expandUrl u =
    case () of
      _
        | Just rest <- Text.stripPrefix "https://git.savannah.gnu.org/cgit/" u
        -> [u, "https://cgit.git.savannah.gnu.org/cgit/" <> rest]
        | Just rest <- Text.stripPrefix "http://git.savannah.gnu.org/cgit/" u
        -> [u, "https://cgit.git.savannah.gnu.org/cgit/" <> rest]
        | Just rest <- Text.stripPrefix "https://ftp.gnu.org/gnu/" u <|> Text.stripPrefix "http://ftp.gnu.org/gnu/" u
        -> [u, "https://ftpmirror.gnu.org/gnu/" <> rest]
        | otherwise
        -> [u]

  extractUrls :: NValue t f m -> m [Text]
  extractUrls = \case
    NVStr ns ->
      case getStringNoContext ns of
        Nothing -> throwError $ ErrorCall "builtins.fetchurl: unsupported arguments to url"
        Just v -> pure $ pure v
    NVList vs -> L.nlToList <$> L.nlMapM (extractUrl <=< demand) vs
    v -> throwError $ ErrorCall $ "builtins.fetchurl: Expected URI or list of URIs, got " <> show v

  extractUrl :: NValue t f m -> m Text
  extractUrl = \case
    NVStr ns ->
      case getStringNoContext ns of
        Nothing -> throwError $ ErrorCall "builtins.fetchurl: unsupported arguments to url"
        Just v -> pure v
    v -> throwError $ ErrorCall $ "builtins.fetchurl: Expected URI string, got " <> show v

  nonEmptyText :: Maybe Text -> Maybe Text
  nonEmptyText = \case
    Just t | Text.null t -> Nothing
    other -> other

  mkDigestFromHash h =
    let (algo, rest) = Text.breakOn "-" h
    in if Text.null algo || Text.null rest
      then StoreHash.mkNamedDigest "sha256" h
      else StoreHash.mkNamedDigest algo h

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

getContextNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
getContextNix =
  \case
    (NVStr ns) ->
      NVSet emptyPositionSet <$> traverseToValue (getNixLikeContext $ toNixLikeContext $ getStringContext ns)
    x -> throwError $ ErrorCall $ "Invalid type for builtins.getContext: " <> show x
  <=< demand

appendContextNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
appendContextNix tx ty =
  do
    x <- demand tx
    y <- demand ty

    case (x, y) of
      (NVStr ns, NVSet _ attrs) ->
        do
          let
            getPathNOuts :: NValue t f m -> m NixLikeContextValue
            getPathNOuts tx =
              do
                x <- demand tx

                case x of
                  NVSet _ atts ->
                    do
                      -- TODO: Fail for unexpected keys.

                      let
                        getK :: VarName -> m Bool
                        getK k =
                          case A.lookup k atts of
                            Nothing -> pure False
                            Just v -> fromValue =<< demand v

                        getOutputs :: m [Text]
                        getOutputs =
                          case A.lookup (mkVarName "outputs") atts of
                            Nothing -> stub
                            Just touts -> do
                              outs <- demand touts
                              case outs of
                                NVList vs -> L.nlToList <$> L.nlMapM (fmap ignoreContext . fromValue) vs
                                _x -> throwError $ ErrorCall $ "Invalid types for context value outputs in builtins.appendContext: " <> show _x

                      path <- getK "path"
                      allOutputs <- getK "allOutputs"

                      NixLikeContextValue path allOutputs <$> getOutputs

                  _x -> throwError $ ErrorCall $ "Invalid types for context value in builtins.appendContext: " <> show _x
            addContext :: AttrSet NixLikeContextValue -> NixString
            addContext newContextValues =
              mkNixString
                (fromNixLikeContext $
                  NixLikeContext $
                    A.unionWith
                      (<>)
                      newContextValues
                      $ getNixLikeContext $
                          toNixLikeContext $
                            getStringContext ns
                )
                $ ignoreContext ns

          toValue . addContext =<< traverse getPathNOuts attrs

      _xy -> throwError $ ErrorCall $ "Invalid types for builtins.appendContext: " <> show _xy


nixVersionNix :: MonadNix e t f m => m (NValue t f m)
nixVersionNix = toValue $ mkNixStringWithoutContext "2.18"

langVersionNix :: MonadNix e t f m => m (NValue t f m)
langVersionNix = toValue (5 :: Int)

-- ** @builtinsList@

builtinsList :: forall e t f m . (MonadNix e t f m, HasProvCfg (CtxCfg e)) => m [Builtin (NValue t f m)]
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
    , add0 Normal   "false"            askInternedFalse
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
    , add0 Normal   "null"             askInternedNull
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
    , add0 Normal   "true"             askInternedTrue
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
   . (MonadNix e t f m, Has e Options, HasProvCfg (CtxCfg e))
  => Maybe Path
  -> m r
  -> m r
withNixContext mpath action =
  do
    base <- builtins
    opts <- askOptions

    pushScope
      (one ("__includes", NVList $ L.nlFromList $ mkNVStrWithoutContext . fromString . coerce <$> getInclude opts))
      (pushScopes
        base $
        case mpath of
          Nothing -> action
          Just path -> do
            traceM $ "Setting __cur_file = " <> show path
            pushScope (one ("__cur_file", NVPath path)) action
      )

builtins
  :: forall e t f m
  . ( MonadNix e t f m
     , Scoped (NValue t f m) m
     , HasProvCfg (CtxCfg e)
     )
  => m (Scopes m (NValue t f m))
builtins =
  do
    ref <- defer $ NVSet emptyPositionSet <$> buildMap
    (`pushScope` askScopes) . coerce . A.fromList . ((mkVarName "builtins", ref) :) =<< topLevelBuiltins
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
