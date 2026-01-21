{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Path and filesystem builtins.
--
-- This module contains builtins for path and filesystem operations:
-- baseNameOf, dirOf, path, pathExists, readFile, readFileType, readDir,
-- findFile, toFile, toPath, storePath, filterSource, import, scopedImport,
-- fromJSON, fromTOML, toJSON, toXML, getEnv, nixPath.
module Nix.Builtins.Path
  ( -- * Path manipulation
    baseNameOfNix
  , dirOfNix
  , toPathNix
    -- * Store operations
  , pathNix
  , storePathNix
  , toFileNix
  , filterSourceNix
    -- * Filesystem queries
  , pathExistsNix
  , readFileNix
  , readFileTypeNix
  , readDirNix
  , findFileNix
    -- * Import operations (parameterized to avoid circular deps)
  , scopedImportNixWith
  , importNixWith
  , getEnvNix
  , nixPathNix
    -- * Serialization
  , fromJSONNix
  , fromTOMLNix
  , toJSONNix
  , toXMLNix
    -- * Helpers (exported for Builtins.hs)
  , storeDirText
  , storeDirPrefix
  , isStorePath
  , attrGetOr
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Key                as AKM
import qualified Data.Aeson.KeyMap             as AKM
import qualified Nix.Core.AttrSet              as NixA
import           Data.Scientific                ( floatingOrInteger )
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as Text
import qualified Data.Time.Clock.POSIX         as Time
import qualified Data.Time.Calendar            as Time
import qualified Data.Time.LocalTime           as Time
import qualified Data.Time.Format              as Time
import           Text.Printf                    ( printf )
import           Data.Fixed                     ( Pico )
import qualified System.IO.Temp                as Temp
import qualified System.Directory              as Directory
import qualified System.FilePath               as FP
import qualified System.PosixCompat.Files      as Posix
import qualified Data.ByteString               as BS
import           Nix.Atoms
import           Nix.Builtins.Internal
import           Nix.Config.Singleton           ( HasProvCfg )
import           Nix.Context                    ( CtxCfg )
import           Nix.Convert
import qualified Nix.Core.List                 as L
import           Nix.Effects
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.FileType
import           Nix.Frames
import           Nix.Json
import           Nix.Normal
import           Nix.Options
import           Nix.Render                     ( readFile
                                                , listDirectory
                                                , doesPathExist
                                                , getSymbolicLinkStatus
                                                )
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Interned             ( internedBool, internedNull )
import           Nix.Value.Monad
import           Nix.XML
import           Nix.Types.VarName.Static       ( sPath, sUri, sPrefix, sValue, sCurFile )
import qualified Toml


-- * Path manipulation

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

toPathNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toPathNix = inHask @Path id


-- * Store operations

-- | A safer version of `attrsetGet`
attrGetOr
  :: forall e t f m v a
   . (MonadNix e t f m, FromValue v m (NValue t f m))
  => a
  -> (v -> m a)
  -> VarName
  -> AttrSet (NValue t f m)
  -> m a
attrGetOr fallback fun name attrs =
  case NixA.lookup name attrs of
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


-- * Filesystem queries

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
    pure $ internedBool exists

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

    (coerce :: CoerceDeeperToNValue t f m) <$> toValue (NixA.fromList itemsWithTypes)

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


-- * Helpers

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


-- * Import operations

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
-- For instance, to trace all calls to function 'map':
--
-- >  let
-- >    overrides = {
-- >      map = f: xs: builtins.trace "call of map!" (map f xs);
--
-- >      # Propagate override by calls to import&scopedImport.
-- >      import = fn: scopedImport overrides fn;
-- >      scopedImport = attrs: fn: scopedImport (overrides // attrs) fn;
--
-- >      # Update 'builtins'.
-- >      builtins = builtins // overrides;
-- >    };
-- >  in scopedImport overrides ./bla.nix
--
-- In the related matter the function can be added and passed around as builtin.
--
-- NOTE: Takes withNixContext as a parameter to avoid circular module dependencies.
scopedImportNixWith
  :: forall e t f m
   . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => (Maybe Path -> m (NValue t f m) -> m (NValue t f m))
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
scopedImportNixWith withNixContext asetArg pathArg =
  do
    (coerce -> scope) <- fromValue @(AttrSet (NValue t f m)) asetArg
    p <- fromValue pathArg

    path  <- pathToDefaultNix @t @f @m p
    path' <-
      do
        mres <- lookupVar sCurFile
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
-- NOTE: Takes withNixContext as a parameter to avoid circular module dependencies.
importNixWith
  :: forall e t f m
   . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => (Maybe Path -> m (NValue t f m) -> m (NValue t f m))
  -> NValue t f m
  -> m (NValue t f m)
importNixWith withNixContext = scopedImportNixWith withNixContext $ NVSet emptyPositionSet mempty

getEnvNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
getEnvNix v =
  (toValue . mkNixStringWithoutContext . maybeToMonoid) =<< getEnvVar =<< fromStringNoContext =<< fromValue v

nixPathNix :: forall e t f m . MonadNix e t f m => m (NValue t f m)
nixPathNix =
  fmap
    NVList
    $ foldNixPath mempty $
        \p mn ty rest ->
          pure $
            L.singleton
              (NVSet
                mempty
                (NixA.fromList
                  [case ty of
                    PathEntryPath -> (sPath, NVPath  p)
                    PathEntryURI  -> (sUri, mkNVStrWithoutContext $ fromString $ coerce p)

                  , (sPrefix, mkNVStrWithoutContext $ maybeToMonoid mn)
                  ]
                )
              )
            <> rest


-- * Serialization

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
          (NixA.fromList [(mkVarName (AKM.toText k), v) | (k, v) <- AKM.toList m])
      A.Array  l -> NVList . L.fromList <$> traverse jsonToNValue (toList l)
      A.String s -> pure $ mkNVStrWithoutContext s
      A.Number n ->
        pure $
          NVConstant $
            case floatingOrInteger n of
              Left f -> NFloat f
              Right i -> NInt i
      A.Bool   b -> pure $ internedBool b
      A.Null     -> pure internedNull
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
      NixA.fromList [(mkVarName k, v) | (k, (_, v)) <- M.toList m]

  tomlToNValue :: Toml.Value' Toml.Position -> m (NValue t f m)
  tomlToNValue = \case
    Toml.Integer' _ n
      | n > fromIntegral (maxBound :: Int64) -> throwError $ ErrorCall $ "builtins.fromTOML: integer too large: " <> show n
      | n < fromIntegral (minBound :: Int64) -> throwError $ ErrorCall $ "builtins.fromTOML: integer too small: " <> show n
      | otherwise -> pure $ NVConstant $ NInt (fromIntegral n)
    Toml.Double' _ d  -> pure $ NVConstant $ NFloat (realToFrac d)
    Toml.Bool' _ b    -> pure $ internedBool b
    Toml.Text' _ t    -> pure $ mkNVStrWithoutContext t
    Toml.List' _ xs   -> NVList <$> traverse tomlToNValue (L.fromList xs)
    Toml.Table' _ t   -> tableToNValue t
    -- Date/time types: convert to { _type = "timestamp"; value = "..."; }
    Toml.Day' _ d         -> mkTimestamp $ formatDay d
    Toml.TimeOfDay' _ t   -> mkTimestamp $ formatTimeOfDay t
    Toml.LocalTime' _ lt  -> mkTimestamp $ formatLocalTime lt
    Toml.ZonedTime' _ zt  -> mkTimestamp $ formatZonedTime zt

  mkTimestamp :: Text -> m (NValue t f m)
  mkTimestamp value = pure $ NVSet emptyPositionSet $ NixA.fromList
    [ (mkVarName "_type", mkNVStrWithoutContext "timestamp")
    , (sValue, mkNVStrWithoutContext value)
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
