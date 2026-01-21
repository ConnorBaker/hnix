{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Fetch and IO builtins.
--
-- This module contains builtins for fetching and IO operations:
-- fetchurl, exec.
module Nix.Builtins.Fetch
  ( -- * Fetch operations
    fetchurlNix
    -- * IO operations
  , execNix
    -- * Helpers (exported for internal use)
  , makeFixedOutputPathLocal
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Crypto.Hash                   as Hash
import qualified Nix.Core.AttrSet              as A
import qualified Data.Text                     as Text
import qualified Data.Vector                   as V
import           Nix.Builtins.Internal
import           Nix.Config.Singleton           ( HasProvCfg )
import           Nix.Context                    ( CtxCfg )
import           Nix.Convert
import           Nix.Core.List                  ( NixList )
import qualified Nix.Core.List                 as L
import           Nix.Effects
import           Nix.Exec
import           Nix.Types.VarName.Static       ( sUrls, sUrl, sHash, sSha256, sName, sExecutable )
import           Nix.Frames
import           Nix.Options
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Monad
import           Data.Dependent.Sum             ( DSum((:=>)) )
import qualified System.Nix.Hash               as StoreHash
import qualified System.Nix.Store.ReadOnly     as StoreRO
import qualified System.Nix.StorePath          as Store
import           System.Nix.FileContentAddress  ( FileIngestionMethod(..) )
import           System.Nix.ContentAddress      ( ContentAddressMethod(..) )


-- * Fetch operations

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
      let mUrlsVal = A.lookup sUrls s <|> A.lookup sUrl s
      urlsVal <- case mUrlsVal of
        Nothing -> throwError $ ErrorCall "builtins.fetchurl: missing url(s)"
        Just v -> pure v
      urls <- extractUrls =<< demand urlsVal
      mHashVal <- traverse (fromValue <=< demand) (A.lookup sHash s)
      mShaVal <- traverse (fromValue <=< demand) (A.lookup sSha256 s)
      mNameVal <- traverse (fromValue <=< demand) (A.lookup sName s)
      mExecVal <- traverse (fromValue <=< demand) (A.lookup sExecutable s)
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
    let mExpected :: Maybe StorePath
        mExpected = case mDigest of
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
    NVList vs -> L.toList <$> traverse (extractUrl <=< demand) vs
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


-- * IO operations

execNix
  :: forall e t f m . (MonadNix e t f m, HasProvCfg (CtxCfg e)) => NValue t f m -> m (NValue t f m)
execNix xs = do
  -- 2018-11-19: NOTE: Still need to do something with the context here
  -- See prim_exec in nix/src/libexpr/primops.cc
  -- Requires the implementation of EvalState::realiseContext
  v <- fromValue @(NixList (NValue t f m)) xs
  strs <- traverse (coerceStringlikeToNixString DontCopyToStore) v
  exec $ V.fromList $ ignoreContext <$> L.toList strs
