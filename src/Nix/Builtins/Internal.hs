{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Shared infrastructure for Nix builtins.
--
-- This module contains types, classes, and helper functions used across
-- all builtin categories.
module Nix.Builtins.Internal
  ( -- * Builtin registration types
    Prim(..)
  , BuiltinType(..)
  , Builtin(..)
  , ToBuiltin(..)
    -- * Value wrapper for genericClosure
  , WValue(..)
    -- * Patterns
  , pattern NVBool
    -- * NIX_PATH handling
  , NixPathEntryType(..)
  , uriAwareSplit
  , foldNixPath
    -- * Helper functions
  , attrsetGet
  , absolutePathFromValue
  , hasKind
    -- * Version handling
  , VersionComponent(..)
  , splitVersion
  , compareVersions
  , splitDrvName
    -- * Regex helpers
  , splitMatches
  , thunkStr
    -- * Builtin registration helpers
  , arity0
  , arity1
  , arity2
  , mkBuiltin
  , hAdd
  , add0
  , add
  , add'
  , add2
  , add3
  , builtin
  , builtin2
  , builtin3
  ) where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Align                     ( alignWith )
import qualified Data.ByteString               as B
import           Data.Char                      ( isDigit )
import           Data.Foldable                  ( foldrM )
import qualified Data.Text                     as Text
import           Data.Text.Read                 ( decimal )
import           Data.These                     ( fromThese, These )
import qualified Text.Show
import           Nix.Atoms
import           Nix.Convert
import qualified Nix.Core.AttrSet              as A
import qualified Nix.Core.List                 as L
import           Nix.Effects
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Frames
import           Nix.String
import           Nix.Scope
import           Nix.Value
import           Nix.Value.Monad

-- * Internal types

-- | Wrapper for monadic values in builtins.
newtype Prim m a = Prim (m a)

-- | Whether a builtin is at top-level or in the builtins set.
data BuiltinType = Normal | TopLevel

-- | A builtin function with its registration info.
data Builtin v =
  Builtin
    { _kind   :: BuiltinType
    , mapping :: (VarName, v)
    }

-- ** @class ToBuiltin@ and its instances

-- | Types that support conversion to nix in a particular monad.
class ToBuiltin t f m a | a -> m where
  toBuiltin :: Text -> a -> m (NValue t f m)

instance
  ( MonadNix e t f m
  , ToValue a m (NValue t f m)
  )
  => ToBuiltin t f m (Prim m a) where
  toBuiltin _ p = toValue @a @m =<< coerce p

instance
  ( MonadNix e t f m
  , FromValue a m (Deeper (NValue t f m))
  , ToBuiltin t f m b
  )
  => ToBuiltin t f m (a -> b) where
  toBuiltin name f =
    pure $ NVBuiltin (mkVarName name) $ toBuiltin name . f <=< fromValue . Deeper

-- ** @WValue@ closure wrapper to have @Ord@

-- | We wrap values solely to provide an Ord instance for genericClosure.
newtype WValue t f m = WValue (NValue t f m)

instance NVConstraint f => Eq (WValue t f m) where
  WValue (NVConstant (NFloat x)) == WValue (NVConstant (NInt y)) =
    x == fromIntegral y
  WValue (NVConstant (NInt   x)) == WValue (NVConstant (NFloat y)) =
    fromIntegral x == y
  WValue (NVConstant (NInt   x)) == WValue (NVConstant (NInt   y)) = x == y
  WValue (NVConstant (NFloat x)) == WValue (NVConstant (NFloat y)) = x == y
  WValue (NVPath     x         ) == WValue (NVPath     y         ) = x == y
  WValue (NVStr x) == WValue (NVStr y) =
    ignoreContext x == ignoreContext y
  _ == _ = False

instance NVConstraint f => Ord (WValue t f m) where
  WValue (NVConstant (NFloat x)) <= WValue (NVConstant (NInt y)) =
    x <= fromIntegral y
  WValue (NVConstant (NInt   x)) <= WValue (NVConstant (NFloat y)) =
    fromIntegral x <= y
  WValue (NVConstant (NInt   x)) <= WValue (NVConstant (NInt   y)) = x <= y
  WValue (NVConstant (NFloat x)) <= WValue (NVConstant (NFloat y)) = x <= y
  WValue (NVPath     x         ) <= WValue (NVPath     y         ) = x <= y
  WValue (NVStr x) <= WValue (NVStr y) =
    ignoreContext x <= ignoreContext y
  _ <= _ = False

-- * Patterns

pattern NVBool :: MonadNix e t f m => Bool -> NValue t f m
pattern NVBool a = NVConstant (NBool a)

-- * NIX_PATH handling

data NixPathEntryType
  = PathEntryPath
  | PathEntryURI
 deriving (Show, Eq)

-- | @NIX_PATH@ is colon-separated, but can also contain URLs, which have a colon
-- (i.e. @https://...@)
uriAwareSplit :: Text -> [(Text, NixPathEntryType)]
uriAwareSplit txt =
  case Text.break (== ':') txt of
    (e1, e2)
      | Text.null e2                              -> one (e1, PathEntryPath)
      | "://" `Text.isPrefixOf` e2      ->
        let ((suffix, _) : path) = uriAwareSplit (Text.drop 3 e2) in
        (e1 <> "://" <> suffix, PathEntryURI) : path
      | otherwise                                 -> (e1, PathEntryPath) : uriAwareSplit (Text.drop 1 e2)

foldNixPath
  :: forall e t f m r
   . MonadNix e t f m
  => r
  -> (Path -> Maybe Text -> NixPathEntryType -> r -> m r)
  -> m r
foldNixPath z f =
  do
    mres <- lookupVar "__includes"
    dirs <-
      case mres of
        Nothing -> stub
        Just v -> (fromValue . Deeper) =<< demand v
    mPath    <- getEnvVar "NIX_PATH"
    mDataDir <- getEnvVar "NIX_DATA_DIR"
    dataDir  <-
      case mDataDir of
        Nothing -> getDataDir
        Just v -> pure . coerce . toString $ v

    foldrM
      fun
      z
      $ (fromInclude . ignoreContext <$> dirs)
        <> uriAwareSplit `whenJust` mPath
        <> one (fromInclude $ "nix=" <> fromString (coerce dataDir) <> "/nix/corepkgs")
 where

  fromInclude :: Text -> (Text, NixPathEntryType)
  fromInclude x =
    (x, ) $
      if "://" `Text.isInfixOf` x
        then PathEntryURI
        else PathEntryPath

  fun :: (Text, NixPathEntryType) -> r -> m r
  fun (x, ty) rest =
    case Text.splitOn "=" x of
      [p] -> f (coerce $ toString p) mempty ty rest
      [n, p] -> f (coerce $ toString p) (pure n) ty rest
      _ -> throwError $ ErrorCall $ "Unexpected entry in NIX_PATH: " <> show x

-- * Helper functions

attrsetGet :: MonadNix e t f m => VarName -> AttrSet (NValue t f m) -> m (NValue t f m)
attrsetGet k s =
  case A.lookup k s of
    Nothing -> throwError $ ErrorCall $ toString @Text $ "Attribute '" <> varNameText k <> "' required"
    Just v -> pure v

absolutePathFromValue :: MonadNix e t f m => NValue t f m -> m Path
absolutePathFromValue =
  \case
    NVStr ns ->
      do
        let
          path = coerce . toString $ ignoreContext ns

        when (not (isAbsolute path)) $ throwError $ ErrorCall $ "string " <> show path <> " doesn't represent an absolute path"
        pure path

    NVPath path -> pure path
    v           -> throwError $ ErrorCall $ "expected a path, got " <> show v

-- | Check if a value is of a specific type. Returns interned boolean.
hasKind
  :: forall a e t f m
   . (MonadNix e t f m, FromValue a m (NValue t f m))
  => NValue t f m
  -> m (NValue t f m)
hasKind nv = do
  mv <- fromValueMay @a nv
  askInternedBool $ isJust mv

-- * Version handling

data VersionComponent
  = VersionComponentPre -- ^ The string "pre"
  | VersionComponentString !Text -- ^ A string other than "pre"
  | VersionComponentNumber !Integer -- ^ A number
  deriving (Read, Eq, Ord)

instance Text.Show.Show VersionComponent where
  show = \case
    VersionComponentPre      -> "pre"
    VersionComponentString s -> Text.Show.show s
    VersionComponentNumber n -> Text.Show.show n

splitVersion :: Text -> [VersionComponent]
splitVersion s =
  (\ (x, xs) -> if
    | isRight eDigitsPart ->
        case eDigitsPart of
          Left e -> error $ "splitVersion: did hit impossible: '" <> fromString e <> "' while parsing '" <> s <> "'."
          Right res ->
            one (VersionComponentNumber $ fst res)
            <> splitVersion (snd res)

    | x `elem` separators -> splitVersion xs

    | otherwise -> one charsPart <> splitVersion rest2
  ) `whenJust` Text.uncons s
 where
  -- | Based on https://github.com/NixOS/nix/blob/4ee4fda521137fed6af0446948b3877e0c5db803/src/libexpr/names.cc#L44
  separators :: String
  separators = ".-"

  eDigitsPart :: Either String (Integer, Text)
  eDigitsPart = decimal @Integer $ s

  (charsSpan, rest2) =
    Text.span
      (\c -> not $ isDigit c || c `elem` separators)
      s

  charsPart :: VersionComponent
  charsPart =
    case charsSpan of
      "pre" -> VersionComponentPre
      xs'   -> VersionComponentString xs'


compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 =
  fold $ (alignWith cmp `on` splitVersion) s1 s2
 where
  cmp :: These VersionComponent VersionComponent -> Ordering
  cmp = uncurry compare . join fromThese (VersionComponentString mempty)

splitDrvName :: Text -> (Text, Text)
splitDrvName s =
  both (Text.intercalate sep) (namePieces, versionPieces)
 where
  sep    = "-"
  pieces :: [Text]
  pieces = Text.splitOn sep s
  isFirstVersionPiece :: Text -> Bool
  isFirstVersionPiece p =
    case Text.uncons p of
      Nothing -> False
      Just (c, _) -> isDigit c
  -- Like 'break', but always puts the first item into the first result
  -- list
  breakAfterFirstItem :: (a -> Bool) -> [a] -> ([a], [a])
  breakAfterFirstItem f =
    handlePresence
      mempty
      (\ (h : t) -> let (a, b) = break f t in (h : a, b))
  (namePieces, versionPieces) =
    breakAfterFirstItem isFirstVersionPiece pieces

-- * Regex helpers

splitMatches
  :: forall e t f m
   . MonadNix e t f m
  => Int
  -> [[(ByteString, (Int, Int))]]
  -> ByteString
  -> [NValue t f m]
splitMatches _ [] haystack = one $ thunkStr haystack
splitMatches _ ([] : _) _ =
  fail "Fail in splitMatches: this should never happen!"
splitMatches numDropped (((_, (start, len)) : captures) : mts) haystack =
  thunkStr before : caps : splitMatches (numDropped + relStart + len)
                                        mts
                                        (B.drop len rest)
 where
  relStart       = max 0 start - numDropped
  (before, rest) = B.splitAt relStart haystack
  caps :: NValue t f m
  caps           = NVList (L.nlFromList $ f <$> captures)
  f :: (ByteString, (Int, b)) -> NValue t f m
  f (a, (s, _))  =
    if s >= 0
      then thunkStr a
      else NVNull

thunkStr :: NVConstraint f => ByteString -> NValue t f m
thunkStr s = mkNVStrWithoutContext $ decodeUtf8 s

-- * Builtin registration helpers

arity0 :: Applicative m => a -> Prim m a
arity0 = Prim . pure

arity1 :: Applicative m => (a -> b) -> (a -> Prim m b)
arity1 g = arity0 . g

arity2 :: Applicative m => (a -> b -> c) -> (a -> b -> Prim m c)
arity2 f = arity1 . f

mkBuiltin
  :: MonadNix e t f m
  => BuiltinType
  -> VarName
  -> m (NValue t f m)
  -> m (Builtin (NValue t f m))
mkBuiltin t n v = wrap t n <$> mkThunk n v
 where
  wrap :: BuiltinType -> VarName -> v -> Builtin v
  wrap t n f = Builtin t (n, f)

  mkThunk :: MonadNix e t f m => VarName -> m (NValue t f m) -> m (NValue t f m)
  mkThunk n = defer . withFrame Info (ErrorCall $ "While calling builtin " <> toString n <> "\n")

hAdd
  :: MonadNix e t f m
  => ( VarName
    -> fun
    -> m (NValue t f m)
    )
  -> BuiltinType
  -> VarName
  -> fun
  -> m (Builtin (NValue t f m))
hAdd f t n v = mkBuiltin t n $ f n v

add0
  :: MonadNix e t f m
  => BuiltinType
  -> VarName
  -> m (NValue t f m)
  -> m (Builtin (NValue t f m))
add0 = hAdd (\ _ x -> x)

add
  :: MonadNix e t f m
  => BuiltinType
  -> VarName
  -> ( NValue t f m
    -> m (NValue t f m)
    )
  -> m (Builtin (NValue t f m))
add = hAdd builtin

add'
  :: (MonadNix e t f m, ToBuiltin t f m a)
  => BuiltinType
  -> VarName
  -> a
  -> m (Builtin (NValue t f m))
add' = hAdd (toBuiltin . varNameText)

add2
  :: MonadNix e t f m
  => BuiltinType
  -> VarName
  -> ( NValue t f m
    -> NValue t f m
    -> m (NValue t f m)
    )
  -> m (Builtin (NValue t f m))
add2 = hAdd builtin2

add3
  :: MonadNix e t f m
  => BuiltinType
  -> VarName
  -> ( NValue t f m
    -> NValue t f m
    -> NValue t f m
    -> m (NValue t f m)
    )
  -> m (Builtin (NValue t f m))
add3 = hAdd builtin3
