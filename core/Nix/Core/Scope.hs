{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Scope management using abstract AttrSet from Backpack signature.
--
-- This module provides the same functionality as Nix.Scope but uses
-- the abstract AttrSet type, allowing compile-time selection of
-- the underlying data structure.
module Nix.Core.Scope
  ( -- * Scope type
    Scope(..)
  , scopeLookup
  , scopeLookupWithDepth
    -- * Scopes container
  , Scopes(..)
  , emptyScopes
    -- * Scoped typeclass
  , Scoped(..)
    -- * Scope operations
  , pushScope
  , pushWeakScope
  , withScopes
    -- * Reader-based implementations
  , askScopesReader
  , clearScopesReader
  , pushScopesReader
  , setScopesReader
  , lookupVarReader
    -- * Profiling support
  , ScopeLookupInfo(..)
  , lookupVarReaderWithInfo
  ) where

import Relude hiding (empty, fromList, toList, null)
import Text.Show (showsPrec, showString)
import Nix.AttrSet.Sig as A
import Nix.Types.VarName
import Nix.Core.Utils (Has(..))
import Lens.Family2
import qualified GHC.Clock

-- | A single scope level, wrapping an AttrSet.
type Scope :: Type -> Type
newtype Scope a = Scope { unScope :: AttrSet a }
  deriving stock (Generic)
  deriving newtype
    ( Eq, NFData, Hashable
    , Semigroup, Monoid
    , Functor, Foldable
    )

instance Traversable Scope where
  traverse f (Scope m) = Scope <$> A.traverseWithKey (\_ v -> f v) m

instance Show (Scope a) where
  showsPrec _ (Scope m) = showsPrec 0 (A.keys m)

instance One (Scope a) where
  type OneItem (Scope a) = (VarName, a)
  one (k, v) = Scope (A.singleton k v)

-- | Look up a variable in a stack of scopes.
scopeLookup :: VarName -> [Scope a] -> Maybe a
scopeLookup key = foldr fun Nothing
 where
  fun :: Scope a -> Maybe a -> Maybe a
  fun (Scope m) rest = A.lookup key m <|> rest

-- | Like scopeLookup but also returns (total depth, scopes searched before finding).
-- If not found, scopes searched = total depth.
scopeLookupWithDepth :: VarName -> [Scope a] -> (Maybe a, Int, Int)
scopeLookupWithDepth key = go 0
 where
  go !depth [] = (Nothing, depth, depth)
  go !depth (Scope m : rest) =
    case A.lookup key m of
      Just v  -> (Just v, depth + 1 + length rest, depth + 1)
      Nothing -> go (depth + 1) rest

-- | Container for lexical and dynamic scopes.
data Scopes m a =
  Scopes
    { lexicalScopes :: ![Scope a]
    , dynamicScopes :: ![m (Scope a)]
    }

instance Show (Scopes m a) where
  showsPrec _ (Scopes m a) =
    showString "Scopes: " . showsPrec 0 m . showString ", and " .
    showsPrec 0 (length a) . showString " with-scopes"

instance Semigroup (Scopes m a) where
  Scopes ls lw <> Scopes rs rw = Scopes (ls <> rs) (lw <> rw)

instance Monoid (Scopes m a) where
  mempty = emptyScopes

-- | Empty scopes.
emptyScopes :: Scopes m a
emptyScopes = Scopes mempty mempty

-- | Typeclass for monads that carry scope information.
class Scoped a m | m -> a where
  askScopes     :: m (Scopes m a)
  clearScopes   :: m r -> m r
  pushScopes    :: Scopes m a -> m r -> m r
  setScopes     :: Scopes m a -> m r -> m r
  lookupVar     :: VarName -> m (Maybe a)

  default askScopes
    :: (MonadReader e m, Has e (Scopes m a))
    => m (Scopes m a)
  askScopes = askScopesReader

  default clearScopes
    :: (MonadReader e m, Has e (Scopes m a))
    => m r -> m r
  clearScopes = clearScopesReader @m @a

  default pushScopes
    :: (MonadReader e m, Has e (Scopes m a))
    => Scopes m a -> m r -> m r
  pushScopes = pushScopesReader

  default setScopes
    :: (MonadReader e m, Has e (Scopes m a))
    => Scopes m a -> m r -> m r
  setScopes = setScopesReader

  default lookupVar
    :: (MonadReader e m, Has e (Scopes m a))
    => VarName -> m (Maybe a)
  lookupVar = lookupVarReader

-- | Reader-based askScopes implementation.
askScopesReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m (Scopes m a)
askScopesReader = asks $ view hasLens

-- | Reader-based clearScopes implementation.
clearScopesReader
  :: forall m a e r
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m r
  -> m r
clearScopesReader = local $ set hasLens $ emptyScopes @m @a

-- | Reader-based setScopes implementation.
setScopesReader
  :: forall m a e r
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => Scopes m a
  -> m r
  -> m r
setScopesReader scopes = local $ set hasLens scopes

-- | Push a single scope.
pushScope
  :: Scoped a m
  => Scope a
  -> m r
  -> m r
pushScope ~scope = pushScopes $ Scopes (one scope) mempty

-- | Push a weak (dynamic) scope.
pushWeakScope
  :: ( Functor m
     , Scoped a m
     )
  => m (Scope a)
  -> m r
  -> m r
pushWeakScope scope = pushScopes $ Scopes mempty $ one scope

-- | Reader-based pushScopes implementation.
pushScopesReader
  :: ( MonadReader e m
     , Has e (Scopes m a)
     )
  => Scopes m a
  -> m r
  -> m r
pushScopesReader s = local $ over hasLens (s <>)

-- | Reader-based lookupVar implementation.
lookupVarReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => VarName
  -> m (Maybe a)
lookupVarReader k = do
  mres <- asks $ scopeLookup k . lexicalScopes @m . view hasLens

  case mres of
    Just res -> pure $ pure res
    Nothing -> do
      ws <- asks $ dynamicScopes . view hasLens

      foldr
        (\ weakscope rest -> do
            mres' <- A.lookup k . unScope <$> weakscope
            case mres' of
              Just res -> pure $ pure res
              Nothing -> rest
        )
        (pure Nothing)
        ws

-- | Set scopes for a computation.
withScopes
  :: Scoped a m
  => Scopes m a
  -> m r
  -> m r
withScopes = setScopes

-- | Scope lookup result for profiling.
data ScopeLookupInfo
  = LexicalHit !Int !Int   -- ^ Found in lexical scope: (depth, scopes searched)
  | DynamicHit !Int !Int   -- ^ Found in dynamic scope: (depth, scopes searched)
  | LookupMiss !Int        -- ^ Not found: depth
  deriving (Show, Eq)

-- | Instrumented version of lookupVarReader that returns lookup info for profiling.
lookupVarReaderWithInfo
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    , MonadIO m
    )
  => VarName
  -> m (Maybe a, ScopeLookupInfo, Word64)
lookupVarReaderWithInfo k = do
  start <- liftIO getMonotonicTimeNSec
  lexScopes <- asks $ lexicalScopes @m . view hasLens
  let (mres, lexDepth, searched) = scopeLookupWithDepth k lexScopes

  result <- case mres of
    Just v -> pure (Just v, LexicalHit lexDepth searched)
    Nothing -> do
      ws <- asks $ dynamicScopes . view hasLens
      let totalDepth = lexDepth + length ws
      searchDynamic lexDepth 0 totalDepth ws
  end <- liftIO getMonotonicTimeNSec
  let (val, info) = result
  pure (val, info, end - start)
 where
  searchDynamic _lexCount _dynSearched totalDepth [] =
    pure (Nothing, LookupMiss totalDepth)
  searchDynamic lexCount dynSearched totalDepth (weakscope : rest) = do
    mres' <- A.lookup k . unScope <$> weakscope
    case mres' of
      Just v  -> pure (Just v, DynamicHit totalDepth (lexCount + dynSearched + 1))
      Nothing -> searchDynamic lexCount (dynSearched + 1) totalDepth rest

-- | Get monotonic time in nanoseconds.
-- Re-exported from GHC.Clock.
getMonotonicTimeNSec :: IO Word64
getMonotonicTimeNSec = GHC.Clock.getMonotonicTimeNSec
