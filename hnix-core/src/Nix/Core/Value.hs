{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

-- | Core Nix value types using Backpack signatures.
--
-- This module provides the essential value types from Nix.Value but uses
-- abstract NixList and AttrSet types from Backpack signatures, enabling
-- compile-time selection of implementations with guaranteed monomorphization.
module Nix.Core.Value
  ( -- * Base functor
    NValueF(..)
    -- * Value type classification
  , ValueType(..)
  , TStringContext(..)
  , valueType
  , describeValue
    -- * Operations
  , hoistNValueF
  , liftNValueF
  , unliftNValueF
  , lmapNValueF
  , sequenceNValueF
  , bindNValueF
    -- * Re-exports
  , NixList
  , AttrSet
  ) where

import Relude hiding (empty, fromList, toList, null)
import Text.Show (showsPrec, showString, showParen)
import Data.Functor.Classes (Eq1(liftEq))
import Control.DeepSeq ()
import Control.Monad.Trans.Class ()

import Nix.AttrSet.Sig as A
import Nix.List.Sig as L
import Nix.Types.VarName
import Nix.Types.Path
import Nix.Types.Atom

-- | An NValueF p m r represents all the possible types of Nix values.
--
--   This is the base functor to form the Free monad of nix expressions.
--   The parameter `r` represents Nix values in their final form (NValue).
--   The parameter `p` represents exactly the same type, but is kept separate
--   or it would prevent NValueF from being a proper functor.
--   It is intended to be hard-coded to the same final type as r.
--   `m` is the monad in which evaluations will run.
--
-- Note: Uses abstract NixList from Backpack signature instead of Vector.
data NValueF p m r
    = NVConstantF NAtom
      -- ^ A constant value (int, float, bool, null)
    | NVStrF Text
      -- ^ A string value (simplified without NixString for core)
    | NVPathF Path
      -- ^ A path value
    | NVListF (NixList r)
      -- ^ Nix lists using abstract NixList type.
      -- Elements remain lazy to preserve Nix semantics.
      -- Implementation (Vector, Seq, etc.) selected at link time.
    | NVSetF (AttrSet r)
      -- ^ Attribute set using abstract AttrSet type.
    | NVClosureF (p -> m r)
      -- ^ A closure (function).
    | NVBuiltinF VarName (p -> m r)
      -- ^ A builtin function.
  deriving (Generic, Functor)


-- ** Eq

instance Eq r => Eq (NValueF p m r) where
  (==) (NVConstantF x) (NVConstantF y) = x == y
  (==) (NVStrF      x) (NVStrF      y) = x == y
  (==) (NVPathF     x) (NVPathF     y) = x == y
  (==) (NVListF     x) (NVListF     y) = x == y
  (==) (NVSetF      x) (NVSetF      y) = x == y
  (==) _               _               = False


-- ** Eq1
-- Note: Using element-wise comparison via Foldable since NixList/AttrSet
-- don't have Eq1 instances in the signature.

instance Eq1 (NValueF p m) where
  liftEq _  (NVConstantF x) (NVConstantF y) = x == y
  liftEq _  (NVStrF      x) (NVStrF      y) = x == y
  liftEq _  (NVPathF     x) (NVPathF     y) = x == y
  liftEq eq (NVListF     x) (NVListF     y) =
    L.nlLength x == L.nlLength y && and (zipWith eq (L.nlToList x) (L.nlToList y))
  liftEq eq (NVSetF      x) (NVSetF      y) =
    A.size x == A.size y && and (zipWith pairEq (A.toList x) (A.toList y))
    where pairEq (k1, v1) (k2, v2) = k1 == k2 && eq v1 v2
  liftEq _  _               _               = False


-- ** Show

instance Show r => Show (NValueF p m r) where
  showsPrec d =
    \case
      (NVConstantF atom     ) -> showsCon1 "NVConstant" atom
      (NVStrF      s        ) -> showsCon1 "NVStr"      s
      (NVListF     lst      ) -> showsCon1 "NVList"     (L.nlToList lst)
      (NVSetF      attrs    ) -> showsCon1 "NVSet"      attrs
      (NVClosureF  _        ) -> showString "NVClosure"
      (NVPathF     p        ) -> showsCon1 "NVPath"     p
      (NVBuiltinF  name   _ ) -> showsCon1 "NVBuiltin"  name
   where
    showsCon1 :: Show a => String -> a -> String -> String
    showsCon1 con a =
      showParen (d > 10) $ showString (con <> " ") . showsPrec 11 a


-- ** Foldable

-- | Folds what the value is known to contain at time of fold.
instance Foldable (NValueF p m) where
  foldMap f = \case
    NVConstantF _  -> mempty
    NVStrF      _  -> mempty
    NVPathF     _  -> mempty
    NVClosureF _   -> mempty
    NVBuiltinF _ _ -> mempty
    NVListF     l  -> foldMap f l
    NVSetF      s  -> foldMap f s


-- ** Traversable

-- | @sequence@
sequenceNValueF
  :: (Functor n, Monad m, Applicative n)
  => (forall x . n x -> m x)
  -> NValueF p m (n a)
  -> n (NValueF p m a)
sequenceNValueF transform = \case
  NVConstantF a  -> pure $ NVConstantF a
  NVStrF      s  -> pure $ NVStrF s
  NVPathF     p  -> pure $ NVPathF p
  NVListF     l  -> NVListF <$> L.nlTraverse id l
  NVSetF      s  -> NVSetF <$> A.traverseWithKey (\_ v -> v) s
  NVClosureF  g  -> pure $ NVClosureF (transform <=< g)
  NVBuiltinF s g -> pure $ NVBuiltinF s (transform <=< g)


-- ** Monad

-- | @bind@
bindNValueF
  :: (Monad m, Monad n)
  => (forall x . n x -> m x) -- ^ Transform @n@ into @m@.
  -> (a -> n b) -- ^ A Kleisli arrow.
  -> NValueF p m a -- ^ "Unfixed" value.
  -> n (NValueF p m b) -- ^ An implementation of @transform (f =<< x)@.
bindNValueF transform f = \case
  NVConstantF a  -> pure $ NVConstantF a
  NVStrF      s  -> pure $ NVStrF s
  NVPathF     p  -> pure $ NVPathF p
  NVListF     l  -> NVListF <$> L.nlTraverse f l
  NVSetF      s  -> NVSetF <$> A.traverseWithKey (\_ v -> f v) s
  NVClosureF  g  -> pure $ NVClosureF (transform . f <=< g)
  NVBuiltinF s g -> pure $ NVBuiltinF s (transform . f <=< g)


-- *** MonadTrans

-- | @lift@
liftNValueF
  :: (MonadTrans u, Monad m)
  => NValueF p m a
  -> NValueF p (u m) a
liftNValueF = hoistNValueF lift

-- **** MonadTransUnlift

-- | @unlift@
unliftNValueF
  :: (MonadTrans u, Monad m)
  => (forall x . u m x -> m x)
  -> NValueF p (u m) a
  -> NValueF p m a
unliftNValueF = hoistNValueF


-- **** Utils

-- | Back & forth hoisting in the monad stack
hoistNValueF
  :: (forall x . m x -> n x)
  -> NValueF p m a
  -> NValueF p n a
hoistNValueF lft =
  \case
    NVConstantF a  -> NVConstantF a
    NVStrF      s  -> NVStrF s
    NVPathF     p  -> NVPathF p
    NVListF     l  -> NVListF l
    NVSetF      s  -> NVSetF s
    NVBuiltinF s g -> NVBuiltinF s (lft . g)
    NVClosureF   g -> NVClosureF (lft . g)
{-# INLINABLE hoistNValueF #-}


-- ** Profunctor

-- | @lmap@
lmapNValueF :: Functor m => (b -> a) -> NValueF a m r -> NValueF b m r
lmapNValueF f = \case
  NVConstantF a  -> NVConstantF a
  NVStrF      s  -> NVStrF s
  NVPathF     p  -> NVPathF p
  NVListF     l  -> NVListF l
  NVSetF      s  -> NVSetF s
  NVClosureF g   -> NVClosureF (g . f)
  NVBuiltinF s g -> NVBuiltinF s (g . f)


-- * TStringContext

data TStringContext = NoContext | HasContext
 deriving (Show, Eq)

instance Semigroup TStringContext where
  (<>) NoContext NoContext = NoContext
  (<>) _         _         = HasContext

instance Monoid TStringContext where
  mempty = NoContext


-- * ValueType

data ValueType
  = TInt
  | TFloat
  | TBool
  | TNull
  | TString TStringContext
  | TList
  | TSet
  | TClosure
  | TPath
  | TBuiltin
 deriving (Show, Eq)


-- | Determine type of a value
valueType :: NValueF a m r -> ValueType
valueType =
  \case
    NVConstantF a ->
      case a of
        NURI   _ -> TString mempty
        NInt   _ -> TInt
        NFloat _ -> TFloat
        NBool  _ -> TBool
        NNull    -> TNull
    NVStrF _       -> TString mempty
    NVListF{}      -> TList
    NVSetF{}       -> TSet
    NVClosureF{}   -> TClosure
    NVPathF{}      -> TPath
    NVBuiltinF{}   -> TBuiltin


-- | Describe type value
describeValue :: ValueType -> Text
describeValue =
  \case
    TInt               -> "an integer"
    TFloat             -> "a float"
    TBool              -> "a boolean"
    TNull              -> "a null"
    TString NoContext  -> "a string with no context"
    TString HasContext -> "a string"
    TList              -> "a list"
    TSet               -> "an attr set"
    TClosure           -> "a function"
    TPath              -> "a path"
    TBuiltin           -> "a builtin function"
