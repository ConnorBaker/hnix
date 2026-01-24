{-# LANGUAGE NoStrict #-}

-- | Logical operations for the compiled Nix runtime.
--
-- These functions implement logical operators (NOT, AND, OR, implication).
-- They work with boolean values and throw TypeError on non-booleans.
--
-- Note on short-circuiting: nixAnd, nixOr, and nixImpl are implemented as
-- regular functions, so both arguments are evaluated by GHC before the
-- function is called. The compiler must generate if-then-else constructs
-- for true short-circuit behavior.
module Nix.Compile.Primops.Logical
  ( nixNot
  , nixAnd
  , nixOr
  , nixImpl
  ) where

import Relude
import Nix.Compile.Value (NixValue(..), throwNixError, NixError(..))
import Nix.Compile.Primops.Coerce (expectBool)

-- | Logical NOT.
nixNot :: NixValue -> NixValue
nixNot v = VBool (not $ expectBool v)
{-# INLINE nixNot #-}

-- | Logical AND. Short-circuits.
-- Note: This is implemented as a regular function, so both arguments are
-- evaluated by GHC before this is called. The compiler must generate
-- if-then-else for true short-circuit behavior.
nixAnd :: NixValue -> NixValue -> NixValue
nixAnd v1 v2 = VBool (expectBool v1 && expectBool v2)
{-# INLINE nixAnd #-}

-- | Logical OR. See nixAnd note about short-circuiting.
nixOr :: NixValue -> NixValue -> NixValue
nixOr v1 v2 = VBool (expectBool v1 || expectBool v2)
{-# INLINE nixOr #-}

-- | Logical implication (a -> b = !a || b). See nixAnd note.
nixImpl :: NixValue -> NixValue -> NixValue
nixImpl v1 v2 = VBool (not (expectBool v1) || expectBool v2)
{-# INLINE nixImpl #-}
