{-# LANGUAGE NoStrict #-}

-- | List built-in functions for the compiled Nix runtime.
--
-- This module implements list manipulation built-ins available in Nix.
-- Each builtin is a Haskell function of type @NixValue -> NixValue@ (for
-- single-argument builtins) or curried for multi-argument ones.
--
-- Many builtins are partial applications that return VBuiltin for currying.
-- For example, @builtins.map f@ returns a VBuiltin that, when applied to
-- a list, returns the mapped list.
module Nix.Compile.Builtins.List
  ( builtinLength
  , builtinHead
  , builtinTail
  , builtinElemAt
  , builtinElem
  , builtinFilter
  , builtinMap
  , builtinFoldl
  , builtinConcatLists
  , builtinGenList
  , builtinSort
  , builtinAll
  , builtinAny
  , builtinPartition
  , builtinGroupBy
  , builtinConcatMap
  , builtinListToAttrs
  , builtinReverse
  , builtinGenAttrs
  ) where

import Relude hiding (head, tail)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.List as List
import Nix.Types.VarName (VarName, mkVarName, varNameText)
import Nix.Compile.Value
import Nix.Compile.Primops

-- | Return the length of a list.
builtinLength :: NixValue -> NixValue
builtinLength v = VInt $ fromIntegral $ V.length $ expectList v
{-# INLINE builtinLength #-}

-- | Return the first element of a list.
builtinHead :: NixValue -> NixValue
builtinHead v =
  let lst = expectList v
  in if V.null lst
     then throwNixError $ ThrownError "builtins.head called on empty list"
     else V.head lst
{-# INLINE builtinHead #-}

-- | Return all elements after the first.
builtinTail :: NixValue -> NixValue
builtinTail v =
  let lst = expectList v
  in if V.null lst
     then throwNixError $ ThrownError "builtins.tail called on empty list"
     else VList $ V.tail lst
{-# INLINE builtinTail #-}

-- | Get element at index. builtins.elemAt list index
builtinElemAt :: NixValue
builtinElemAt = VBuiltin "elemAt" $ \v1 ->
  let lst = expectList v1
  in VBuiltin "elemAt index" $ \v2 ->
    let idx = fromIntegral $ expectInt v2
    in if idx < 0 || idx >= V.length lst
       then throwNixError $ ThrownError $ "list index " <> show idx <> " out of bounds"
       else lst V.! idx
{-# NOINLINE builtinElemAt #-}

-- | Check if element is in list. builtins.elem x list
builtinElem :: NixValue
builtinElem = VBuiltin "elem" $ \needle ->
  VBuiltin "elem haystack" $ \v ->
    let lst = expectList v
    in VBool $ V.any (nixEqBool needle) lst
{-# NOINLINE builtinElem #-}

-- | Filter a list. builtins.filter pred list
builtinFilter :: NixValue
builtinFilter = VBuiltin "filter" $ \pred ->
  VBuiltin "filter list" $ \v ->
    let lst = expectList v
        f = expectFunction pred
    in VList $ V.filter (\x -> expectBool (f x)) lst
{-# NOINLINE builtinFilter #-}

-- | Map over a list. builtins.map f list
builtinMap :: NixValue
builtinMap = VBuiltin "map" $ \fn ->
  VBuiltin "map list" $ \v ->
    let lst = expectList v
        f = expectFunction fn
    in VList $ V.map f lst
{-# NOINLINE builtinMap #-}

-- | Left fold. builtins.foldl' op init list
builtinFoldl :: NixValue
builtinFoldl = VBuiltin "foldl'" $ \op ->
  VBuiltin "foldl' init" $ \initVal ->
    VBuiltin "foldl' list" $ \v ->
      let lst = expectList v
          f = expectFunction op
      in V.foldl' (\acc x -> expectFunction (f acc) x) initVal lst
{-# NOINLINE builtinFoldl #-}

-- | Concatenate a list of lists.
builtinConcatLists :: NixValue -> NixValue
builtinConcatLists v =
  let lsts = expectList v
  in VList $ V.concat $ V.toList $ V.map expectList lsts
{-# INLINE builtinConcatLists #-}

-- | Generate a list. builtins.genList generator length
builtinGenList :: NixValue
builtinGenList = VBuiltin "genList" $ \gen ->
  VBuiltin "genList length" $ \v ->
    let n = expectInt v
        f = expectFunction gen
    in if n < 0
       then throwNixError $ ThrownError "builtins.genList: negative length"
       else VList $ V.generate (fromIntegral n) (f . VInt . fromIntegral)
{-# NOINLINE builtinGenList #-}

-- | Sort a list. builtins.sort comparator list
builtinSort :: NixValue
builtinSort = VBuiltin "sort" $ \cmp ->
  VBuiltin "sort list" $ \v ->
    let lst = V.toList $ expectList v
        f = expectFunction cmp
        comparator a b
          | expectBool (expectFunction (f a) b) = LT
          | expectBool (expectFunction (f b) a) = GT
          | otherwise = EQ
    in VList $ V.fromList $ List.sortBy comparator lst
{-# NOINLINE builtinSort #-}

-- | Check if all elements satisfy predicate.
builtinAll :: NixValue
builtinAll = VBuiltin "all" $ \pred ->
  VBuiltin "all list" $ \v ->
    let lst = expectList v
        f = expectFunction pred
    in VBool $ V.all (expectBool . f) lst
{-# NOINLINE builtinAll #-}

-- | Check if any element satisfies predicate.
builtinAny :: NixValue
builtinAny = VBuiltin "any" $ \pred ->
  VBuiltin "any list" $ \v ->
    let lst = expectList v
        f = expectFunction pred
    in VBool $ V.any (expectBool . f) lst
{-# NOINLINE builtinAny #-}

-- | Partition a list into matching and non-matching elements.
builtinPartition :: NixValue
builtinPartition = VBuiltin "partition" $ \pred ->
  VBuiltin "partition list" $ \v ->
    let lst = expectList v
        f = expectFunction pred
        (yes, no) = V.partition (expectBool . f) lst
    in VAttrs $ attrsFromList
        [ ("right", VList yes)
        , ("wrong", VList no)
        ]
{-# NOINLINE builtinPartition #-}

-- | Group elements by a key function.
builtinGroupBy :: NixValue
builtinGroupBy = VBuiltin "groupBy" $ \keyFn ->
  VBuiltin "groupBy list" $ \v ->
    let lst = V.toList $ expectList v
        f = expectFunction keyFn
        -- Get key for each element
        keyed = [(expectString (f x), x) | x <- lst]
        -- Group by key text
        grouped = List.groupBy (\(k1, _) (k2, _) -> fst k1 == fst k2) $
                  List.sortOn (fst . fst) keyed
        -- Build result attribute set
        pairs = [(mkVarName k, VList $ V.fromList $ map snd grp)
                | grp <- grouped, let (k, _) = fst $ List.head grp]
    in VAttrs $ attrsFromList pairs
{-# NOINLINE builtinGroupBy #-}

-- | Map and concatenate. builtins.concatMap f list
builtinConcatMap :: NixValue
builtinConcatMap = VBuiltin "concatMap" $ \fn ->
  VBuiltin "concatMap list" $ \v ->
    let lst = expectList v
        f = expectFunction fn
    in VList $ V.concat $ V.toList $ V.map (expectList . f) lst
{-# NOINLINE builtinConcatMap #-}

-- | Convert a list of {name, value} attrs to an attrset.
-- IMPORTANT: On duplicate keys, the FIRST occurrence wins (Nix semantics).
builtinListToAttrs :: NixValue -> NixValue
builtinListToAttrs v =
  let lst = V.toList $ expectList v
      extractPair attrs =
        let as = expectAttrs attrs
        in ( mkVarName $ fst $ expectString $ nixSelect as (mkVarName "name")
           , nixSelect as (mkVarName "value")
           )
      pairs = map extractPair lst
      -- foldl' to keep FIRST occurrence: process left-to-right, skip if key exists
      finalAttrs = foldl' (\acc (k, val) ->
        case lookupAttr k acc of
          Just _ -> acc  -- Key already exists, keep first occurrence
          Nothing -> insertAttr k val acc
        ) emptyAttrs pairs
  in VAttrs finalAttrs
{-# INLINE builtinListToAttrs #-}

-- | Reverse a list.
builtinReverse :: NixValue -> NixValue
builtinReverse v = VList $ V.reverse $ expectList v
{-# INLINE builtinReverse #-}

-- | Generate an attribute set from a list of names.
-- builtins.genAttrs names f returns { (n) = f n; } for each n in names.
builtinGenAttrs :: NixValue
builtinGenAttrs = VBuiltin "genAttrs" $ \namesVal ->
  VBuiltin "genAttrs f" $ \f ->
    let names = V.toList $ expectList namesVal
        fn = expectFunction f
        pairs = [ (mkVarName (fst $ expectString n), fn n)
                | n <- names
                ]
    in VAttrs $ attrsFromList pairs
{-# NOINLINE builtinGenAttrs #-}
