{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Atomic values in Nix - values that evaluate to themselves.
--
-- Atoms are unpacked where beneficial for performance.
-- Int64 and Double are primitive types that benefit from UNPACK.
module Nix.Types.Atom
  ( NAtom(..)
  , atomText
  -- Re-export Int64 for convenience
  , Int64
  -- Checked arithmetic operations (throw on overflow)
  , checkedAdd
  , checkedSub
  , checkedMul
  , checkedDiv
  , checkedNeg
  ) where

import           Relude
import           Codec.Serialise                (Serialise)
import           Data.Data                      (Data)
import           Data.Fixed                     (mod')
import           Numeric                        (showEFloat, showFFloat)
import           Data.List                      (dropWhileEnd)
import           Data.Binary                    (Binary)
import           Data.Aeson.Types               (FromJSON, ToJSON)

-- | Atoms are values that evaluate to themselves.
-- In other words - this is a constructors that are literals in Nix.
-- This means that they appear in both the parsed AST (in the form of literals)
-- and the evaluated form as themselves.
data NAtom
  -- | An URI like @https://example.com@.
  = NURI !Text
  -- | An integer. The Nix implementation uses checked 64-bit integers
  -- that throw an error on overflow.
  | NInt {-# UNPACK #-} !Int64
  -- | A floating point number
  | NFloat {-# UNPACK #-} !Double
  -- | Booleans. @false@ or @true@.
  | NBool !Bool
  -- | Null values. There's only one of this variant: @null@.
  | NNull
  deriving
    ( Eq
    , Ord
    , Generic
    , Data
    , Show
    , Read
    , NFData
    , Hashable
    )

instance Serialise NAtom
instance Binary NAtom
instance ToJSON NAtom
instance FromJSON NAtom

-- | Translate an atom into its Nix representation.
atomText :: NAtom -> Text
atomText (NURI   t) = t
atomText (NInt   i) = show i
atomText (NFloat f) = showNixFloat f
 where
  -- | Format a Double to match Nix's output format:
  -- 1. Whole numbers < 1e6 are shown as integers (no decimal point)
  -- 2. |x| >= 1e6 or (|x| < 1e-4 and x /= 0) uses scientific notation
  -- 3. Otherwise uses decimal notation with 6 significant figures
  showNixFloat :: Double -> Text
  showNixFloat x
    -- Whole numbers less than 1e6 are displayed as integers
    | x `mod'` 1 == 0 && abs x < 1e6 = show (truncate x :: Integer)
    -- Large values (>= 1e6) or very small values (< 1e-4) use scientific notation
    | abs x >= 1e6 || (abs x < 1e-4 && x /= 0) = fromString $ formatScientific x
    -- Normal range uses decimal notation
    | otherwise = fromString $ formatDecimal x

  formatScientific :: Double -> String
  formatScientific x =
    let raw = showEFloat (Just 5) x ""
    in cleanupScientific raw

  formatDecimal :: Double -> String
  formatDecimal x =
    let absX = abs x
        digitsBeforeDecimal = max 1 $ floor (logBase 10 absX) + 1
        decimalPlaces = max 0 $ 6 - digitsBeforeDecimal
        raw = showFFloat (Just decimalPlaces) x ""
    in cleanupDecimal raw

  cleanupScientific :: String -> String
  cleanupScientific s =
    case break (== 'e') s of
      (mantissa, expo) ->
        cleanupMantissa mantissa <> formatExponent expo

  cleanupDecimal :: String -> String
  cleanupDecimal = cleanupMantissa

  cleanupMantissa :: String -> String
  cleanupMantissa s =
    case break (== '.') s of
      (whole, '.' : frac) ->
        let trimmed = dropWhileEnd (== '0') frac
        in if null trimmed
           then whole
           else whole <> "." <> trimmed
      _ -> s

  formatExponent :: String -> String
  formatExponent "" = ""
  formatExponent ('e' : rest) =
    case rest of
      '-' : num -> "e-" <> padExponent num
      '+' : num -> "e+" <> padExponent num
      num       -> "e+" <> padExponent num
  formatExponent s = s

  padExponent :: String -> String
  padExponent [c] = '0' : [c]
  padExponent s   = s
atomText (NBool  b) = if b then "true" else "false"
atomText NNull      = "null"

-- | Checked arithmetic operations for Int64 that match Nix's behavior.
-- These throw an error on overflow rather than wrapping.

-- | Checked addition. Throws on overflow.
checkedAdd :: Int64 -> Int64 -> Either String Int64
checkedAdd x y
  | y > 0 && x > maxBound - y = Left $ "integer overflow in adding " <> show x <> " + " <> show y
  | y < 0 && x < minBound - y = Left $ "integer overflow in adding " <> show x <> " + " <> show y
  | otherwise = Right (x + y)

-- | Checked subtraction. Throws on overflow.
checkedSub :: Int64 -> Int64 -> Either String Int64
checkedSub x y
  | y < 0 && x > maxBound + y = Left $ "integer overflow in subtracting " <> show x <> " - " <> show y
  | y > 0 && x < minBound + y = Left $ "integer overflow in subtracting " <> show x <> " - " <> show y
  | otherwise = Right (x - y)

-- | Checked multiplication. Throws on overflow.
checkedMul :: Int64 -> Int64 -> Either String Int64
checkedMul x y
  | x == 0 || y == 0 = Right 0
  | x == -1 && y == minBound = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | y == -1 && x == minBound = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x > 0 && y > 0 && x > maxBound `div` y = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x > 0 && y < 0 && y < minBound `div` x = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x < 0 && y > 0 && x < minBound `div` y = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x < 0 && y < 0 && x < maxBound `div` y = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | otherwise = Right (x * y)

-- | Checked integer division. Uses truncation toward zero (like C's /).
-- Throws on division by zero.
checkedDiv :: Int64 -> Int64 -> Either String Int64
checkedDiv _ 0 = Left "division by zero"
checkedDiv x y = Right (x `quot` y)

-- | Checked negation. Throws on overflow (negating minBound).
checkedNeg :: Int64 -> Either String Int64
checkedNeg x
  | x == minBound = Left $ "integer overflow in negating " <> show x
  | otherwise = Right (negate x)
