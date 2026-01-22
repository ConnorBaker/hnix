{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | VarName type - interned variable names using GHC's FastString.
--
-- Variable names are interned using GHC's FastString infrastructure for O(1)
-- comparison via Unique comparison, and efficient memory usage via the global
-- FastString table that never garbage collects entries.
module Nix.Types.VarName
  ( VarName(..)
  , mkVarName
  , varNameText
  , varNameFS
  ) where

import           Relude
import           GHC.Data.FastString (FastString, mkFastString, unpackFS)
import           GHC.Types.Unique (Uniquable(..), getKey)
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import           Data.Data
import           Data.Aeson
import           Data.Aeson.Types (toJSONKeyText)
import qualified Text.Show
import qualified Text.Read
import           Text.Read (parens, lexP)
import qualified Language.Haskell.TH.Syntax as TH

-- | Variable name type, backed by GHC's FastString.
--
-- Uses GHC's FastString for hash-consing, enabling O(1) equality
-- comparison via Unique comparison of the interned representation.
--
-- Note: Eq and Hashable use the FastString's Unique for O(1) operations,
-- but Ord uses lexicographic text comparison for deterministic ordering.
newtype VarName = VarName { getVarNameFS :: FastString }
  deriving stock (Generic)

-- | Eq uses FastString's Unique for O(1) comparison.
instance Eq VarName where
  VarName fs1 == VarName fs2 = fs1 == fs2
  {-# INLINE (==) #-}

-- | Hashable uses the Unique's key for O(1) hashing.
instance Hashable VarName where
  hashWithSalt s (VarName fs) = hashWithSalt s (getKey (getUnique fs))
  {-# INLINE hashWithSalt #-}

-- | Ord compares lexicographically by text content for deterministic ordering.
-- This is important for Nix semantics where attrNames returns sorted names.
instance Ord VarName where
  compare v1 v2 = compare (varNameText v1) (varNameText v2)
  {-# INLINE compare #-}

-- | NFData for VarName - the FastString is already strict.
instance NFData VarName where
  rnf (VarName !_) = ()
  {-# INLINE rnf #-}

-- | Create a VarName from Text by interning it.
-- O(1) amortized for repeated lookups via FastString's global table.
mkVarName :: Text -> VarName
mkVarName = VarName . mkFastString . toString
{-# INLINABLE mkVarName #-}

-- | Extract the Text from a VarName.
-- O(n) operation - converts via String.
varNameText :: VarName -> Text
varNameText = toText . unpackFS . getVarNameFS
{-# INLINABLE varNameText #-}

-- | Get the underlying FastString for direct use with FastStringEnv.
varNameFS :: VarName -> FastString
varNameFS = getVarNameFS
{-# INLINE varNameFS #-}

instance IsString VarName where
  fromString = VarName . mkFastString

instance ToString VarName where
  toString = toString . varNameText

instance Show VarName where
  show v = "VarName " <> show (varNameText v)

instance Read VarName where
  readPrec = parens $ Text.Read.prec 10 $ do
    Text.Read.Ident "VarName" <- lexP
    t <- Text.Read.readPrec
    pure (mkVarName t)

-- Custom Serialise instance: serialize as Text for determinism
-- (Uniques are not stable across runs)
instance Serialise VarName where
  encode = Serialise.encode . varNameText
  decode = mkVarName <$> Serialise.decode

-- Custom Binary instance: serialize as Text for determinism
instance Binary VarName where
  put = Binary.put . varNameText
  get = mkVarName <$> Binary.get

-- Custom JSON instances: serialize as Text
instance ToJSON VarName where
  toJSON = toJSON . varNameText
  toEncoding = toEncoding . varNameText

instance FromJSON VarName where
  parseJSON = fmap mkVarName . parseJSON

-- Key instances for HashMap serialization
instance ToJSONKey VarName where
  toJSONKey = toJSONKeyText varNameText

instance FromJSONKey VarName where
  fromJSONKey = FromJSONKeyText mkVarName

-- Data instance needs manual implementation since FastString doesn't have Data
instance Data VarName where
  gfoldl k z v = z mkVarName `k` varNameText v
  gunfold k z _ = k (z mkVarName)
  toConstr _ = varNameConstr
  dataTypeOf _ = varNameDataType

varNameConstr :: Constr
varNameConstr = mkConstr varNameDataType "VarName" [] Data.Data.Prefix

varNameDataType :: DataType
varNameDataType = mkDataType "Nix.Types.VarName.VarName" [varNameConstr]

-- TH Lift instance: generate code that uses mkVarName
instance TH.Lift VarName where
  lift v = [| mkVarName $(TH.lift (varNameText v)) |]
  liftTyped v = [|| mkVarName $$(TH.liftTyped (varNameText v)) ||]
