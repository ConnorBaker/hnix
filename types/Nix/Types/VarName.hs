{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | VarName type - interned variable names for efficient comparison.
--
-- Variable names are interned using the @intern@ package for O(1) comparison
-- and reduced memory usage. The intern table never garbage collects entries,
-- ensuring stable IDs for the lifetime of the program.
module Nix.Types.VarName
  ( VarName(..)
  , mkVarName
  , varNameText
  ) where

import           Relude
import           Data.Interned.Text             (InternedText)
import           Data.Interned                  (intern, unintern)
import           Codec.Serialise                (Serialise)
import qualified Codec.Serialise               as Serialise
import qualified Data.Binary                   as Binary
import           Data.Binary                    (Binary)
import           Data.Data
import           Data.Aeson
import           Data.Aeson.Types               (toJSONKeyText)
import qualified Text.Show
import qualified Text.Read
import           Text.Read                      (parens, lexP)
import qualified Language.Haskell.TH.Syntax    as TH

-- | Variable name type, backed by an interned Text.
--
-- Uses the @intern@ package for hash-consing, enabling O(1) equality
-- comparison via pointer equality of the interned representation.
--
-- Note: Eq and Hashable use the interned ID for O(1) operations,
-- but Ord uses lexicographic text comparison for deterministic ordering.
newtype VarName = VarName { getVarNameInterned :: InternedText }
  deriving stock (Generic)
  deriving newtype (Eq, Hashable)

-- | Ord compares lexicographically by text content for deterministic ordering.
-- This is important for Nix semantics where attrNames returns sorted names.
instance Ord VarName where
  compare v1 v2 = compare (varNameText v1) (varNameText v2)
  {-# INLINE compare #-}

-- | NFData for VarName - the InternedText is already strict.
instance NFData VarName where
  rnf (VarName !_) = ()
  {-# INLINE rnf #-}

-- | Create a VarName from Text by interning it.
-- O(1) amortized for repeated lookups.
mkVarName :: Text -> VarName
mkVarName = VarName . intern
{-# INLINABLE mkVarName #-}

-- | Extract the Text from a VarName.
-- O(1) operation.
varNameText :: VarName -> Text
varNameText = unintern . getVarNameInterned
{-# INLINABLE varNameText #-}

instance IsString VarName where
  fromString = mkVarName . fromString

instance ToString VarName where
  toString = toString . varNameText

instance Show VarName where
  show v = "VarName " <> show (varNameText v)

instance Read VarName where
  readPrec = parens $ Text.Read.prec 10 $ do
    Text.Read.Ident "VarName" <- lexP
    t <- Text.Read.readPrec
    pure (mkVarName t)

-- Custom Serialise instance: serialize as Text
instance Serialise VarName where
  encode = Serialise.encode . varNameText
  decode = mkVarName <$> Serialise.decode

-- Custom Binary instance: serialize as Text
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

-- Data instance needs manual implementation since InternedText doesn't have Data
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
