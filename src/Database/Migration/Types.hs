module Database.Migration.Types where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import GHC.Generics

data DBDiff
  = Sync
  | Diff ![T.Text]
  deriving (Show)

class RenderPredicate be p where
  renderQuery :: p -> [T.Text]
  mutatePredicate :: BP.Connection -> p -> IO p

type PredicateInfo a = Map.Map T.Text a

data CharTypeInfo = CharTypeInfo
  { prec :: !(Maybe T.Text)
  , collation :: !(Maybe T.Text)
  } deriving (Generic, Show, Eq)

instance A.FromJSON CharTypeInfo

data TimestampTypeInfo = TimestampTypeInfo
  { prec :: !(Maybe T.Text)
  , timezone :: !Bool
  } deriving (Generic, Show, Eq)

instance A.FromJSON TimestampTypeInfo

data NumericTypeInfo = NumericTypeInfo
  { prec :: !(Maybe Word)
  , decimal :: !(Maybe Word)
  } deriving (Generic, Show, Eq)

instance A.FromJSON NumericTypeInfo

data TypeInfo = TypeInfo
  { be_specific :: !T.Text
  , be_data :: !A.Value
  } deriving (Generic, Show, Eq)

instance A.FromJSON TypeInfo where
  parseJSON =
    A.genericParseJSON
      $ A.defaultOptions
          { A.fieldLabelModifier =
              Prelude.map
                (\c ->
                   if c == '_'
                     then '-'
                     else c)
          }

data ColumnInfo
  = VarChar
      { varchar :: !CharTypeInfo
      }
  | Char
      { char :: !CharTypeInfo
      }
  | Timestamp
      { timestamp :: !TimestampTypeInfo
      }
  | Integer
  | Numeric
      { numeric :: !NumericTypeInfo
      }
  | Boolean
  | JSON
  | Double
  | Bytea !TypeInfo
  | Enum !T.Text
  | BigInt
  deriving (Generic, Show, Eq)

instance A.FromJSON ColumnInfo where
  parseJSON =
    \case
      A.String "int" -> return Integer
      A.String "boolean" -> return Boolean
      A.String "double" -> return Double
      A.String "bigint" -> return BigInt
      val ->
        case A.fromJSON val of
          A.Error err ->
            A.genericParseJSON
              (A.defaultOptions {A.sumEncoding = A.UntaggedValue})
              val
          A.Success (result :: TypeInfo) ->
            case be_data result of
              "json" -> return JSON
              "bytea" -> return $ Bytea result
              A.Object hm ->
                case HM.lookup "customType" hm of
                  Just (A.String enum) -> return $ Enum enum
                  _unknown -> fail $ "Expected enum got " ++ show val
              _ignore -> fail $ "Unhandled column type " ++ show val

data TableHasColumnInfo = TableHasColumnInfo
  { _table :: !BM.QualifiedName
  , _column :: !T.Text
  , _type :: !ColumnInfo
  } deriving (Generic, Show, Eq)

instance A.FromJSON TableHasColumnInfo where
  parseJSON =
    A.genericParseJSON
      $ A.defaultOptions {A.fieldLabelModifier = Prelude.drop 1}

data ConstraintType =
  NOT_NULL
  deriving (Generic, Show, Eq)

instance A.FromJSON ConstraintType where
  parseJSON =
    \case
      (A.String "not-null") -> return NOT_NULL
      c -> fail $ "Unhandled constraint type: " ++ show c

data ConstraintInfo = ConstraintInfo
  { name :: !(Maybe T.Text) -- Constraint Name
  , attributes :: !(Maybe [T.Text])
  , constraint :: !ConstraintType
  } deriving (Generic, Show, Eq)

instance A.FromJSON ConstraintInfo

data ColumnConstraintInfo = ColumnConstraintInfo
  { _table :: !BM.QualifiedName
  , _column :: !T.Text
  , _constraint :: !ConstraintInfo
  } deriving (Generic, Show, Eq)

instance A.FromJSON ColumnConstraintInfo where
  parseJSON =
    A.genericParseJSON
      $ A.defaultOptions {A.fieldLabelModifier = Prelude.drop 1}

data PrimaryKeyInfo = PrimaryKeyInfo
  { table :: !BM.QualifiedName
  , columns :: ![T.Text]
  } deriving (Generic, Show, A.FromJSON, Eq)

newtype TableInfo =
  TableInfo BM.QualifiedName
  deriving (Generic, Show, A.FromJSON, Eq)

data EnumInfo = EnumInfo
  { name :: !T.Text
  , values :: ![T.Text]
  } deriving (Generic, Show, A.FromJSON, Eq)

data TablePredicate =
  TablePredicate
    !TableInfo
    !(Map.Map T.Text ColumnPredicate)
    !(Maybe PrimaryKeyInfo)
  deriving (Generic, Show, Eq)

type ExistingEnumValues = [T.Text]

data DBPredicate
  = DBHasEnum !EnumPredicate
  | DBHasTable !TablePredicate
  | DBTableHasColumns !(Map.Map T.Text ColumnPredicate)
  deriving (Generic, Show, Eq)

instance Ord DBPredicate where
  compare p1 p2 =
    case (p1, p2) of
      (DBHasEnum _, _) -> LT
      (DBHasTable _, DBHasEnum _) -> GT
      (DBHasTable _, DBTableHasColumns _) -> LT
      (DBTableHasColumns _, _) -> GT
      _same -> EQ

data ColumnPredicate = ColumnPredicate
  { columnName :: !T.Text
  , columnTable :: !BM.QualifiedName
  , columnType :: !(Maybe ColumnInfo)
  , columnConstraint :: ![ColumnConstraintInfo]
  , isPrimary :: !(Maybe PrimaryKeyInfo)
  , existsInDB :: !Bool
  } deriving (Generic, Show, Eq)

data EnumPredicate = EnumPredicate
  { enumInfo :: !EnumInfo
  , enumValuesInDB :: ![T.Text]
  } deriving (Generic, Show, Eq)
