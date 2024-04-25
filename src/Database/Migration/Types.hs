module Database.Migration.Types where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Tree as DT
import qualified Database.Beam.Migrate.Generics as BMG
import qualified Database.Beam.Migrate.Simple as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Syntax as BPS
import GHC.Generics

data DBDiff
  = Sync
  | Diff ![String]
  deriving (Show)

data TreeNode
  = TreeNode
      { name :: !T.Text
      , predicate :: !(Maybe BM.SomeDatabasePredicate)
      }
  | Leaf !BM.SomeDatabasePredicate
  deriving (Show, Eq)

type DBTree = DT.Tree TreeNode

type PredicateInfo a = HM.HashMap String a

data CharTypeInfo = CharTypeInfo
  { prec :: !(Maybe T.Text)
  , collation :: !(Maybe T.Text)
  } deriving (Generic, Show)

instance A.FromJSON CharTypeInfo

data TimestampTypeInfo = TimestampTypeInfo
  { prec :: !(Maybe T.Text)
  , timezone :: !Bool
  } deriving (Generic, Show)

instance A.FromJSON TimestampTypeInfo

data NumericTypeInfo = NumericTypeInfo
  { prec :: !(Maybe Word)
  , decimal :: !(Maybe Word)
  } deriving (Generic, Show)

instance A.FromJSON NumericTypeInfo

data TypeInfo = TypeInfo
  { be_specific :: !T.Text
  , be_data :: !A.Value
  } deriving (Generic, Show)

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
  deriving (Generic, Show)

instance A.FromJSON ColumnInfo where
  parseJSON =
    \case
      A.String "int" -> return Integer
      A.String "boolean" -> return Boolean
      A.String "double" -> return Double
      A.String "bigint" -> return BigInt
      val ->
        case A.fromJSON val of
          A.Error err --fail $ "Expected TypeInfo got : " ++ show err
           ->
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
  } deriving (Generic, Show)

instance A.FromJSON TableHasColumnInfo where
  parseJSON =
    A.genericParseJSON
      $ A.defaultOptions {A.fieldLabelModifier = Prelude.drop 1}

data ConstraintType
  = NOT_NULL
  | UNIQUE
  | PRIMARY_KEY -- Check, ForeignKey
  deriving (Generic, Show)

instance A.FromJSON ConstraintType where
  parseJSON =
    \case
      (A.String "not-null") -> return NOT_NULL
      (A.String "unique") -> return UNIQUE
      (A.String "primary-key") -> return PRIMARY_KEY
      c -> fail $ "Unhandled constraint type: " ++ show c

data ConstraintInfo = ConstraintInfo
  { name :: !(Maybe String) -- Constraint Name
  , attributes :: !(Maybe [String])
  , constraint :: !ConstraintType
  } deriving (Generic, Show)

instance A.FromJSON ConstraintInfo

data ColumnConstraintInfo = ColumnConstraintInfo
  { _table :: !BM.QualifiedName
  , _column :: !T.Text
  , _constraint :: !ConstraintInfo
  } deriving (Generic, Show)

instance A.FromJSON ColumnConstraintInfo where
  parseJSON =
    A.genericParseJSON
      $ A.defaultOptions {A.fieldLabelModifier = Prelude.drop 1}

data PrimaryKeyInfo = PrimaryKeyInfo
  { table :: !BM.QualifiedName
  , columns :: ![T.Text]
  } deriving (Generic, Show, A.FromJSON)

newtype TableInfo =
  TableInfo BM.QualifiedName
  deriving (Generic, Show, A.FromJSON)

data EnumInfo = EnumInfo
  { name :: !T.Text
  , values :: ![T.Text]
  } deriving (Generic, Show, A.FromJSON)

instance BMG.HasDefaultSqlDataType BP.Postgres A.Value where
  defaultSqlDataType _ _ _ = BPS.pgJsonType
