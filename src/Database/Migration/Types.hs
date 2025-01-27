module Database.Migration.Types where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Schema.Tables as BT
import GHC.Generics

import qualified Database.Migration.Predicate as MP
import qualified Database.Migration.Types.LinkedHashMap as LHM

data SchemaDiffResult =  
    DB_IN_SYNC 
  | Difference DBDiff 
  | DB_NOT_IN_SYNC 
  | Error String
  deriving (Eq,Show)

type DBDiff = [T.Text]

class RenderPredicate be p where
  renderQuery :: p -> [T.Text]
  mutatePredicate ::
       Maybe BP.Connection -> LHM.LinkedHashMap T.Text DBPredicate -> p -> IO p

type ColumnTypeCheck = T.Text -> T.Text -> ColumnType -> ColumnType -> Bool

data PartitionOption = PartitionOption
  { includeParentTable :: !Bool
  , partitionMap :: !(HM.HashMap T.Text [T.Text])
  }

data Options = Options
  { schemaName :: ![T.Text]
  , typeLenient :: !(Maybe ColumnTypeCheck)
  , partitionOptions :: !PartitionOption
  , ignoreEnumOrder :: !Bool
  , ignoreIndexName :: !Bool
  , listDifference :: !Bool 
  }

defaultPartitionOption :: PartitionOption
defaultPartitionOption = PartitionOption True HM.empty

defaultOptions :: Options
defaultOptions = Options [] Nothing defaultPartitionOption False False True

-- | A type used to wrap columns for defining indexes
data IndexColumn where
  IC :: forall tbl a. BT.TableField tbl a -> IndexColumn

type PredicateInfo a = Map.Map T.Text a

data CharTypeInfo = CharTypeInfo
  { prec :: !(Maybe Integer)
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

data CustomType
  = CustomType
      { customType :: !BM.QualifiedName
      }
  | ArrType
      { mod :: !Integer
      , oid :: !Integer
      }
  deriving (Generic, Show, Eq)

instance A.FromJSON CustomType where
  parseJSON =
    A.genericParseJSON $ A.defaultOptions {A.sumEncoding = A.UntaggedValue}

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

data ColumnType
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
  | Bytea
  | Enum !BM.QualifiedName
  | BigInt
  | SmallInt
  | PgText
  | JSONB
  | Arr !ColumnType
  deriving (Generic, Show, Eq)

instance A.FromJSON ColumnType where
  parseJSON v =
    case v of
      "int" -> return Integer
      "boolean" -> return Boolean
      "double" -> return Double
      "bigint" -> return BigInt
      "smallint" -> return SmallInt
      val -> do
        case val of
          (A.Object hm) ->
            case HM.toList hm of
              [("varchar", value)] -> VarChar <$> A.parseJSON value
              [("timestamp", value)] -> Timestamp <$> A.parseJSON value
              [("numeric", value)] -> Numeric <$> A.parseJSON value
              [("char", value)] -> Char <$> A.parseJSON value
              [("be-specific", _), ("be-data", value)] ->
                case value of
                  "json" -> return JSON
                  "bytea" -> return Bytea
                  "text" -> return PgText
                  "jsonb" -> return JSONB
                  rest ->
                    case A.fromJSON rest of
                      A.Error err -> fail $ "Expected CustomType: " ++ show err
                      A.Success (res :: CustomType) ->
                        case res of
                          CustomType name -> return $ Enum name
                          ArrType _mod oid ->
                            if oid == 1015
                              then return
                                     $ Arr
                                     $ VarChar
                                     $ CharTypeInfo (Just _mod) Nothing
                              else fail $ "Unhandled oid " ++ show val
              _unhandled -> fail $ "Couldn't parse object " ++ show _unhandled
          _unknown -> fail $ "Expected Object got " ++ show _unknown

data TableHasColumnInfo = TableHasColumnInfo
  { _table :: !BM.QualifiedName
  , _column :: !T.Text
  , _type :: !ColumnType
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
  deriving (Generic, Show, Eq)
  deriving anyclass (A.FromJSON)

data EnumInfo = EnumInfo
  { name :: !BM.QualifiedName
  , values :: ![T.Text]
  } deriving (Generic, Show, A.FromJSON, Eq)

data TablePredicate =
  TablePredicate
    !TableInfo
    !(LHM.LinkedHashMap T.Text ColumnPredicate)
    !(Maybe PrimaryKeyInfo)
  deriving (Generic, Show, Eq)

type ExistingEnumValues = [T.Text]

data SequencePredicate = SequencePredicate
  { predicate :: !MP.PgHasSequence
  , sequenceInDB :: !(Maybe MP.PgHasSequence)
  } deriving (Generic, Show, Eq)

data DBPredicate
  = DBHasEnum !EnumPredicate
  | DBHasSequence !SequencePredicate
  | DBHasTable !TablePredicate
  | DBTableHasColumns !(LHM.LinkedHashMap T.Text ColumnPredicate)
  | DBHasSchema !MP.PgHasSchema
  | DBTableHasIndex !TableHasIndexPredicate
  deriving (Generic, Show, Eq)

instance Ord DBPredicate where
  compare p1 p2 =
    case (p1, p2) of
      (DBHasSchema _, _) -> LT
      (DBHasEnum _, DBHasSchema _) -> GT
      (DBHasEnum _, _) -> LT
      (DBHasSequence _, DBHasSchema _) -> GT
      (DBHasSequence _, DBHasEnum _) -> GT
      (DBHasSequence _, _) -> LT
      (DBHasTable _, DBHasSchema _) -> GT
      (DBHasTable _, DBHasEnum _) -> GT
      (DBHasTable _, DBHasSequence _) -> GT
      (DBHasTable _, _) -> LT
      (DBTableHasColumns _, DBHasSchema _) -> GT
      (DBTableHasColumns _, DBHasEnum _) -> GT
      (DBTableHasColumns _, DBHasSequence _) -> GT
      (DBTableHasColumns _, _) -> LT
      (DBTableHasIndex _, _) -> GT

data ColumnPredicate = ColumnPredicate
  { columnName :: !T.Text
  , columnTable :: !BM.QualifiedName
  , columnType :: !(Maybe ColumnType)
  , columnConstraint :: ![ColumnConstraintInfo]
  , isPrimary :: !(Maybe PrimaryKeyInfo)
  , columnDefault :: !(Maybe MP.ColumnDefault)
  , columnTypeInDB :: !(Maybe ColumnType) -- If Nothing means the column doesn't exist in database, else the column exist with type
  } deriving (Generic, Show, Eq)

data EnumPredicate = EnumPredicate
  { enumInfo :: !EnumInfo
  , dependentColumns :: ![(BM.QualifiedName, T.Text, ColumnType)]
  , enumValuesInDB :: ![T.Text]
  } deriving (Generic, Show, Eq)

data IndexPredicate = IndexPredicate
  { _table :: !BM.QualifiedName
  , _name :: !T.Text
  , _constraint :: !(Maybe BT.IndexConstraint)
  , _columns :: ![T.Text]
  , _predicate :: !(Maybe T.Text)
  } deriving (Generic, Show, Eq)

instance A.FromJSON IndexPredicate where
  parseJSON =
    A.genericParseJSON $ A.defaultOptions {A.fieldLabelModifier = drop 1}

data TableHasIndexPredicate = TableHasIndexPredicate
  { tableName :: !BM.QualifiedName
  , indexName :: !BM.QualifiedName
  , indexConstraint :: !(Maybe BT.IndexConstraint)
  , indexColumns :: ![T.Text]
  , indexPredicate :: !(Maybe T.Text)
  , existingIndex :: !(Maybe BM.QualifiedName)
  } deriving (Generic, Show, Eq)

-- Attempt to introduce index declaration in db-migration instead of beam
class BT.IsDatabaseEntity be entity =>
      IndexesTable be entity
  where
  indexes :: BM.CheckedDatabaseEntityDescriptor be entity -> [BT.TableIndex]

instance (BT.Beamable tbl, TableIndexed tbl) =>
         IndexesTable BP.Postgres (BT.TableEntity tbl) where
  indexes (BM.CheckedDatabaseTable desc _ _ _) =
    tableIndex @tbl $ BT.dbTableSettings desc

class BT.Beamable tbl =>
      TableIndexed tbl
  where
  tableIndex :: BT.TableSettings tbl -> [BT.TableIndex]
