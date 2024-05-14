module Database.Migration.Predicate where

import Data.Aeson (KeyValue((.=)), object)
import qualified Data.Aeson as A
import Data.ByteString as BS ()
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import GHC.Generics (Generic)

data SequenceTypes
  = SmallInt
  | Integer
  | BigInt
  deriving (Show, Eq, Generic, Hashable)

instance A.FromJSON SequenceTypes where
  parseJSON =
    A.genericParseJSON
      $ A.defaultOptions {A.constructorTagModifier = fmap toLower}

instance A.ToJSON SequenceTypes where
  toJSON =
    A.genericToJSON $ A.defaultOptions {A.constructorTagModifier = fmap toLower}

data PgHasSequence = PgHasSequence
  { seqName :: !BM.QualifiedName -- ^ Sequence name
  , seqRange :: !(Integer, Integer) -- ^ Sequence range
  , seqOffset :: !Integer -- ^ Sequence start number
  , seqStep :: !Integer -- ^ Sequence increment by
  , seqCycle :: !Bool -- ^ Sequence has cycle
  , seqType :: !SequenceTypes
  } deriving (Show, Eq, Generic)

instance Hashable PgHasSequence

instance A.FromJSON PgHasSequence where
  parseJSON =
    A.withObject "PgHasSequence" $ \v ->
      PgHasSequence
        <$> v A..: "sequence"
        <*> v A..: "range"
        <*> v A..: "offset"
        <*> v A..: "step"
        <*> v A..: "cycle"
        <*> v A..: "type"

instance BM.DatabasePredicate PgHasSequence where
  englishDescription (PgHasSequence name range offset step wrap _type) =
    "Sequence "
      <> show name
      <> " must exist with range "
      <> show range
      <> ", offset "
      <> show offset
      <> ", step "
      <> show step
      <> ", cycle "
      <> show wrap
      <> ", type "
      <> (toLower <$> show _type)
  predicateSpecificity _ = BM.PredicateSpecificityOnlyBackend "Postgres"
  serializePredicate (PgHasSequence name range offset step wrap _type) =
    object
      [ "has-postgres-sequence"
          .= object
               [ "sequence" .= name
               , "range" .= range
               , "offset" .= offset
               , "step" .= step
               , "cycle" .= wrap
               , "type" .= _type
               ]
      ]
  predicateCascadesDropOn _ _ = False -- Check for column using sequences

data ColumnDefault
  = LiteralStr !T.Text
  | LiteralInt
      { scale :: !(Maybe Int)
      , num :: !Scientific
      }
  | Sequence !T.Text
  deriving (Generic, Show, Eq)

instance Hashable ColumnDefault

instance A.FromJSON ColumnDefault where
  parseJSON =
    \case
      A.String str ->
        if T.isPrefixOf "nextval" str
          then return $ Sequence str
          else return $ LiteralStr str
      val ->
        A.withObject
          "LiteralInt"
          (\v -> LiteralInt <$> v A..: "scale" <*> v A..: "num")
          val

instance A.ToJSON ColumnDefault where
  toJSON =
    \case
      LiteralStr str -> A.String str
      LiteralInt scale num ->
        A.Object
          $ HM.fromList [("scale", A.toJSON scale), ("num", A.Number num)]
      Sequence name -> A.String name

data TableColumnHasDefault = TableColumnHasDefault
  { table :: !BM.QualifiedName -- ^ Table name
  , colName :: !T.Text -- ^ Column Name
  , defaultValue :: !ColumnDefault -- ^ Default value of column
  } deriving (Generic, Show, Eq)

instance Hashable TableColumnHasDefault

instance A.FromJSON TableColumnHasDefault where
  parseJSON =
    A.withObject "TableColumnHasDefault" $ \v ->
      TableColumnHasDefault
        <$> v A..: "table"
        <*> v A..: "columnName"
        <*> v A..: "defaultValue"

instance BM.DatabasePredicate TableColumnHasDefault where
  englishDescription (TableColumnHasDefault tableName columnName defaultValue) =
    "Table "
      <> show tableName
      <> " with column "
      <> show columnName
      <> " must have default "
      <> show defaultValue
  predicateSpecificity _ = BM.PredicateSpecificityAllBackends
  serializePredicate (TableColumnHasDefault tableName columnName defaultValue) =
    object
      [ "has-column-default"
          .= object
               [ "table" .= tableName
               , "columnName" .= columnName
               , "defaultValue" .= defaultValue
               ]
      ]
  predicateCascadesDropOn _ _ = False -- Check for cascades

newtype PgHasSchema = PgHasSchema
  { schemaName :: T.Text
  } deriving (Generic, Show, Eq)
    deriving anyclass (A.FromJSON)

instance Hashable PgHasSchema

instance BM.DatabasePredicate PgHasSchema where
  englishDescription (PgHasSchema schemaName) =
    "Postgres must have schema " ++ T.unpack schemaName
  predicateSpecificity _ = BM.PredicateSpecificityOnlyBackend "Postgres"
  serializePredicate (PgHasSchema schemaName) =
    object ["has-postgres-schema" .= object ["schemaName" .= schemaName]]
