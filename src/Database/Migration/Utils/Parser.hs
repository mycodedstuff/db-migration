module Database.Migration.Utils.Parser where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Database.Beam.Migrate.Types as BM

import Database.Migration.Predicate
import Database.Migration.Types

parseTableExistPredicate :: BM.SomeDatabasePredicate -> TableInfo
parseTableExistPredicate (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err ->
      error $ "Couldn't parse predicate TableExistsPredicate : " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("table-exists" :: String) predicateObj of
        Just result -> result
        Nothing -> error $ "Couldn't find table-exists key" ++ show predicateObj

parseTableHasColumnPredicate :: BM.SomeDatabasePredicate -> TableHasColumnInfo
parseTableHasColumnPredicate (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err ->
      error $ "Couldn't parse predicate TableHasColumn: " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-column" :: String) predicateObj of
        Just result -> result
        Nothing -> error $ "Couldn't find has-column key" ++ show predicateObj

parseTableColumnConstraintPredicate ::
     BM.SomeDatabasePredicate -> ColumnConstraintInfo
parseTableColumnConstraintPredicate (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err ->
      error $ "Couldn't parse predicate TableColumnHasConstraint: " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-column-constraint" :: String) predicateObj of
        Just result -> result
        Nothing ->
          error $ "Couldn't find has-column-constraint key" ++ show predicateObj

parseTableHasPrimaryKeyPredicate :: BM.SomeDatabasePredicate -> PrimaryKeyInfo
parseTableHasPrimaryKeyPredicate (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err ->
      error $ "Couldn't parse predicate TableHasPrimaryKey: " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-primary-key" :: String) predicateObj of
        Just result -> result
        Nothing -> error $ "Couldn't find has-primary key" ++ show predicateObj

parsePgHasEnum :: BM.SomeDatabasePredicate -> EnumInfo
parsePgHasEnum (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err -> error $ "Couldn't parse predicate PgHasEnum: " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-postgres-enum" :: String) predicateObj of
        Just result -> result
        Nothing ->
          error $ "Couldn't find has-postgres-enum key" ++ show predicateObj

parsePgHasSequence :: BM.SomeDatabasePredicate -> PgHasSequence
parsePgHasSequence (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err ->
      error $ "Couldn't parse predicate PgHasSequence: " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-postgres-sequence" :: String) predicateObj of
        Just result -> result
        Nothing ->
          error $ "Couldn't find has-postgres-sequence key" ++ show predicateObj

parseTableHasColumnDefault :: BM.SomeDatabasePredicate -> TableColumnHasDefault
parseTableHasColumnDefault (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err ->
      error $ "Couldn't parser predicate TableHasColumnDefault " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-column-default" :: String) predicateObj of
        Just result -> result
        Nothing ->
          error
            $ "Couldn't find has-column-default key in " ++ show predicateObj

parsePgHasSchema :: BM.SomeDatabasePredicate -> PgHasSchema
parsePgHasSchema (BM.SomeDatabasePredicate p) =
  case A.fromJSON $ BM.serializePredicate p of
    A.Error err -> error $ "Couldn't parser predicate PgHasSchema " ++ show err
    A.Success predicateObj ->
      case HM.lookup ("has-postgres-schema" :: String) predicateObj of
        Just result -> result
        Nothing ->
          error
            $ "Couldn't find has-postgres-schema key in " ++ show predicateObj
