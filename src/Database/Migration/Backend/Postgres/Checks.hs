module Database.Migration.Backend.Postgres.Checks where

import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Migrate as BP
import Database.Migration.Backend.Postgres.Queries
import Database.Migration.Predicate
import Database.Migration.Utils.Beam (yesNoToBool, parseColumnDefault)
import Text.Read (readMaybe)

getPgConstraintForSchema ::
     BP.Connection -> Maybe T.Text -> IO [BM.SomeDatabasePredicate]
getPgConstraintForSchema conn mSchema = do
  mDatabaseName <- getCurrentDatabase conn
  let databaseName =
        case mDatabaseName of
          Just dbName -> dbName
          Nothing -> error "Couldn't find database name"
  dbConstraints <-
    BP.getDbConstraintsForSchemas (sequence [T.unpack <$> mSchema]) conn
  counterPredicates <- sequenceChecks mSchema <$> getSequencesFromPg conn mSchema
  schemaPredicates <-
    fmap (BM.SomeDatabasePredicate . PgHasSchema)
      <$> getSchemasFromPg conn databaseName
  columnDefatulPredicates <- columnDefaultChecks mSchema <$> getColumnDefaultsFromPg conn mSchema
  return $ schemaPredicates ++ counterPredicates ++ dbConstraints ++ columnDefatulPredicates

sequenceChecks ::
     Maybe T.Text
  -> [(String, String, String, String, String, String, String)]
  -> [BM.SomeDatabasePredicate]
sequenceChecks mSchema =
  fmap
    (\(schemaName, name, minV, maxV, start, inc, wrap) ->
       BM.SomeDatabasePredicate
         $ PgHasSequence
             (BM.QualifiedName
                (if schemaName == "public" && isNothing mSchema
                   then Nothing
                   else Just $ T.pack schemaName)
                $ T.pack name)
             (fromMaybe 0 $ readMaybe minV, fromMaybe 0 $ readMaybe maxV)
             (fromMaybe 0 $ readMaybe start)
             (fromMaybe 0 $ readMaybe inc)
             (yesNoToBool wrap))

columnDefaultChecks :: Maybe T.Text -> [(T.Text, T.Text, T.Text, T.Text)] -> [BM.SomeDatabasePredicate]
columnDefaultChecks mSchema =
  fmap
    (\(schemaName, tableName, columnName, defaultValue) ->
        BM.SomeDatabasePredicate
          $ TableColumnHasDefault
             (BM.QualifiedName
                (if schemaName == "public" && isNothing mSchema
                   then Nothing
                   else Just  schemaName) tableName)
             columnName
             (parseColumnDefault defaultValue)
             )
