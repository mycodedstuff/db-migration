module Database.Migration.Backend.Postgres.Checks where

import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Migrate as BP
import Text.Read (readMaybe)

import Database.Migration.Backend.Postgres.Queries
import Database.Migration.Predicate
import Database.Migration.Utils.Beam (parseColumnDefault, yesNoToBool)
import Database.Migration.Utils.Common (fromResult, headMaybe)
import qualified Data.List as DL
import qualified Data.Foldable as DF

getPgConstraintForSchema ::
     BP.Connection -> [T.Text] -> IO [BM.SomeDatabasePredicate]
getPgConstraintForSchema conn mSchema = do
  searchPaths <- getSearchPath conn
  {- Hack to get consistent results from postgres
   - Postgres has a tendency to remove schema information if the schema is the first element of search path
   - Hence removing search path and then resetting it at the end
  -}
  setSearchPath conn []
  dbConstraints <-
    BP.getDbConstraintsForSchemas
      (Just $ T.unpack <$> mSchema)
      conn
  pgSequences <-
        DF.foldl' (\acc' x -> do
          acc <- acc'
          pgseq <- getSequencesFromPg conn x
          return $ pgseq ++ acc
           ) (return []) mSchema
  let counterPredicates = sequenceChecks pgSequences
  schemaPredicates <-
    fmap (BM.SomeDatabasePredicate . PgHasSchema) <$> getSchemasFromPg conn
  columnDefaults <- 
      DF.foldl' (\acc' x ->  do 
        acc <- acc'
        colDef <- getColumnDefaultsFromPg conn x
        return $ colDef ++ acc) (return []) mSchema
  let columnDefaultPredicates = columnDefaultChecks columnDefaults
  setSearchPath conn searchPaths
  return
    $ schemaPredicates
        ++ counterPredicates
        ++ dbConstraints
        ++ columnDefaultPredicates

sequenceChecks ::
     [(String, String, String, String, String, String, String, String)]
  -> [BM.SomeDatabasePredicate]
sequenceChecks =
  fmap
    (\(schemaName, name, minV, maxV, start, inc, wrap, _type) ->
       BM.SomeDatabasePredicate
         $ PgHasSequence
             (BM.QualifiedName (Just $ T.pack schemaName) $ T.pack name)
             (fromMaybe 0 $ readMaybe minV, fromMaybe 0 $ readMaybe maxV)
             (fromMaybe 0 $ readMaybe start)
             (fromMaybe 0 $ readMaybe inc)
             (yesNoToBool wrap)
             (fromResult
                (const $ error $ "Sequence type decode failed for " ++ _type)
                $ A.fromJSON
                $ A.String
                $ T.pack _type))

columnDefaultChecks ::
     [(T.Text, T.Text, T.Text, T.Text)] -> [BM.SomeDatabasePredicate]
columnDefaultChecks =
  fmap
    (\(schemaName, tableName, columnName, defaultValue) ->
       BM.SomeDatabasePredicate
         $ TableColumnHasDefault
             (BM.QualifiedName (Just schemaName) tableName)
             columnName
             (parseColumnDefault defaultValue))
