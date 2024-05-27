module Database.Migration where

import qualified Data.Foldable as DF
import qualified Data.HashSet as HS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate.Simple as BM
import qualified Database.Beam.Postgres as BP

import Database.Migration.Backend.Postgres
import Database.Migration.Predicate (PgHasSchema(PgHasSchema))
import Database.Migration.Types
import qualified Database.Migration.Types.LinkedHashMap as LHM
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Check
import Database.Migration.Utils.Common

schemaDiff ::
     B.Database BP.Postgres db
  => BP.Connection
  -> BM.CheckedDatabaseSettings BP.Postgres db
  -> Options
  -> IO (Either String DBDiff)
schemaDiff conn checkedDB options = do
  let haskellConstraints =
        collectPartitionChecks (partitionOptions options) checkedDB
      mSchema = schemaName options
      schema = fromMaybe "public" mSchema
      schemaConstraint = BM.SomeDatabasePredicate $ PgHasSchema schema
  actualPredicates <- getPgConstraintForSchema conn mSchema
  let expected = HS.fromList $ schemaConstraint : haskellConstraints
      actual = HS.fromList actualPredicates
      diff = expected `HS.difference` actual
      groupedDBChecks = groupPredicates actualPredicates
  let dbPredicates =
        sort
          $ LHM.elems
          $ groupPredicates
          $ sortArrUsingRefArr haskellConstraints
          $ HS.toList diff
      lenientPredicates =
        DF.foldl'
          (\preds dP ->
             maybe preds (snoc preds)
               $ lenientPredicateCheck options dP groupedDBChecks)
          []
          dbPredicates
  if null lenientPredicates
    then return $ Right Sync
    else do
      Right . Diff
        <$> DF.foldlM
              (\acc p ->
                 (acc ++) . renderQuery @BP.Postgres
                   <$> mutatePredicate @BP.Postgres conn groupedDBChecks p)
              []
              lenientPredicates
