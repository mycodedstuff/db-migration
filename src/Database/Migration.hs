module Database.Migration where

import qualified Data.Foldable as DF
import qualified Data.HashSet as HS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as DT
import qualified Database.Beam as B
import qualified Database.Beam.Migrate.Simple as BM
import qualified Database.Beam.Postgres as BP

import Database.Migration.Backend.Postgres
import Database.Migration.Predicate (PgHasSchema(PgHasSchema))
import Database.Migration.Types
import qualified Database.Migration.Types.LinkedHashMap as LHM
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Common

schemaDiff ::
     B.Database BP.Postgres db
  => BP.Connection
  -> Maybe DT.Text
  -> BM.CheckedDatabaseSettings BP.Postgres db
  -> IO (Either String DBDiff)
schemaDiff conn mSchema checkedDB = do
  let haskellConstraints = BM.collectChecks checkedDB
      schema = fromMaybe "public" mSchema
      schemaConstraint = BM.SomeDatabasePredicate $ PgHasSchema schema
  actualPredicates <- getPgConstraintForSchema conn mSchema
  let expected = HS.fromList $ schemaConstraint : haskellConstraints
      actual = HS.fromList actualPredicates
      diff = expected `HS.difference` actual
      groupedDBChecks = groupPredicates actualPredicates
  if HS.null diff
    then return $ Right Sync
    else do
      let dbPredicates =
            sort
              $ LHM.elems
              $ groupPredicates
              $ sortArrUsingRefArr haskellConstraints
              $ HS.toList diff
      Right . Diff
        <$> DF.foldlM
              (\acc p ->
                 (acc ++) . renderQuery @BP.Postgres
                   <$> mutatePredicate @BP.Postgres conn groupedDBChecks p)
              []
              dbPredicates
