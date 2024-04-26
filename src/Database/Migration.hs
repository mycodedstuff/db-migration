{-# LANGUAGE PolyKinds #-}

module Database.Migration where

import Control.Exception
import Data.Foldable
import qualified Data.List as DL
import Data.Monoid (Endo(..))
import Data.Text
import Data.Typeable (typeOf)
import qualified Database.Beam as B
import Database.Beam.Migrate (collectChecks)
import Database.Beam.Migrate.Backend as BMB (backendGetDbConstraints)
import qualified Database.Beam.Migrate.Simple as BM
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Migrate as BP
import qualified Database.Beam.Schema.Tables as BM

import Database.Migration.Backend.Postgres
import Database.Migration.Types
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Common

schemaDiff ::
     B.Database BP.Postgres db
  => BP.Connection
  -> Maybe Text
  -> BM.CheckedDatabaseSettings BP.Postgres db
  -> IO (Either String DBDiff)
schemaDiff conn schema checkedDB = do
  diff <-
    try @SomeException
      $ BP.runBeamPostgres conn
      $ BM.verifySchema
          (BP.migrationBackend
             { BMB.backendGetDbConstraints =
                 B.liftIO
                   (BP.getDbConstraintsForSchemas
                      ((: []) . unpack <$> schema)
                      conn)
             })
          checkedDB
  case diff of
    Left err -> do
      return $ Left $ "Failed to find schema details due to " ++ show err
    Right BM.VerificationSucceeded -> return $ Right Sync
    Right (BM.VerificationFailed predicates) -> do
      let sortedPredicates =
            DL.sortOn
              (\(BM.SomeDatabasePredicate p) ->
                 case show $ typeOf p of
                   "TableExistsPredicate" -> 1
                   "TableHasColumn Postgres" -> 2
                   "TableColumnHasConstraint Postgres" -> 3
                   "TableHasPrimaryKey" -> 4
                   _ignore -> 99)
              predicates
      let dbTree = constructDBTree $ groupByTableName sortedPredicates
          enumPredicates =
            DL.filter
              (\(BM.SomeDatabasePredicate p) -> show (typeOf p) == "PgHasEnum")
              predicates
      enumQueries <-
        Prelude.concat <$> mapM (predicateToMigrationQuery conn) enumPredicates
      ddlQueries <-
        traverseTree
          (\case
             (TreeNode name maybePredicate) -> do
               case maybePredicate of
                 Just predicate -> do
                   predicateToMigrationQuery conn predicate
                 Nothing -> return []
             Leaf predicate -> do
               predicateToMigrationQuery conn predicate)
          dbTree
      return . Right . Diff $ enumQueries ++ ddlQueries
