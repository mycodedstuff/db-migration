{-# LANGUAGE PolyKinds #-}

module Database.Migration where

import Control.Exception
import qualified Data.Foldable as DF
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text
import qualified Database.Beam as B
import Database.Beam.Migrate.Backend as BMB (backendGetDbConstraints)
import qualified Database.Beam.Migrate.Simple as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Migrate as BP

import Database.Migration.Backend.Postgres ()
import Database.Migration.Types
import Database.Migration.Utils.Beam

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
      let dbPredicates = sort $ Map.elems $ groupPredicates predicates
      Right . Diff
        <$> DF.foldlM
              (\acc p ->
                 (acc ++) . renderQuery @BP.Postgres
                   <$> mutatePredicate @BP.Postgres conn p)
              []
              dbPredicates
