{-# LANGUAGE PolyKinds #-}

module Database.Migration where

import qualified Data.Foldable as DF
import qualified Data.HashSet as HS
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import qualified Data.Text as DT
import qualified Database.Beam as B
import qualified Database.Beam.Migrate.Simple as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Migrate as BP
import qualified Database.PostgreSQL.Simple as Pg
import Text.Read (readMaybe)

import Database.Migration.Backend.Postgres ()
import Database.Migration.Predicate (PgHasSequence(PgHasSequence))
import Database.Migration.Types
import Database.Migration.Utils.Beam

schemaDiff ::
     B.Database BP.Postgres db
  => BP.Connection
  -> Maybe DT.Text
  -> BM.CheckedDatabaseSettings BP.Postgres db
  -> IO (Either String DBDiff)
schemaDiff conn schema checkedDB = do
  let haskellConstraints = BM.collectChecks checkedDB
  dbConstraints <-
    BP.getDbConstraintsForSchemas (sequence [DT.unpack <$> schema]) conn
  counters <-
    Pg.query_ conn
      $ fromString
      $ unlines
          [ "select sequence_schema, sequence_name, minimum_value,"
          , "maximum_value, start_value, increment, cycle_option"
          , "from information_schema.sequences"
          ]
  let counterPredicate =
        fmap
          (\(schemaName, name, minV, maxV, start, inc, wrap) ->
             BM.SomeDatabasePredicate
               $ PgHasSequence
                   (BM.QualifiedName
                      (if schemaName == "public" && isJust schema
                         then Just schemaName
                         else Nothing)
                      $ DT.pack name)
                   (fromMaybe 0 $ readMaybe minV, fromMaybe 0 $ readMaybe maxV)
                   (fromMaybe 0 $ readMaybe start)
                   (fromMaybe 0 $ readMaybe inc)
                   (yesNoToBool wrap))
          counters
  let expected = HS.fromList haskellConstraints
      actual = HS.fromList $ dbConstraints ++ counterPredicate
      diff = expected `HS.difference` actual
      groupedDBChecks = groupPredicates $ dbConstraints ++ counterPredicate
  if HS.null diff
    then return $ Right Sync
    else do
      let dbPredicates = sort $ Map.elems $ groupPredicates $ HS.toList diff
      Right . Diff
        <$> DF.foldlM
              (\acc p ->
                 (acc ++) . renderQuery @BP.Postgres
                   <$> mutatePredicate @BP.Postgres conn groupedDBChecks p)
              []
              dbPredicates
