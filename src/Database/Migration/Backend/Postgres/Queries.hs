module Database.Migration.Backend.Postgres.Queries where

import Data.String (fromString)
import qualified Data.Text as T
import qualified Database.Beam.Postgres as BP
import qualified Database.PostgreSQL.Simple as Pg

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Database.Migration.Utils.Common

getCurrentDatabase :: BP.Connection -> IO (Maybe T.Text)
getCurrentDatabase conn =
  headMaybe . fmap Pg.fromOnly
    <$> Pg.query_ conn (fromString "select current_database();")

getSequencesFromPg ::
     BP.Connection
  -> Maybe T.Text
  -> IO [(String, String, String, String, String, String, String, String)]
getSequencesFromPg conn mSchema =
  Pg.query_ conn
    $ fromString
    $ unlines
        [ "select sequence_schema, sequence_name, minimum_value,"
        , "maximum_value, start_value, increment, cycle_option, data_type"
        , "from information_schema.sequences"
        , "where sequence_schema = '" ++ maybe "public" T.unpack mSchema ++ "';"
        ]

getSchemasFromPg :: BP.Connection -> T.Text -> IO [T.Text]
getSchemasFromPg conn databaseName =
  fmap Pg.fromOnly
    <$> Pg.query_
          conn
          (fromString
             $ "select schema_name from information_schema.schemata where catalog_name = '"
                 ++ T.unpack databaseName
                 ++ "';")

getColumnDefaultsFromPg ::
     BP.Connection -> Maybe T.Text -> IO [(T.Text, T.Text, T.Text, T.Text)]
getColumnDefaultsFromPg conn mSchema =
  Pg.query_ conn
    $ fromString
    $ unlines
        [ "select table_schema, table_name, column_name, column_default"
        , "from information_schema.columns where"
        , "column_default is not null and"
        , "table_schema = '" ++ maybe "public" T.unpack mSchema ++ "';"
        ]

getSearchPath :: BP.Connection -> IO [T.Text]
getSearchPath conn =
  fromMaybe [] . headMaybe . fmap (V.toList . Pg.fromOnly)
    <$> Pg.query_ conn (fromString "select current_schemas(false)")

setSearchPath :: BP.Connection -> [T.Text] -> IO ()
setSearchPath conn schemas =
  void
    $ Pg.execute_
        conn
        (fromString
           $ "set search_path = '"
               ++ T.unpack (T.intercalate "', '" schemas)
               ++ "';")
