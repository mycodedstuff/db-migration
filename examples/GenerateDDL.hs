module GenerateDDL where

import Database.Migration as DBM
import Database.Migration.Types as DBM

import Data.Foldable (traverse_)
import Data.Text
import qualified Database.Beam as B
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import GHC.Generics (Generic)

import Schema.Configuration
import Schema.Issue

data TestDB f = TestDB
  { configuration :: !(f (B.TableEntity ConfigurationT))
  , issue :: !(f (B.TableEntity IssueT))
  } deriving (Generic)
    deriving anyclass (B.Database be)

testDB :: Maybe Text -> BM.CheckedDatabaseSettings BP.Postgres TestDB
testDB schema =
  BM.defaultMigratableDbSettings
    `B.withDbModification` B.dbModification
                             { configuration =
                                 configurationEMod "Configurations" schema
                             , issue = issueEMod "Issues" schema
                             }

main :: IO ()
main = do
  putStrLn "Initiating connect"
  conn <-
    BP.connect
      $ BP.ConnectInfo
          { connectHost = "localhost"
          , connectPort = 5432
          , connectUser = "postgres"
          , connectPassword = ""
          , connectDatabase = "testdb"
          }
  putStrLn "Connected to postgres"
  let schema = Just "migration"
  let options = DBM.defaultOptions {schemaName = schema}
  diff <- DBM.schemaDiff conn (testDB schema) options
  case diff of
    Right DBM.Sync -> putStrLn "Schema in sync"
    Right (DBM.Diff queries) -> traverse_ (putStrLn . unpack) queries
    Left err -> putStrLn err
  BP.close conn
