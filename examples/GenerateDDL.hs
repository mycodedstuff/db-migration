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
import qualified Database.Migration.Utils.Beam as UB

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
  let schema =  pack "migration"
  putStrLn "\nTable names ==>"
  print (UB.collectTableNames $ testDB (Just schema))
  let options = DBM.defaultOptions  {schemaName = [schema]}
  putStrLn "\nSchema dump ==>"
  sqlQueries <- DBM.createSchema options (testDB (Just schema))
  traverse_ (putStrLn . unpack) sqlQueries
  putStrLn "\nSchema diff ==>"
  diff <- DBM.schemaDiff conn (testDB (Just schema)) options
  BP.close conn
