module GenerateDDL where

import Database.Migration as DBM
import Database.Migration.Types as DBM
import Database.Migration.Utils.Beam as DBM

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Text
import Data.Time (LocalTime)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL.SQL92 as BA
import qualified Database.Beam.Migrate as BM
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.CustomTypes as BP
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.PostgreSQL.Simple.FromField as PS
import GHC.Generics (Generic)

import Schema.Configuration
import Schema.Issue

data TestDB f = TestDB
  { configuration :: !(f (B.TableEntity ConfigurationT))
  , issue :: !(f (B.TableEntity IssueT))
  } deriving (Generic)
    deriving anyclass (B.Database be)

testDB :: Maybe Text -> BM.CheckedDatabaseSettings Postgres TestDB
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
  let schema = Nothing
  diff <- DBM.schemaDiff conn schema $ testDB schema
  case diff of
    Right DBM.Sync -> putStrLn "Schema in sync"
    Right (DBM.Diff queries) -> traverse_ putStrLn queries
    Left err -> putStrLn err
  BP.close conn