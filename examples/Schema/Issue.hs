module Schema.Issue where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Text
import Data.Time (UTCTime)
import qualified Database.Beam as B
import qualified Database.Beam.Backend as BA
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.CustomTypes as BP
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.PostgreSQL.Simple.FromField as PS
import GHC.Generics (Generic)

import Database.Migration.Types
import Database.Migration.Utils.Beam

data IssueT f = Issue
  { _id :: !(B.C f Text)
  , _ticketNo :: !(B.C f Int)
  , _message :: !(B.C f Text)
  , _status :: !(B.C f IssueStatus)
  , _image :: !(B.C f (Maybe BS.ByteString))
  , _store :: !(B.C f (Maybe (BP.PgJSON A.Value)))
  , _createdAt :: !(B.C f UTCTime)
  , _updatedAt :: !(B.C f UTCTime)
  } deriving (Generic, B.Beamable)

data IssueStatus
  = RAISED
  | ACTIVE
  | RESOLVED
  deriving (Generic, Show, Eq, A.ToJSON, A.FromJSON)

instance B.FromBackendRow BP.Postgres IssueStatus

instance PS.FromField IssueStatus where
  fromField f mbValue = do
    case mbValue of
      Nothing ->
        PS.returnError BP.UnexpectedNull f "Unexpected null in enum type"
      Just value ->
        case value of
          "RAISED" -> pure RAISED
          "ACTIVE" -> pure ACTIVE
          "RESOLVED" -> pure RESOLVED
          x ->
            PS.returnError
              BP.ConversionFailed
              f
              ("Unexpected value in enum type: " <> show x)

instance B.HasSqlEqualityCheck BP.Postgres IssueStatus

instance BA.HasSqlValueSyntax BP.PgValueSyntax IssueStatus where
  sqlValueSyntax = BP.pgEnumValueSyntax show

instance BM.HasDefaultSqlDataType BP.Postgres IssueStatus where
  defaultSqlDataType _ _ _ = pgDefaultEnumerationType
  defaultSqlDataTypeConstraints p _ _ = [enumFieldCheckId p]

instance B.Table IssueT where
  data PrimaryKey IssueT f =
    IssuePrimaryKey (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = IssuePrimaryKey . _id
  tableIndexes tblName tblFields@Issue {..} =
    [ uniqueIndex tblName [IC _ticketNo]
    , defaultIndexWithPred
        (tblName <> "_ticketNo_customIdx")
        tblFields
        [IC _ticketNo] $ \Issue {..} ->
        _status
          B.==. B.val_ RAISED
          B.&&. _message
          B.==. B.val_ "dummy"
          B.||. B.isNothing_ _store
    ]

issueEMod ::
     Text
  -> Maybe Text
  -> B.EntityModification
       (BM.CheckedDatabaseEntity be db)
       be
       (B.TableEntity IssueT)
issueEMod tableName schemaName =
  modifyCheckedEntitySchema (const schemaName)
    <> BM.modifyCheckedTable
         (const tableName)
         BM.checkedTableModification
           { _id = "id"
           , _ticketNo = "ticketNo"
           , _message = "message"
           , _status = "status"
           , _image = "image"
           , _store = "store"
           , _createdAt = "createdAt"
           , _updatedAt = "updatedAt"
           }
