module Schema.Configuration where

import Data.Function ((&))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Int (Int32)
import Data.Monoid (Endo(Endo))
import Data.Text
import Data.Time (UTCTime)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Schema.Tables as B
import Database.Beam.Schema.Tables (dbEntitySchema)
import GHC.Generics (Generic)

import Database.Migration.Utils.Beam

data ConfigurationT f = ConfigurationT
  { _id :: !(B.C f Int32)
  , _key :: !(B.C f Text)
  , _value :: !(B.C f Text)
  , _createdAt :: !(B.C f UTCTime)
  , _updatedAt :: !(B.C f UTCTime)
  } deriving (Generic, B.Beamable)

instance B.Table ConfigurationT where
  data PrimaryKey ConfigurationT f =
    ConfigurationPrimaryKey (B.C f Int32)
    deriving (Generic, B.Beamable)
  primaryKey = ConfigurationPrimaryKey . _id

configurationEMod ::
     Text
  -> Maybe Text
  -> B.EntityModification
       (BM.CheckedDatabaseEntity be db)
       be
       (B.TableEntity ConfigurationT)
configurationEMod tableName schema =
  modifyCheckedEntitySchema (const schema)
    <> BM.modifyCheckedTable
         (const tableName)
         BM.checkedTableModification
           { _id = "id"
           , _key = "key"
           , _value = "value"
           , _createdAt = "createdAt"
           , _updatedAt = "updatedAt"
           }
