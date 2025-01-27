module Database.Migration.Types.Sequence where

import qualified Data.Aeson as A
import Data.Int (Int16, Int32)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Database.Beam as B
import qualified Database.Beam.Backend as BA
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import Database.Migration.Predicate
import Database.Migration.Utils.Beam (mkTableName)
import Database.Migration.Utils.Common

renderSequenceDefault :: T.Text -> T.Text
renderSequenceDefault str = "nextval('" <> str <> "'::regclass)"

newtype (Integral a, Bounded a) =>
        AutoIncrement a =
  AutoIncrement a
  deriving newtype (Show, Eq, A.ToJSON, A.FromJSON, Ord, Num)

instance (Typeable a, Bounded a, Integral a) =>
         B.FromBackendRow BP.Postgres (AutoIncrement a)

instance (Integral a, Bounded a, Typeable a) => Pg.FromField (AutoIncrement a) where
  fromField field mValue =
    if Pg.typeOid field
         `notElem` [Pg.typoid Pg.int4, Pg.typoid Pg.int8, Pg.typoid Pg.int2]
      then Pg.returnError Pg.Incompatible field ""
      else case mValue of
             Just d -> return $ AutoIncrement $ fromInteger $ bsToInteger d
             Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance B.HasSqlEqualityCheck BP.Postgres (AutoIncrement a)

instance (Integral a, Bounded a, BA.HasSqlValueSyntax BP.PgValueSyntax a) =>
         BA.HasSqlValueSyntax BP.PgValueSyntax (AutoIncrement a) where
  sqlValueSyntax (AutoIncrement a) = BA.sqlValueSyntax a

instance (Bounded a, Integral a) =>
         BM.HasDefaultSqlDataType BP.Postgres (AutoIncrement a) where
  defaultSqlDataType _ _ _ _ _ =
    snd $ resolveSequenceType $ toInteger $ maxBound @a
  defaultSqlDataTypeConstraints _ _ _ =
    let upperBound = toInteger $ maxBound @a
     in [ BM.FieldCheck
            (\tblName colName ->
               BM.p
                 (PgHasSequence
                    (mkSequenceName tblName colName) -- This assumes sequence is defined in same schema as table
                    (1, upperBound)
                    1
                    1
                    False
                    (fst $ resolveSequenceType upperBound)))
        , BM.FieldCheck
            (\tableName colName ->
               BM.p
                 (TableColumnHasDefault
                    tableName
                    colName
                    (Sequence
                       $ renderSequenceDefault
                       $ mkTableName
                       $ mkSequenceName tableName colName)))
        ]

resolveSequenceType ::
     Integer -> (SequenceTypes, BA.BeamSqlBackendDataTypeSyntax BP.Postgres)
resolveSequenceType upperBound
  | upperBound <= toInteger (maxBound @Int16) = (SmallInt, BA.smallIntType)
  | upperBound <= toInteger (maxBound @Int32) = (Integer, BA.intType)
  | otherwise = (BigInt, BA.bigIntType)

-- This function create sequence name
-- It has an option to ignore public schema which is used for column default as postgres drops public schema in sequence names
mkSequenceName :: BM.QualifiedName -> T.Text -> BM.QualifiedName
mkSequenceName (BM.QualifiedName mSchema tableName) colName =
  BM.QualifiedName mSchema (tableName <> "_" <> colName <> "_seq")
