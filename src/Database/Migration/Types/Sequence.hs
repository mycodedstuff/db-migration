module Database.Migration.Types.Sequence where

import qualified Data.Aeson as A
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import qualified Data.List as L
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep)
import qualified Database.Beam as B
import qualified Database.Beam.Backend as BA
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import Database.Migration.Predicate
import Database.Migration.Utils.Beam (mkTableName)

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = L.foldl' f 0 . BS.unpack
  where
    f n w = toInteger w .|. shiftL n 8

renderSequenceDefault :: T.Text -> T.Text
renderSequenceDefault str = "nextval('" <> str <> "'::regclass)"

newtype (Integral a, Bounded a) =>
        AutoIncrement a =
  AutoIncrement a
  deriving newtype (Show, Eq, A.ToJSON, A.FromJSON, Ord)

instance (Typeable a, Bounded a, Integral a) =>
         B.FromBackendRow BP.Postgres (AutoIncrement a)

instance (Integral a, Bounded a, Typeable a) => Pg.FromField (AutoIncrement a) where
  fromField field mValue =
    let rep = (typeRep $ Proxy @a, Pg.typeOid field)
     in if rep
             `notElem` [ (typeRep (Proxy :: Proxy Int32), Pg.typoid Pg.int4)
                       , (typeRep (Proxy :: Proxy Int64), Pg.typoid Pg.int8)
                       , (typeRep (Proxy :: Proxy Int16), Pg.typoid Pg.int2)
                       ]
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
  defaultSqlDataType _ _ _ = BA.intType
  defaultSqlDataTypeConstraints _ _ _ =
    [ BM.FieldCheck
        (\tblName colName ->
           BM.p
             (PgHasSequence
                (mkSequenceName tblName colName) -- This assumes sequence is defined in same schema as table
                (1, toInteger (maxBound @a))
                1
                1
                False))
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

mkSequenceName :: BM.QualifiedName -> T.Text -> BM.QualifiedName
mkSequenceName (BM.QualifiedName mSchema tableName) colName =
  BM.QualifiedName mSchema (tableName <> "_" <> colName <> "_seq")