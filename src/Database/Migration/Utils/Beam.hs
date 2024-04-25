module Database.Migration.Utils.Beam where

import qualified Data.Aeson as A
import qualified Data.Foldable as DF
import Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe as DM
import Data.Monoid (Endo(Endo))
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Data.Tree as DT
import Data.Typeable (typeOf)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.Beam.Schema.Tables as BT

import Database.Migration.Types
import Database.Migration.Utils.Parser

pgEnumerationType :: T.Text -> BP.PgDataTypeSyntax
pgEnumerationType nm =
  BP.PgDataTypeSyntax
    (BP.PgDataTypeDescrDomain nm)
    (BP.emit (DTE.encodeUtf8 nm))
    (BP.pgDataTypeJSON (A.object ["customType" A..= nm]))

columnTypeToSqlType :: ColumnInfo -> String
columnTypeToSqlType (VarChar _) = "varchar"
columnTypeToSqlType (Char _) = "char"
columnTypeToSqlType Integer = "int"
columnTypeToSqlType (Numeric info) = "numeric" ++ mkNumericPrec info
columnTypeToSqlType Boolean = "boolean"
columnTypeToSqlType JSON = "json"
columnTypeToSqlType Double = "double"
columnTypeToSqlType (Bytea _) = "bytea"
columnTypeToSqlType (Enum enum) = "\"" ++ T.unpack enum ++ "\""
columnTypeToSqlType BigInt = "bigint"
columnTypeToSqlType (Timestamp TimestampTypeInfo {timezone}) =
  "timestamp"
    ++ if timezone
         then " with time zone"
         else ""

mkNumericPrec :: NumericTypeInfo -> String
mkNumericPrec (NumericTypeInfo prec decimal) =
  case (prec, decimal) of
    (Just prec, Nothing) -> "(" ++ show prec ++ ")"
    (Just prec, Just decimal) -> "(" ++ show prec ++ ", " ++ show decimal ++ ")"
    _ignore -> ""

mkTableName :: BM.QualifiedName -> T.Text
mkTableName (BM.QualifiedName schema tableName) =
  DM.fromMaybe "public" schema <> "." <> tableName

constructDBTree ::
     HM.HashMap
       T.Text
       (Maybe BM.SomeDatabasePredicate, [BM.SomeDatabasePredicate])
  -> DBTree
constructDBTree predicateMap =
  let rootNode = DT.Node (TreeNode "DB" Nothing) []
      forest =
        fmap
          (\(tableName, (predicate, predicates)) ->
             DT.Node (TreeNode tableName predicate)
               $ fmap (\p -> DT.Node (Leaf p) []) predicates)
          (HM.toList predicateMap)
   in DT.Node (TreeNode "DB" Nothing) forest

{-
 Groups predicates by table name
 -- Note: This can be further grouped by column
-}
groupByTableName ::
     [BM.SomeDatabasePredicate]
  -> HM.HashMap
       T.Text
       (Maybe BM.SomeDatabasePredicate, [BM.SomeDatabasePredicate])
groupByTableName =
  DF.foldl'
    (\acc predicate@(BM.SomeDatabasePredicate p) ->
       let (key, value) =
             case show $ typeOf p of
               "TableExistsPredicate" ->
                 let TableInfo tableInfo = parseTableExistPredicate predicate
                  in (Just $ mkTableName tableInfo, (Just predicate, []))
               "TableHasColumn Postgres" ->
                 let TableHasColumnInfo {_table} =
                       parseTableHasColumnPredicate predicate
                  in (Just $ mkTableName _table, (Nothing, [predicate]))
               "TableColumnHasConstraint Postgres" ->
                 let ColumnConstraintInfo {_table} =
                       parseTableColumnConstraintPredicate predicate
                  in (Just $ mkTableName _table, (Nothing, [predicate]))
               "TableHasPrimaryKey" ->
                 let PrimaryKeyInfo {table} =
                       parseTableHasPrimaryKeyPredicate predicate
                  in (Just $ mkTableName table, (Nothing, [predicate]))
               _ -> (Nothing, (Nothing, []))
        in case (key, flip HM.lookup acc =<< key) of
             (Just key, Just _) ->
               HM.insertWith
                 (\(_, ndepPred) (pred, depPred) -> (pred, depPred ++ ndepPred))
                 key
                 value
                 acc
             (Just key, Nothing) -> HM.insert key value acc
             _ignore -> acc)
    HM.empty

constraintTypeToSqlSyntax :: ConstraintInfo -> T.Text -> String
constraintTypeToSqlSyntax ConstraintInfo {..} columnName =
  case constraint of
    NOT_NULL -> "alter column \"" ++ T.unpack columnName ++ "\" set not null"
    UNIQUE ->
      "add"
        ++ maybe "" (" constraint " ++) name
        ++ " unique (\""
        ++ T.unpack columnName
        ++ "\")"
    PRIMARY_KEY -> "add primary key (\"" ++ T.unpack columnName ++ "\")"

modifyCheckedEntitySchema ::
     BT.IsDatabaseEntity be (B.TableEntity tbl)
  => (Maybe T.Text -> Maybe T.Text)
  -> B.EntityModification
       (BM.CheckedDatabaseEntity be db)
       be
       (B.TableEntity tbl)
modifyCheckedEntitySchema modSchema =
  BT.EntityModification
    (Endo
       (\(BM.CheckedDatabaseEntity (BM.CheckedDatabaseTable desc tblChk fldChk) pred) ->
          BM.CheckedDatabaseEntity
            (BM.CheckedDatabaseTable
               (runIdentity $ BT.dbEntitySchema (Identity . modSchema) desc)
               tblChk
               fldChk)
            pred))
