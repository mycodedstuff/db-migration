module Database.Migration.Utils.Beam where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Foldable as DF
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo(Endo))
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import Data.Typeable (typeOf)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.Beam.Schema.Tables as BT

import Database.Migration.Types
import Database.Migration.Utils.Common
import Database.Migration.Utils.Parser

pgEnumerationType :: T.Text -> BP.PgDataTypeSyntax
pgEnumerationType nm =
  BP.PgDataTypeSyntax
    (BP.PgDataTypeDescrDomain nm)
    (BP.emit (DTE.encodeUtf8 nm))
    (BP.pgDataTypeJSON (A.object ["customType" A..= nm]))

columnTypeToSqlType :: ColumnInfo -> T.Text
columnTypeToSqlType (VarChar _) = "varchar"
columnTypeToSqlType (Char _) = "char"
columnTypeToSqlType Integer = "int"
columnTypeToSqlType (Numeric info) = "numeric" <> mkNumericPrec info
columnTypeToSqlType Boolean = "boolean"
columnTypeToSqlType JSON = "json"
columnTypeToSqlType Double = "double"
columnTypeToSqlType (Bytea _) = "bytea"
columnTypeToSqlType (Enum enum) = "\"" <> enum <> "\""
columnTypeToSqlType BigInt = "bigint"
columnTypeToSqlType (Timestamp TimestampTypeInfo {timezone}) =
  "timestamp"
    <> if timezone
         then " with time zone"
         else ""

mkNumericPrec :: NumericTypeInfo -> T.Text
mkNumericPrec (NumericTypeInfo maybePrec maybeDecimal) =
  T.pack
    $ case (maybePrec, maybeDecimal) of
        (Just prec, Nothing) -> "(" ++ show prec ++ ")"
        (Just prec, Just decimal) ->
          "(" ++ show prec ++ ", " ++ show decimal ++ ")"
        _ignore -> ""

mkTableName :: BM.QualifiedName -> T.Text
mkTableName (BM.QualifiedName schema tableName) =
  maybe "public" quote schema <> "." <> quote tableName

{-
 Groups predicates by table/enum name
-}
groupPredicates :: [BM.SomeDatabasePredicate] -> Map.Map T.Text DBPredicate
groupPredicates =
  DF.foldl'
    (\acc predicate@(BM.SomeDatabasePredicate p) ->
       case show $ typeOf p of
         "TableExistsPredicate" -> groupTablePredicate predicate acc
         "TableHasColumn Postgres" -> groupColumnPredicate predicate acc
         "TableColumnHasConstraint Postgres" ->
           groupConstraintPredicate predicate acc
         "TableHasPrimaryKey" -> groupPrimaryKeyPredicate predicate acc
         "PgHasEnum" -> groupEnumPredicate predicate acc
         _ -> acc)
    Map.empty

groupTablePredicate ::
     BM.SomeDatabasePredicate
  -> Map.Map T.Text DBPredicate
  -> Map.Map T.Text DBPredicate
groupTablePredicate predicate acc =
  let TableInfo tableInfo = parseTableExistPredicate predicate
   in Map.insertWith
        (\newV oldV ->
           case oldV of
             DBHasTable (TablePredicate _ preds pKey) ->
               DBHasTable (TablePredicate (TableInfo tableInfo) preds pKey)
             DBTableHasColumns preds ->
               DBHasTable (TablePredicate (TableInfo tableInfo) preds Nothing)
             _ignore -> newV)
        (mkTableName tableInfo)
        (DBHasTable (TablePredicate (TableInfo tableInfo) mempty Nothing))
        acc

groupColumnPredicate ::
     BM.SomeDatabasePredicate
  -> Map.Map T.Text DBPredicate
  -> Map.Map T.Text DBPredicate
groupColumnPredicate predicate acc = do
  let TableHasColumnInfo {..} = parseTableHasColumnPredicate predicate
      columnPredicate =
        ColumnPredicate _column _table (Just _type) [] Nothing False
   in Map.insertWith
        (\newV oldV ->
           case oldV of
             DBHasTable (TablePredicate tableInfo preds pKey) ->
               DBHasTable
                 (TablePredicate
                    tableInfo
                    (upsertColumnPredicate columnPredicate preds)
                    pKey)
             DBTableHasColumns preds ->
               DBTableHasColumns (upsertColumnPredicate columnPredicate preds)
             _ignore -> newV)
        (mkTableName _table)
        (DBTableHasColumns $ Map.singleton _column columnPredicate)
        acc

groupConstraintPredicate ::
     BM.SomeDatabasePredicate
  -> Map.Map T.Text DBPredicate
  -> Map.Map T.Text DBPredicate
groupConstraintPredicate predicate acc = do
  let constraintInfo@ColumnConstraintInfo {_table, _column} =
        parseTableColumnConstraintPredicate predicate
      columnPredicate =
        ColumnPredicate _column _table Nothing [constraintInfo] Nothing False
   in Map.insertWith
        (\newV oldV ->
           case oldV of
             DBHasTable (TablePredicate tableInfo preds pKey) ->
               DBHasTable
                 (TablePredicate
                    tableInfo
                    (upsertColumnPredicate columnPredicate preds)
                    pKey)
             DBTableHasColumns preds ->
               DBTableHasColumns $ upsertColumnPredicate columnPredicate preds
             _ignore -> newV)
        (mkTableName _table)
        (DBTableHasColumns $ Map.singleton _column columnPredicate)
        acc

groupPrimaryKeyPredicate ::
     BM.SomeDatabasePredicate
  -> Map.Map T.Text DBPredicate
  -> Map.Map T.Text DBPredicate
groupPrimaryKeyPredicate predicate acc = do
  let pKeyInfo@(PrimaryKeyInfo table columns) =
        parseTableHasPrimaryKeyPredicate predicate
   in case columns of
        [] -> acc
        [c] ->
          let columnPredicate =
                ColumnPredicate c table Nothing [] (Just pKeyInfo) False
           in Map.insertWith
                (\newV oldV ->
                   case oldV of
                     DBHasTable (TablePredicate tableInfo preds _) ->
                       DBHasTable
                         (TablePredicate
                            tableInfo
                            (upsertColumnPredicate columnPredicate preds)
                            Nothing)
                     DBTableHasColumns preds ->
                       DBTableHasColumns
                         (upsertColumnPredicate columnPredicate preds)
                     _ignore -> newV)
                (mkTableName table)
                (DBTableHasColumns (Map.singleton c columnPredicate))
                acc
        _cs ->
          Map.insertWith
            (\newV oldV ->
               case oldV of
                 DBHasTable (TablePredicate tableInfo preds _) ->
                   DBHasTable (TablePredicate tableInfo preds (Just pKeyInfo))
                 _ignore -> newV)
            (mkTableName table)
            (DBHasTable
               (TablePredicate (TableInfo table) mempty (Just pKeyInfo)))
            acc

groupEnumPredicate ::
     BM.SomeDatabasePredicate
  -> Map.Map T.Text DBPredicate
  -> Map.Map T.Text DBPredicate
groupEnumPredicate predicate acc = do
  let enumInfo@EnumInfo {name} = parsePgHasEnum predicate
   in Map.insert name (DBHasEnum $ EnumPredicate enumInfo []) acc

upsertColumnPredicate ::
     ColumnPredicate
  -> Map.Map T.Text ColumnPredicate
  -> Map.Map T.Text ColumnPredicate
upsertColumnPredicate p =
  Map.insertWith
    (\newV oldV ->
       oldV
         { columnType = columnType oldV <|> columnType newV
         , isPrimary = isPrimary oldV <|> isPrimary newV
         , columnConstraint = columnConstraint oldV ++ columnConstraint newV
         , existsInDB = existsInDB oldV || existsInDB newV
         })
    (columnName p)
    p

constraintTypeToSqlSyntax :: ConstraintInfo -> T.Text -> T.Text
constraintTypeToSqlSyntax ConstraintInfo {..} columnName =
  case constraint of
    NOT_NULL -> "alter column " <> quote columnName <> " set not null"

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
       (\(BM.CheckedDatabaseEntity (BM.CheckedDatabaseTable desc tblChk fldChk) predicate) ->
          BM.CheckedDatabaseEntity
            (BM.CheckedDatabaseTable
               (runIdentity $ BT.dbEntitySchema (Identity . modSchema) desc)
               tblChk
               fldChk)
            predicate))
