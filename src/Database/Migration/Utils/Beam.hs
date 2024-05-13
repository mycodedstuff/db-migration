module Database.Migration.Utils.Beam where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Char (isUpper, toUpper)
import qualified Data.Foldable as DF
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Endo(Endo))
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import Data.Typeable (typeOf)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.Beam.Schema.Tables as BT
import Text.Read (readMaybe)

import Database.Migration.Predicate
  ( ColumnDefault(..)
  , PgHasSchema(..)
  , PgHasSequence(..)
  , TableColumnHasDefault(..)
  )
import Database.Migration.Types
import qualified Database.Migration.Types.LinkedHashMap as LHM
import Database.Migration.Utils.Common
import Database.Migration.Utils.Parser

pgEnumerationType :: T.Text -> BP.PgDataTypeSyntax
pgEnumerationType nm =
  BP.PgDataTypeSyntax
    (BP.PgDataTypeDescrDomain nm)
    (BP.emit (DTE.encodeUtf8 nm))
    (BP.pgDataTypeJSON (A.object ["customType" A..= nm]))

columnTypeToSqlType :: ColumnInfo -> T.Text
columnTypeToSqlType (VarChar cTypeInfo) = "varchar" <> mkVarcharPrec cTypeInfo
columnTypeToSqlType (Char _) = "char"
columnTypeToSqlType Integer = "int"
columnTypeToSqlType (Numeric info) = "numeric" <> mkNumericPrec info
columnTypeToSqlType Boolean = "boolean"
columnTypeToSqlType JSON = "json"
columnTypeToSqlType Double = "double"
columnTypeToSqlType Bytea = "bytea"
columnTypeToSqlType (Enum enum) = quote enum
columnTypeToSqlType BigInt = "bigint"
columnTypeToSqlType (Timestamp TimestampTypeInfo {timezone}) =
  "timestamp"
    <> if timezone
         then " with time zone"
         else ""
columnTypeToSqlType PgText = "text"
columnTypeToSqlType JSONB = "jsonb"
columnTypeToSqlType (Arr c) = columnTypeToSqlType c <> "[]"
columnTypeToSqlType Blob = "blob"
columnTypeToSqlType SmallInt = "smallint"

-- Handle collation
mkVarcharPrec :: CharTypeInfo -> T.Text
mkVarcharPrec (CharTypeInfo mPrec _) =
  T.pack
    $ case mPrec of
        Just prec -> "(" ++ show prec ++ ")"
        Nothing -> ""

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
  maybe
    "public"
    (\s ->
       if T.any isUpper s
         then quote s
         else s)
    schema
    <> "."
    <> quote tableName

{-
 Groups predicates by table/enum name
-}
groupPredicates ::
     [BM.SomeDatabasePredicate] -> LHM.LinkedHashMap T.Text DBPredicate
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
         "PgHasSequence" -> groupSequencePredicate predicate acc
         "TableColumnHasDefault" -> groupColumnDefaultPredicate predicate acc
         "PgHasSchema" -> groupPgHasSchemaPredicate predicate acc
         _ -> acc)
    LHM.empty

groupTablePredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupTablePredicate predicate acc =
  let TableInfo tableInfo = parseTableExistPredicate predicate
   in LHM.insertWith
        (\newV oldV ->
           case oldV of
             DBHasTable (TablePredicate _ preds pKey) ->
               DBHasTable (TablePredicate (TableInfo tableInfo) preds pKey)
             DBTableHasColumns preds ->
               DBHasTable (TablePredicate (TableInfo tableInfo) preds Nothing)
             _ignore -> newV)
        (mkTableName tableInfo)
        (DBHasTable (TablePredicate (TableInfo tableInfo) LHM.empty Nothing))
        acc

groupColumnPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupColumnPredicate predicate acc = do
  let TableHasColumnInfo {..} = parseTableHasColumnPredicate predicate
      columnPredicate =
        ColumnPredicate _column _table (Just _type) [] Nothing Nothing False
   in LHM.insertWith
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
        (DBTableHasColumns $ LHM.singleton _column columnPredicate)
        acc

groupConstraintPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupConstraintPredicate predicate acc = do
  let constraintInfo@ColumnConstraintInfo {_table, _column} =
        parseTableColumnConstraintPredicate predicate
      columnPredicate =
        ColumnPredicate
          _column
          _table
          Nothing
          [constraintInfo]
          Nothing
          Nothing
          False
   in LHM.insertWith
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
        (DBTableHasColumns $ LHM.singleton _column columnPredicate)
        acc

groupPrimaryKeyPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupPrimaryKeyPredicate predicate acc = do
  let pKeyInfo@(PrimaryKeyInfo table columns) =
        parseTableHasPrimaryKeyPredicate predicate
   in case columns of
        [] -> acc
        [c] ->
          let columnPredicate =
                ColumnPredicate c table Nothing [] (Just pKeyInfo) Nothing False
           in LHM.insertWith
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
                (DBTableHasColumns (LHM.singleton c columnPredicate))
                acc
        _cs ->
          LHM.insertWith
            (\newV oldV ->
               case oldV of
                 DBHasTable (TablePredicate tableInfo preds _) ->
                   DBHasTable (TablePredicate tableInfo preds (Just pKeyInfo))
                 _ignore -> newV)
            (mkTableName table)
            (DBHasTable
               (TablePredicate (TableInfo table) LHM.empty (Just pKeyInfo)))
            acc

groupEnumPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupEnumPredicate predicate acc = do
  let enumInfo@EnumInfo {name} = parsePgHasEnum predicate
   in LHM.insert name (DBHasEnum $ EnumPredicate enumInfo []) acc

groupSequencePredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupSequencePredicate predicate acc = do
  let sequencePredicate@PgHasSequence {seqName} = parsePgHasSequence predicate
   in LHM.insert
        (mkTableName seqName)
        (DBHasSequence $ SequencePredicate sequencePredicate Nothing)
        acc

groupColumnDefaultPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupColumnDefaultPredicate predicate acc = do
  let TableColumnHasDefault {..} = parseTableHasColumnDefault predicate
      columnPredicate =
        ColumnPredicate
          colName
          table
          Nothing
          []
          Nothing
          (Just defaultValue)
          False
   in LHM.insertWith
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
        (mkTableName table)
        (DBTableHasColumns $ LHM.singleton colName columnPredicate)
        acc

groupPgHasSchemaPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupPgHasSchemaPredicate predicate acc = do
  let schemaPredicate@PgHasSchema {schemaName} = parsePgHasSchema predicate
   in LHM.insert schemaName (DBHasSchema schemaPredicate) acc

upsertColumnPredicate ::
     ColumnPredicate
  -> LHM.LinkedHashMap T.Text ColumnPredicate
  -> LHM.LinkedHashMap T.Text ColumnPredicate
upsertColumnPredicate p =
  LHM.insertWith
    (\newV oldV ->
       oldV
         { columnType = columnType oldV <|> columnType newV
         , isPrimary = isPrimary oldV <|> isPrimary newV
         , columnConstraint = columnConstraint oldV ++ columnConstraint newV
         , columnExistsInDB = columnExistsInDB oldV || columnExistsInDB newV
         , columnDefault = columnDefault oldV <|> columnDefault newV
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

yesNoToBool :: String -> Bool
yesNoToBool str =
  case toUpper <$> str of
    "YES" -> True
    "NO" -> False
    _ -> False

parseColumnDefault :: T.Text -> ColumnDefault
parseColumnDefault str =
  case readMaybe $ T.unpack str of
    Just num -> LiteralInt (Just $ T.length $ T.takeWhile (== '.') str) num
    Nothing ->
      if T.isPrefixOf "nextval" str
        then Sequence str
        else LiteralStr str
