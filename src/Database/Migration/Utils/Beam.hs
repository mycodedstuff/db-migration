module Database.Migration.Utils.Beam where

import Control.Applicative ((<|>))
import Control.Monad.Free.Church (runF)
import Control.Monad.Writer
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Data.Char (toUpper)
import qualified Data.Foldable as DF
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Strict as HM
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import Data.Typeable (typeOf)
import qualified Database.Beam as B
import qualified Database.Beam.Backend as BA
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Postgres.Syntax as BP
import qualified Database.Beam.Schema.Tables as BT
import GHC.Generics (Generic(..))
import Generics.Deriving (Enum', genumDefault)
import Lens.Micro ((%~), (^.))
import Text.Read (readMaybe)

import qualified Database.Beam.Migrate.Types as BT
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

pgEnumerationType :: Maybe T.Text -> T.Text -> BP.PgDataTypeSyntax
pgEnumerationType sch nm =
  BP.PgDataTypeSyntax
    (BP.PgDataTypeDescrDomain (BT.QualifiedName sch nm))
    (BP.emit (DTE.encodeUtf8 nm))
    (BP.pgDataTypeJSON (A.object ["customType" A..= BT.QualifiedName sch nm]))

pgDefaultEnumerationType :: BM.QualifiedName -> T.Text -> BP.PgDataTypeSyntax
pgDefaultEnumerationType (BM.QualifiedName sch tbl) col =
  pgEnumerationType sch $ mkEnumName tbl col

columnTypeToSqlType :: ColumnType -> T.Text
columnTypeToSqlType (VarChar cTypeInfo) = "varchar" <> mkVarcharPrec cTypeInfo
columnTypeToSqlType (Char cTypeInfo) = "char" <> mkVarcharPrec cTypeInfo
columnTypeToSqlType Integer = "int"
columnTypeToSqlType (Numeric info) = "numeric" <> mkNumericPrec info
columnTypeToSqlType Boolean = "boolean"
columnTypeToSqlType JSON = "json"
columnTypeToSqlType Double = "double"
columnTypeToSqlType Bytea = "bytea"
columnTypeToSqlType (Enum enum) = mkTableName enum
columnTypeToSqlType BigInt = "bigint"
columnTypeToSqlType (Timestamp TimestampTypeInfo {timezone}) =
  "timestamp"
    <> if timezone
         then " with time zone"
         else ""
columnTypeToSqlType PgText = "text"
columnTypeToSqlType JSONB = "jsonb"
columnTypeToSqlType (Arr c) = columnTypeToSqlType c <> "[]"
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
  maybe "" ((<> ".") . quoteIfAnyUpper) schema <> quoteIfAnyUpper tableName

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
         "TableHasIndex" -> groupTableHasIndexPredicate predicate acc
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
groupColumnPredicate predicate acc =
  let TableHasColumnInfo {..} = parseTableHasColumnPredicate predicate
      columnPredicate =
        ColumnPredicate _column _table (Just _type) [] Nothing Nothing Nothing
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
        $ updateColumnOfEnumType _table _column _type acc

-- This function collects the name of columns with table using enums grouped by enum name
updateColumnOfEnumType ::
     BM.QualifiedName
  -> T.Text
  -> ColumnType
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
updateColumnOfEnumType tblName colNm t@(Enum name) =
  LHM.adjust
    (\oldV ->
       case oldV of
         DBHasEnum (EnumPredicate enumInfo cols v) ->
           DBHasEnum (EnumPredicate enumInfo ((tblName, colNm, t) : cols) v)
         _ignore -> oldV)
    (mkTableName name)
updateColumnOfEnumType _ _ _ = id

groupConstraintPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupConstraintPredicate predicate acc =
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
          Nothing
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
groupPrimaryKeyPredicate predicate acc =
  let pKeyInfo@(PrimaryKeyInfo table columns) =
        parseTableHasPrimaryKeyPredicate predicate
   in case columns of
        [] -> acc
        [c] ->
          let columnPredicate =
                ColumnPredicate
                  c
                  table
                  Nothing
                  []
                  (Just pKeyInfo)
                  Nothing
                  Nothing
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
groupEnumPredicate predicate acc =
  let enumInfo@EnumInfo {name} = parsePgHasEnum predicate
      enumName = mkTableName name
   in LHM.insertWith
        (\newV oldV ->
           case oldV of
             DBHasEnum p ->
               DBHasEnum $ EnumPredicate enumInfo (dependentColumns p) []
             _ignore -> newV)
        enumName
        (DBHasEnum $ EnumPredicate enumInfo [] [])
        acc

groupSequencePredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupSequencePredicate predicate acc =
  let sequencePredicate@PgHasSequence {seqName} = parsePgHasSequence predicate
   in LHM.insert
        (mkTableName seqName)
        (DBHasSequence $ SequencePredicate sequencePredicate Nothing)
        acc

groupColumnDefaultPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupColumnDefaultPredicate predicate acc =
  let TableColumnHasDefault {..} = parseTableHasColumnDefault predicate
      columnPredicate =
        ColumnPredicate
          colName
          table
          Nothing
          []
          Nothing
          (Just defaultValue)
          Nothing
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
groupPgHasSchemaPredicate predicate acc =
  let schemaPredicate@PgHasSchema {schemaName} = parsePgHasSchema predicate
   in LHM.insert schemaName (DBHasSchema schemaPredicate) acc

groupTableHasIndexPredicate ::
     BM.SomeDatabasePredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
groupTableHasIndexPredicate predicate acc =
  let IndexPredicate {..} = parseIndexPredicate predicate
      BM.QualifiedName sch tbl  = _table
      qualifiedIndexedName = BM.QualifiedName sch _name
  in LHM.insert
        _name
        (DBTableHasIndex
           $ TableHasIndexPredicate
               _table
               qualifiedIndexedName
               _constraint
               _columns
               _predicate
               Nothing)
        acc

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
         , columnTypeInDB = columnTypeInDB oldV <|> columnTypeInDB newV
         , columnDefault = columnDefault oldV <|> columnDefault newV
         })
    (columnName p)
    p

constraintTypeToSqlSyntax :: ConstraintInfo -> T.Text -> T.Text
constraintTypeToSqlSyntax ConstraintInfo {..} columnName =
  case constraint of
    NOT_NULL -> "alter column " <> quote columnName <> " set not null"

modifyCheckedEntitySchema ::
     BT.IsDatabaseEntity be entity
  => (Maybe T.Text -> Maybe T.Text)
  -> B.EntityModification (BM.CheckedDatabaseEntity be db) be entity
modifyCheckedEntitySchema modSchema =
  BT.EntityModification
    (Endo
       (\(BM.CheckedDatabaseEntity desc predicate) ->
          BM.CheckedDatabaseEntity
            (desc & BM.unChecked . BT.dbEntitySchema %~ modSchema)
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
    Just num ->
      LiteralInt (Just $ T.length $ T.drop 1 $ T.dropWhile (/= '.') str) num
    Nothing ->
      if T.isPrefixOf "nextval" str
        then Sequence str
        else LiteralStr str

qname ::
     BT.IsDatabaseEntity be entity
  => BT.DatabaseEntityDescriptor be entity
  -> BM.QualifiedName
qname e = BM.QualifiedName (e ^. BT.dbEntitySchema) (e ^. BT.dbEntityName)

collectPartitionChecks ::
     forall db. (B.Database BP.Postgres db)
  => PartitionOption
  -> BM.CheckedDatabaseSettings BP.Postgres db
  -> [BM.SomeDatabasePredicate]
collectPartitionChecks options db =
  let details = partitionMap options
      (_ :: BM.CheckedDatabaseSettings BP.Postgres db, a) =
        runWriter
          $ BT.zipTables
              (Proxy @BP.Postgres)
              (\e@(BM.CheckedDatabaseEntity entity _ :: BM.CheckedDatabaseEntity
                     BP.Postgres
                     db
                     entityType) b -> do
                 let entityName = BM.unCheck entity ^. BT.dbEntityName
                     partitionNames =
                       fromMaybe [] $ HM.lookup entityName details
                 when (includeParentTable options || null partitionNames) $ do
                   tell $ BM.collectEntityChecks entity
                 DF.traverse_
                   (\pName ->
                      let rEntity =
                            getEntity
                              $ renameCheckedDatabaseEntity (const pName) e
                       in tell $ BM.collectEntityChecks rEntity)
                   partitionNames
                 pure b)
              db
              db
   in a

renameCheckedDatabaseEntity ::
     (T.Text -> T.Text)
  -> BM.CheckedDatabaseEntity BP.Postgres db entityType
  -> BM.CheckedDatabaseEntity BP.Postgres db entityType
renameCheckedDatabaseEntity fn =
  appEndo
    (let (BT.EntityModification endo) = BM.renameCheckedEntity fn
      in endo)

getEntity ::
     BM.CheckedDatabaseEntity BP.Postgres db entityType
  -> BM.CheckedDatabaseEntityDescriptor BP.Postgres entityType
getEntity (BM.CheckedDatabaseEntity entity _) = entity

unCheckFieldModification ::
     forall (tbl :: (Type -> Type) -> Type).
     ( BT.Beamable tbl
     , Generic (BT.TableSettings tbl)
     , BT.GDefaultTableFieldSettings (Rep (BT.TableSettings tbl) ())
     )
  => tbl (BM.CheckedFieldModification tbl)
  -> tbl (B.FieldModification (B.TableField tbl))
unCheckFieldModification fm =
  runIdentity
    $ BT.zipBeamFieldsM
        (\(BT.Columnar' (BT.CheckedFieldModification fieldMod _)) _ ->
           pure $ BT.Columnar' (BT.FieldModification fieldMod))
        fm
        fm

-- Function to define enum check on a sum type
-- This function takes Proxy a as input where a is the enum type
-- and optionally a name for enum if not present it's auto derived
-- It uses genumDefault to get all constructor values as array
-- which gets rendered using HasSqlValueSyntax instance
enumFieldCheck ::
     forall a.
     ( Generic a
     , Enum' (Rep a)
     , BA.HasSqlValueSyntax BP.PgValueSyntax a
     )
  => Proxy a
  -> Maybe T.Text
  -> BM.FieldCheck
enumFieldCheck _ mName =
  BM.FieldCheck
    (\(BM.QualifiedName mSchema tableName) colName ->
       BM.p
         (BP.PgHasEnum
            (BT.QualifiedName
               mSchema
               (fromMaybe (mkEnumName tableName colName) mName))
            $ renderSqlValueSyntax <$> genumDefault @a))

mkEnumName :: T.Text -> T.Text -> T.Text
mkEnumName tblName colName = "enum_" <> tblName <> "_" <> colName

enumFieldCheckId ::
     (Generic a, Enum' (Rep a), BA.HasSqlValueSyntax BP.PgValueSyntax a)
  => Proxy a
  -> BT.FieldCheck
enumFieldCheckId p = enumFieldCheck p Nothing

enumFieldCheckWithName ::
     (Generic a, Enum' (Rep a), BA.HasSqlValueSyntax BP.PgValueSyntax a)
  => Proxy a
  -> T.Text
  -> BT.FieldCheck
enumFieldCheckWithName p name = enumFieldCheck p (Just name)

-- Helper function to render an enum type using HasSqlValueSyntax instance
renderSqlValueSyntax :: BA.HasSqlValueSyntax BP.PgValueSyntax ty => ty -> T.Text
renderSqlValueSyntax val =
  let BP.PgValueSyntax (BP.PgSyntax syntax) = BA.sqlValueSyntax val
   in runF
        syntax
        (\_ -> error "Expecting a simple text encoding for enumeration type")
        (\case
           BP.EmitByteString "'" next -> next
           BP.EscapeString s _ -> DTE.decodeUtf8 s
           _err -> error "Expecting a simple text encoding for enumeration type")

collectTableNames ::
     forall db. (B.Database BP.Postgres db)
  => BM.CheckedDatabaseSettings BP.Postgres db
  -> [T.Text]
collectTableNames db =
  let (_ :: BM.CheckedDatabaseSettings BP.Postgres db, a) =
        runWriter
          $ BT.zipTables
              (Proxy @BP.Postgres)
              (\(BM.CheckedDatabaseEntity entity _ :: BM.CheckedDatabaseEntity
                   BP.Postgres
                   db
                   entityType) b -> do
                 tell [BM.unCheck entity ^. BT.dbEntityName]
                 pure b)
              db
              db
   in a

renameSchemaCheckedDatabaseSetting ::
     forall db. (B.Database BP.Postgres db)
  => T.Text
  -> BM.CheckedDatabaseSettings BP.Postgres db
  -> BM.CheckedDatabaseSettings BP.Postgres db
renameSchemaCheckedDatabaseSetting schema checkedDB =
  runIdentity
    $ BT.zipTables
        (Proxy @BP.Postgres)
        (\(c :: BM.CheckedDatabaseEntity BP.Postgres db entityType) _ ->
           let (BT.EntityModification endo) =
                 modifyCheckedEntitySchema (const $ Just schema)
            in pure $ appEndo endo c)
        checkedDB
        checkedDB

-- Functions to define indexes for a table
-- This function takes table name (to auto derive index name) and columns using IndexColumn GADT and returns TableIndex type used by beam
defaultIndex :: T.Text -> [IndexColumn] -> BT.TableIndex
defaultIndex tblName columns =
  BT.TableIndex
    (mkIndexName tblName columns)
    Nothing
    (getFieldName <$> columns)
    Nothing

-- Functions to define unique indexes for a table
-- This function takes table name (to auto derive index name) and columns using IndexColumn GADT and returns TableIndex type used by beam
uniqueIndex :: T.Text -> [IndexColumn] -> BT.TableIndex
uniqueIndex tblName columns =
  BT.TableIndex
    (mkIndexName tblName columns)
    (Just BT.UNIQUE)
    (getFieldName <$> columns)
    Nothing

-- Functions to define indexes for a table with a predicate/where clause
-- This function takes index name (It doesn't auto derive as same column may be used for other indexes)
-- a TableSettings which is used for where clause generation and
-- columns using IndexColumn GADT and returns TableIndex type used by beam
-- a function which return QGenExpr for defining where clause
defaultIndexWithPred ::
     BT.Beamable table
  => T.Text
  -> BT.TableSettings table
  -> [IndexColumn]
  -> (table (B.QGenExpr context BP.Postgres s) -> B.QGenExpr
                                                    context
                                                    BP.Postgres
                                                    s
                                                    bool)
  -> BT.TableIndex
defaultIndexWithPred indexName tbl columns predicate =
  BT.TableIndex indexName Nothing (getFieldName <$> columns)
    $ Just
    $ mkIndexPredicate tbl predicate

-- Functions to define unique indexes for a table with a predicate/where clause
-- This function takes index name (It doesn't auto derive as same column may be used for other indexes)
-- a TableSettings which is used for where clause generation and
-- columns using IndexColumn GADT and returns TableIndex type used by beam
-- a function which return QGenExpr for defining where clause
uniqueIndexWithPred ::
     BT.Beamable table
  => T.Text
  -> BT.TableSettings table
  -> [IndexColumn]
  -> (table (B.QGenExpr context BP.Postgres s) -> B.QGenExpr
                                                    context
                                                    BP.Postgres
                                                    s
                                                    bool)
  -> BT.TableIndex
uniqueIndexWithPred indexName tbl columns predicate =
  BT.TableIndex indexName (Just BT.UNIQUE) (getFieldName <$> columns)
    $ Just
    $ mkIndexPredicate tbl predicate

-- Helper functions for index definition
getFieldName :: IndexColumn -> T.Text
getFieldName (IC b) = b ^. B.fieldName

-- This function takes a TableSettings converts it to table (QGenExpr c be s)
-- and takes a function which defines index predicate and renders it in SQL
mkIndexPredicate ::
     BT.Beamable table
  => table (BT.TableField table)
  -> (table (B.QGenExpr context BP.Postgres s) -> B.QGenExpr
                                                    context
                                                    BP.Postgres
                                                    s
                                                    bool)
  -> T.Text
mkIndexPredicate tblFields predicateFn =
  let B.QExpr exprFn = predicateFn $ tableFieldsToExpressions tblFields
   in DTE.decodeUtf8
        $ toStrict
        $ BP.pgRenderSyntaxScript
        $ BP.fromPgExpression
        $ exprFn mempty

-- Helper function to auto derive index name using Table name and columns in index
mkIndexName :: T.Text -> [IndexColumn] -> T.Text
mkIndexName tblName columns =
  tblName <> "_" <> T.intercalate "_" (map getFieldName columns) <> "_idx"

-- Converts a TableSettings table (table (TableField table)) type to table (B.QGenExpr ctxt be s)
-- so that it can be used to construct where clause
-- This function construct where clause which ignores table alias if any using unqualifiedField function
tableFieldsToExpressions ::
     (BA.BeamSqlBackend be, BT.Beamable table)
  => BT.TableSettings table
  -> table (B.QGenExpr ctxt be s)
tableFieldsToExpressions =
  BT.changeBeamRep
    (\(BT.Columnar' f) ->
       BT.Columnar'
         (B.QExpr (const (BA.fieldE $ BA.unqualifiedField $ f ^. B.fieldName))))

-- This function compares all the fields of TableHasIndexPredicate except the index name
-- to find if an index already exists with some other name
-- The first argument takes predicates collected from database
-- Second argument is the predicate we want to find in database predicates
-- TODO: Make index lookup less expensive (Approach: derive key name based on TableHasIndexPredicate props for groupedDBPredicates)
findIndexInDBPredicates ::
     TableHasIndexPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> Maybe TableHasIndexPredicate
findIndexInDBPredicates pd groupedDBPredicates =
  let mPred =
        DF.find
          (\case
             DBTableHasIndex p ->
               indexColumns p == indexColumns pd
                 && tableName p == tableName pd
                 && indexPredicate p == indexPredicate pd
                 && indexConstraint p == indexConstraint pd
             _ignore -> False)
          $ LHM.elems groupedDBPredicates
   in case mPred of
        Just (DBTableHasIndex p) -> Just p
        _ignore -> Nothing
