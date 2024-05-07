module Database.Migration.Backend.Postgres where

import qualified Data.Foldable as DF
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.PostgreSQL.Simple as Pg

import Data.Maybe (catMaybes, fromMaybe)
import Database.Migration.Types
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Common

mkConstraintTypeToSqlSyntax :: ColumnConstraintInfo -> T.Text
mkConstraintTypeToSqlSyntax ColumnConstraintInfo {..} =
  case constraint _constraint of
    NOT_NULL -> "not null"

mkColumnForCreateTable :: ColumnPredicate -> T.Text
mkColumnForCreateTable (ColumnPredicate name _ (Just _type) constraints maybePKey _) =
  T.intercalate " "
    $ [quote name, columnTypeToSqlType _type]
        ++ fmap mkConstraintTypeToSqlSyntax constraints
        ++ maybe [] (const ["primary key"]) maybePKey
mkColumnForCreateTable (ColumnPredicate _ _ Nothing _ _ _) =
  error "Expected column info to exist for create table"

instance RenderPredicate BP.Postgres TablePredicate where
  mutatePredicate _ p = return p
  renderQuery (TablePredicate (TableInfo tableName) columns maybePKey) =
    if Map.null columns
      then case maybePKey of
             Just PrimaryKeyInfo {..} ->
               [ "alter table "
                   <> mkTableName tableName
                   <> " add constraint "
                   <> quote (mkPrimaryContraintName table)
                   <> " primary key ("
                   <> T.intercalate ", " (quote <$> columns)
                   <> ");"
               ]
             Nothing ->
               ["create table if not exists " <> mkTableName tableName <> "();"]
      else [ "create table if not exists "
               <> mkTableName tableName
               <> " ("
               <> T.intercalate
                    ", "
                    (mkColumnForCreateTable <$> Map.elems columns)
               <> maybe
                    ""
                    (\(PrimaryKeyInfo _ pColumns) ->
                       "primary key ("
                         <> T.intercalate ", " (quote <$> pColumns)
                         <> ")")
                    maybePKey
               <> ");"
           ]

mkAlterSuffix :: T.Text -> ConstraintInfo -> T.Text
mkAlterSuffix columnName ConstraintInfo {..} =
  case constraint of
    NOT_NULL -> "alter column " <> quote columnName <> " set not null;"

instance RenderPredicate BP.Postgres ColumnPredicate where
  mutatePredicate conn p@ColumnPredicate {columnName, columnTable} = do
    let BM.QualifiedName maybeSchema tableName = columnTable
        schemaName = fromMaybe "public" maybeSchema
    columnExists :: [Pg.Only Int] <-
      Pg.query_
        conn
        (fromString
           (Prelude.unlines
              [ "SELECT 1 FROM information_schema.columns"
              , "where table_schema = '" ++ T.unpack schemaName ++ "'"
              , "and table_name = '" ++ T.unpack tableName ++ "'"
              , "and column_name = '" ++ T.unpack columnName ++ "' LIMIT 1;"
              ])) :: IO [Pg.Only Int]
    return $ p {existsInDB = not $ null columnExists}
  renderQuery p@(ColumnPredicate columnName tableName maybeType constraints maybePKey existsInDB) =
    if existsInDB
      then maybe
             []
             (\_type ->
                [ "alter table "
                    <> mkTableName tableName
                    <> " alter column "
                    <> quote columnName
                    <> " type "
                    <> columnTypeToSqlType _type
                    <> ";"
                ])
             maybeType
             ++ fmap
                  (\ColumnConstraintInfo {..} ->
                     T.intercalate
                       " "
                       [ "alter table"
                       , mkTableName _table
                       , mkAlterSuffix _column _constraint
                       ])
                  constraints
             ++ maybe
                  []
                  (\PrimaryKeyInfo {..} ->
                     [ "alter table "
                         <> mkTableName table
                         <> " add constraint "
                         <> quote (mkPrimaryContraintName table)
                         <> " primary key ("
                         <> T.intercalate ", " (quote <$> columns)
                         <> ");"
                     ])
                  maybePKey
      else [ "alter table "
               <> mkTableName tableName
               <> " add column if not exists "
               <> mkColumnForCreateTable p
               <> ";"
           ]

mkPrimaryContraintName :: BM.QualifiedName -> T.Text
mkPrimaryContraintName (BM.QualifiedName _ tableName) = tableName <> "_pkey"

instance RenderPredicate BP.Postgres EnumPredicate where
  renderQuery (EnumPredicate (EnumInfo name enums) existValues) =
    if null existValues
      then [ "create type "
               <> quote name
               <> " as enum ('"
               <> T.intercalate "', '" enums
               <> "');"
           ]
      else let missingEnums =
                 DF.foldr
                   (\cur acc ->
                      if cur `Prelude.notElem` existValues
                        then cur : acc
                        else acc)
                   []
                   enums
            in Prelude.map
                 (\val ->
                    "alter type "
                      <> quote name
                      <> " add value if not exists '"
                      <> val
                      <> "';")
                 missingEnums
  mutatePredicate conn p@EnumPredicate {enumInfo} = do
    let EnumInfo {name} = enumInfo
    enumerationData :: [Maybe (Pg.Only (V.Vector T.Text))] <-
      Pg.query_
        conn
        (fromString
           (Prelude.unlines
              [ "SELECT array_agg(e.enumlabel ORDER BY e.enumsortorder) as values"
              , "FROM pg_enum e JOIN pg_type t ON t.oid = e.enumtypid"
              , "where t.typnamespace = (select oid from pg_namespace where nspname = 'public')"
              , "and t.typname = '" ++ T.unpack name ++ "' LIMIT 1;"
              ]))
    case V.toList . Pg.fromOnly <$> catMaybes enumerationData of
      [] -> return p
      x:_ -> return $ p {enumValuesInDB = x}

instance RenderPredicate BP.Postgres DBPredicate where
  renderQuery =
    \case
      DBHasTable p -> renderQuery @BP.Postgres p
      DBTableHasColumns p -> concat $ renderQuery @BP.Postgres <$> p
      DBHasEnum p -> renderQuery @BP.Postgres p
  mutatePredicate conn =
    \case
      DBHasTable p -> DBHasTable <$> mutatePredicate @BP.Postgres conn p
      DBTableHasColumns p ->
        DBTableHasColumns <$> mapM (mutatePredicate @BP.Postgres conn) p
      DBHasEnum p -> DBHasEnum <$> mutatePredicate @BP.Postgres conn p
