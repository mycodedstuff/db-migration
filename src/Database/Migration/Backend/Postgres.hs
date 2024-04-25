module Database.Migration.Backend.Postgres where

import qualified Data.Aeson as A
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import Data.String (fromString)
import qualified Data.Text as T
import Data.Typeable (typeOf)
import qualified Data.Vector as V
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.PostgreSQL.Simple as Pg

import Database.Migration.Types
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Parser

predicateToMigrationQuery ::
     BP.Connection -> BM.SomeDatabasePredicate -> IO [String]
predicateToMigrationQuery conn predicate@(BM.SomeDatabasePredicate p) =
  let predicateType = show $ typeOf p
   in case predicateType of
        "TableHasColumn Postgres" -> do
          let TableHasColumnInfo {..} = parseTableHasColumnPredicate predicate
              BM.QualifiedName schema tableName = _table
              schemaName = maybe "public" T.unpack schema
          enumerationData :: [Pg.Only Int] <-
            Pg.query_
              conn
              (fromString
                 (Prelude.unlines
                    [ "SELECT 1 FROM information_schema.columns"
                    , "where table_schema = '" ++ schemaName ++ "'"
                    , "and table_name = '" ++ T.unpack tableName ++ "'"
                    , "and column_name = '" ++ T.unpack _column ++ "' LIMIT 1;"
                    ]))
          if null enumerationData
            then return
                   [ "alter table \""
                       ++ schemaName
                       ++ "\".\""
                       ++ T.unpack tableName
                       ++ "\" add column if not exists \""
                       ++ T.unpack _column
                       ++ "\" "
                       ++ columnTypeToSqlType _type
                       ++ ";"
                   ]
            else return
                   [ "alter table \""
                       ++ schemaName
                       ++ "\".\""
                       ++ T.unpack tableName
                       ++ "\" alter column \""
                       ++ T.unpack _column
                       ++ "\" type "
                       ++ columnTypeToSqlType _type
                       ++ ";"
                   ]
          -- return
          --   [ "alter table \""
          --       ++ schemaName
          --       ++ "\".\""
          --       ++ T.unpack tableName
          --       ++ "\" add column if not exists \""
          --       ++ T.unpack _column
          --       ++ "\" "
          --       ++ columnTypeToSqlType _type
          --       ++ ";"
          --   , "alter table \""
          --       ++ schemaName
          --       ++ "\".\""
          --       ++ T.unpack tableName
          --       ++ "\" alter column \""
          --       ++ T.unpack _column
          --       ++ "\" type "
          --       ++ columnTypeToSqlType _type
          --       ++ ";"
          --   ]
        "TableColumnHasConstraint Postgres" -> do
          let ColumnConstraintInfo {..} =
                parseTableColumnConstraintPredicate predicate
              BM.QualifiedName schema tableName = _table
              columnName = _column
              columnType = _type
              schemaName = maybe "public" T.unpack schema
              constraintType = constraint
              prefixQuery =
                "alter table \""
                  ++ schemaName
                  ++ "\".\""
                  ++ T.unpack tableName
                  ++ "\" "
          return
            [ prefixQuery
                ++ constraintTypeToSqlSyntax _constraint columnName
                ++ ";"
            ]
        "TableHasPrimaryKey" -> do
          let PrimaryKeyInfo {..} = parseTableHasPrimaryKeyPredicate predicate
              BM.QualifiedName schema tableName = table
          return
            [ "alter table \""
                ++ maybe "public" T.unpack schema
                ++ "\".\""
                ++ T.unpack tableName
                ++ "\" add primary key (\""
                ++ T.unpack (T.intercalate "\",\"" columns)
                ++ "\");"
            ]
        "TableExistsPredicate" -> do
          let (TableInfo (BM.QualifiedName schema tableName)) =
                parseTableExistPredicate predicate
          return
            [ "create table if not exists \""
                ++ maybe "public" T.unpack schema
                ++ "\".\""
                ++ T.unpack tableName
                ++ "\" ();"
            ]
        "PgHasEnum" -> do
          let (EnumInfo name values) = parsePgHasEnum predicate
          enumerationData :: [Pg.Only (V.Vector T.Text)] <-
            Pg.query_
              conn
              (fromString
                 (Prelude.unlines
                    [ "SELECT array_agg(e.enumlabel ORDER BY e.enumsortorder) as values"
                    , "FROM pg_enum e JOIN pg_type t ON t.oid = e.enumtypid"
                    , "where t.typnamespace = (select oid from pg_namespace where nspname = 'public')"
                    , "and t.typname = '" ++ T.unpack name ++ "' LIMIT 1;"
                    ]))
          case V.toList . Pg.fromOnly <$> enumerationData of
            [] ->
              return
                [ "create type \""
                    ++ T.unpack name
                    ++ "\" as enum ('"
                    ++ T.unpack (T.intercalate "', '" values)
                    ++ "');"
                ]
            [enumValues :: [T.Text]] -> do
              let missingEnums =
                    DF.foldr
                      (\cur acc ->
                         if cur `Prelude.notElem` enumValues
                           then cur : acc
                           else acc)
                      []
                      values
              return
                $ Prelude.map
                    (\val ->
                       "alter type \""
                         ++ T.unpack name
                         ++ "\" add value if not exists '"
                         ++ T.unpack val
                         ++ "';")
                    missingEnums
            _throw ->
              error
                $ "Unexpected case found multiple enum types with same name "
                    ++ T.unpack name
                    ++ " : "
                    ++ show _throw
        _ -> return [show $ BM.serializePredicate p]
