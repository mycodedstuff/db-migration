{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Migration.Backend.Postgres.Render where

import Data.Char (toLower)
import qualified Data.Containers.ListUtils as LU
import qualified Data.Foldable as DF
import Data.Scientific (FPFormat(Fixed), formatScientific)
import qualified Data.Text as T
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Schema.Tables as BT

import Database.Migration.Predicate
import Database.Migration.Types
import qualified Database.Migration.Types.LinkedHashMap as LHM
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Common

mkConstraintTypeToSqlSyntax :: ColumnConstraintInfo -> T.Text
mkConstraintTypeToSqlSyntax ColumnConstraintInfo {..} =
  case constraint _constraint of
    NOT_NULL -> "not null"

mkColumnForCreateTable :: ColumnPredicate -> T.Text
mkColumnForCreateTable (ColumnPredicate name _ (Just _type) constraints maybePKey mDefault _) =
  T.intercalate " "
    $ [quote name, columnTypeToSqlType _type]
        ++ fmap mkConstraintTypeToSqlSyntax constraints
        ++ maybe [] (const ["primary key"]) maybePKey
        ++ maybe [] (\d -> ["default", mkDefault d]) mDefault
mkColumnForCreateTable (ColumnPredicate _ _ Nothing _ _ _ _) =
  error "Expected column info to exist for create table"

instance RenderPredicate BP.Postgres TablePredicate where
  mutatePredicate _ _ p = return p
  renderQuery (TablePredicate (TableInfo tableName) columnPredicates maybePKey) =
    if LHM.null columnPredicates
      then case maybePKey of
             Just PrimaryKeyInfo {..} ->
               [ "alter table "
                   <> mkTableName tableName
                   <> " add constraint "
                   <> quoteIfAnyUpper (mkPrimaryContraintName table)
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
                    (mkColumnForCreateTable <$> LHM.elems columnPredicates)
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

mkAlterColumnTypeQuery ::
     BM.QualifiedName -> T.Text -> ColumnType -> ColumnType -> T.Text
mkAlterColumnTypeQuery tableName columnName _type columnTypeInDB =
  "alter table "
    <> mkTableName tableName
    <> " alter column "
    <> quote columnName
    <> " type "
    <> columnTypeToSqlType _type
    <> maybe ";" (\q -> " " <> q <> ";") (mkColumnTypeCasting columnName _type columnTypeInDB)

instance RenderPredicate BP.Postgres ColumnPredicate where
  mutatePredicate _ dbPreds p@ColumnPredicate {columnName, columnTable} = do
    let cTypeInDB =
          case LHM.lookup (mkTableName columnTable) dbPreds of
            Just (DBHasTable (TablePredicate _ colMap _)) ->
              columnType =<< LHM.lookup columnName colMap
            _ignore -> Nothing
    return $ p {columnTypeInDB = cTypeInDB}
  renderQuery p@(ColumnPredicate columnName tableName maybeType constraints maybePKey mDefault mColumnTypeInDB) =
    case mColumnTypeInDB of
      Just cTypeInDB ->
        maybe
          []
          (\_type ->
             [mkAlterColumnTypeQuery tableName columnName _type cTypeInDB])
          maybeType
          ++ maybe
               []
               (\_default ->
                  [ T.intercalate
                      " "
                      [ "alter table"
                      , mkTableName tableName
                      , "alter column"
                      , quote columnName
                      , "set default"
                      , mkDefault _default
                      ]
                      <> ";"
                  ])
               mDefault
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
                      <> quoteIfAnyUpper (mkPrimaryContraintName table)
                      <> " primary key ("
                      <> T.intercalate ", " (quote <$> columns)
                      <> ");"
                  ])
               maybePKey
      Nothing ->
        [ "alter table "
            <> mkTableName tableName
            <> " add column if not exists "
            <> mkColumnForCreateTable p
            <> ";"
        ]

-- Constructs type casting for alter type queries with using clause
-- TODO: Add support for more type casting
mkColumnTypeCasting :: T.Text -> ColumnType -> ColumnType -> Maybe T.Text
mkColumnTypeCasting colName hType dbType =
  ("using " <>) <$> castTypes hType dbType
  where
    castTypes hT@(Enum _) (VarChar _) =
      Just $ quoteIfAnyUpper colName <> "::" <> columnTypeToSqlType hT
    castTypes hT@(Numeric _) (VarChar _) =
      Just $ quoteIfAnyUpper colName <> "::" <> columnTypeToSqlType hT
    castTypes hT@(Enum _) (Enum _) =
      Just $ quoteIfAnyUpper colName <> "::varchar::" <> columnTypeToSqlType hT
    castTypes _ _ = Nothing

mkDefault :: ColumnDefault -> T.Text
mkDefault =
  \case
    LiteralInt mScale num -> T.pack $ formatScientific Fixed mScale num
    LiteralStr str -> str
    Sequence str -> str

mkPrimaryContraintName :: BM.QualifiedName -> T.Text
mkPrimaryContraintName (BM.QualifiedName _ tableName) = tableName <> "_pkey"

instance RenderPredicate BP.Postgres EnumPredicate where
  renderQuery p@(EnumPredicate e@(EnumInfo name enums) cols existValues) =
    if null existValues
      then [mkCreateEnumQuery e]
      else let missingEnums =
                 DF.foldr
                   (\cur acc ->
                      if cur `Prelude.notElem` existValues
                        then cur : acc
                        else acc)
                   []
                   enums
            in if null missingEnums
                 then case cols of
                        [] ->
                          error
                            $ "An enum can't be reached without any column using it. Enum details: "
                                ++ show p
                        c@(_, _, Enum dbEnumName@(BM.QualifiedName sch nm)):cs ->
                          let renamedEnumName = nm <> "_renamed"
                           in [ "alter type "
                                  <> mkTableName dbEnumName
                                  <> " rename to "
                                  <> quoteIfAnyUpper renamedEnumName -- This enum can later be dropped, but choosing not to as other schemas can also use it
                                  <> ";"
                              , mkCreateEnumQuery (EnumInfo name enums)
                              ]
                                ++ map
                                     (\(tblNm, colNm, _) ->
                                        mkAlterColumnTypeQuery
                                          tblNm
                                          colNm
                                          (Enum name)
                                          (Enum
                                             $ BM.QualifiedName
                                                 sch
                                                 renamedEnumName))
                                     (c : cs)
                        _notEnum ->
                          error
                            $ "Columns aren't of enum type which shouldn't be the case: "
                                ++ show cols
                 else Prelude.map
                        (\val ->
                           "alter type "
                             <> mkTableName name
                             <> " add value if not exists '"
                             <> val
                             <> "';")
                        missingEnums
  mutatePredicate _ dbPreds p@EnumPredicate {enumInfo} = do
    let EnumInfo {name} = enumInfo
        (enumValuesInDB, dependentCols) =
          case LHM.lookup (mkTableName name) dbPreds of
            Just (DBHasEnum (EnumPredicate (EnumInfo _ dbValues) cols _)) ->
              (dbValues, cols)
            _ignore -> ([], [])
    return
      $ p
          { enumValuesInDB = enumValuesInDB
          , dependentColumns =
              LU.nubOrdOn
                (\(a, b, _) -> (a, b))
                (dependentColumns p ++ dependentCols)
          }

mkCreateEnumQuery :: EnumInfo -> T.Text
mkCreateEnumQuery (EnumInfo name enums) =
  "create type "
    <> mkTableName name
    <> " as enum ('"
    <> T.intercalate "', '" enums
    <> "');"

instance RenderPredicate BP.Postgres SequencePredicate where
  renderQuery (SequencePredicate PgHasSequence {..} exists) =
    case exists of
      Nothing ->
        [ (<> ";") . T.intercalate " "
            $ [ "create sequence if not exists"
              , mkTableName seqName
              , "as"
              , T.pack (toLower <$> show seqType)
              , "increment by"
              , T.pack (show seqStep)
              , "minvalue"
              , T.pack (show $ fst seqRange)
              , "maxvalue"
              , T.pack (show $ snd seqRange)
              , "start with"
              , T.pack (show seqOffset)
              ]
                ++ ["CYCLE" | seqCycle]
        ]
      Just (PgHasSequence _ range offset step wrap _type) ->
        (<> ";")
          . T.intercalate " "
          . (["alter sequence if exists", mkTableName seqName] ++)
          <$> [["as " <> T.pack (toLower <$> show seqType)] | _type /= seqType]
                ++ [ ["increment by " <> T.pack (show seqStep)]
                   | step /= seqStep
                   ]
                ++ [ ["minvalue " <> T.pack (show $ fst seqRange)]
                   | fst range /= fst seqRange
                   ]
                ++ [ ["maxvalue " <> T.pack (show $ snd seqRange)]
                   | snd range /= snd seqRange
                   ]
                ++ [["start " <> T.pack (show seqOffset)] | offset /= seqOffset]
                ++ [ [ if seqCycle
                         then "cycle"
                         else "no cycle"
                     ]
                   | wrap /= seqCycle
                   ]
  mutatePredicate _ dbPreds p@SequencePredicate {predicate} = do
    let PgHasSequence {seqName} = predicate
        sequenceExists =
          case LHM.lookup (mkTableName seqName) dbPreds of
            Just (DBHasSequence (SequencePredicate seqP _)) -> Just seqP
            _ignore -> Nothing
    return $ p {sequenceInDB = sequenceExists}

instance RenderPredicate BP.Postgres PgHasSchema where
  mutatePredicate _ _ p = return p
  renderQuery PgHasSchema {schemaName} =
    ["create schema if not exists " <> quoteIfAnyUpper schemaName <> ";"]

instance RenderPredicate BP.Postgres TableHasIndexPredicate where
  mutatePredicate _ dbPreds p =
    case findIndexInDBPredicates p dbPreds of
      Just tp -> return $ p {existingIndex = Just $ indexName tp}
      Nothing -> return p
  renderQuery TableHasIndexPredicate {..} = do
    let BM.QualifiedName _ ind = indexName
    case existingIndex of
      Just oldIndex -> do
        let BM.QualifiedName mSch _ = tableName
        let BM.QualifiedName _ oldInd = oldIndex
         in [ "alter index if exists "
                <> maybe mempty ((<> ".") . quoteIfAnyUpper) mSch
                <> quote oldInd
                <> " rename to "
                <> quote ind
                <> ";"
            ]
      Nothing ->
        [ "create"
            <> maybe mempty ((" " <>) . renderIndexConstraint) indexConstraint
            <> " index concurrently if not exists "
            <> quote ind
            <> " on "
            <> mkTableName tableName
            <> " (\""
            <> T.intercalate "\", \"" indexColumns
            <> "\")"
            <> maybe mempty (" where " <>) indexPredicate
            <> ";"
        ]

renderIndexConstraint :: BT.IndexConstraint -> T.Text
renderIndexConstraint BT.UNIQUE = "unique"

instance RenderPredicate BP.Postgres DBPredicate where
  renderQuery =
    \case
      DBHasTable p -> renderQuery @BP.Postgres p
      DBTableHasColumns p -> concat $ renderQuery @BP.Postgres <$> LHM.elems p
      DBHasEnum p -> renderQuery @BP.Postgres p
      DBHasSequence p -> renderQuery @BP.Postgres p
      DBHasSchema p -> renderQuery @BP.Postgres p
      DBTableHasIndex p -> renderQuery @BP.Postgres p
  mutatePredicate conn predMap =
    \case
      DBHasTable p -> DBHasTable <$> mutatePredicate @BP.Postgres conn predMap p
      DBTableHasColumns p ->
        DBTableHasColumns <$> mapM (mutatePredicate @BP.Postgres conn predMap) p
      DBHasEnum p -> DBHasEnum <$> mutatePredicate @BP.Postgres conn predMap p
      DBHasSequence p ->
        DBHasSequence <$> mutatePredicate @BP.Postgres conn predMap p
      DBHasSchema p ->
        DBHasSchema <$> mutatePredicate @BP.Postgres conn predMap p
      DBTableHasIndex p ->
        DBTableHasIndex <$> mutatePredicate @BP.Postgres conn predMap p
