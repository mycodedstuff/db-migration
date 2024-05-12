{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Migration.Backend.Postgres where

import qualified Data.Foldable as DF
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Database.Beam.Migrate.Types as BM
import qualified Database.Beam.Postgres as BP
import Data.Scientific (FPFormat(Fixed), formatScientific)

import Database.Migration.Predicate
import Database.Migration.Types
import Database.Migration.Utils.Beam
import Database.Migration.Utils.Common
import qualified Database.Migration.Types.LinkedHashMap as LHM

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

instance RenderPredicate BP.Postgres ColumnPredicate where
  mutatePredicate _ dbPreds p@ColumnPredicate {columnName, columnTable} = do
    let columnExists =
          case LHM.lookup (mkTableName columnTable) dbPreds of
            Just (DBHasTable (TablePredicate _ colMap _)) ->
              isJust $ LHM.lookup columnName colMap
            _ignore -> False
    return $ p {columnExistsInDB = columnExists}
  renderQuery p@(ColumnPredicate columnName tableName maybeType constraints maybePKey mDefault existsInDB) =
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

mkDefault :: ColumnDefault -> T.Text
mkDefault =
  \case
    LiteralInt mScale num -> T.pack $ formatScientific Fixed mScale num
    LiteralStr str -> str
    Sequence str -> str

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
  mutatePredicate _ dbPreds p@EnumPredicate {enumInfo} = do
    let EnumInfo {name} = enumInfo
        enumValuesInDB =
          case LHM.lookup name dbPreds of
            Just (DBHasEnum (EnumPredicate _ dbValues)) -> dbValues
            _ignore -> []
    return $ p {enumValuesInDB = enumValuesInDB}

instance RenderPredicate BP.Postgres SequencePredicate where
  renderQuery (SequencePredicate PgHasSequence {..} exists) =
    case exists of
      Nothing ->
        [ (<> ";") . T.intercalate " "
            $ [ "create sequence if not exists"
              , mkTableName seqName
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
      Just (PgHasSequence _ range offset step wrap) ->
        (<> ";")
          . T.intercalate " "
          . (["alter sequence if exists", mkTableName seqName] ++)
          <$> [ ["increment by " <> T.pack (show step) | step /= seqStep]
              , [ "minvalue " <> T.pack (show $ fst range)
                | fst range /= fst seqRange
                ]
              , [ "maxvalue " <> T.pack (show $ snd range)
                | snd range /= snd seqRange
                ]
              , ["start " <> T.pack (show offset) | offset /= seqOffset]
              , [ if wrap
                  then "cycle"
                  else "no cycle"
                | wrap /= seqCycle
                ]
              ]
  mutatePredicate _ dbPreds p@SequencePredicate {predicate} = do
    let PgHasSequence {seqName} = predicate
        sequenceExists =
          case LHM.lookup (mkTableName seqName) dbPreds of
            Just (DBHasSequence (SequencePredicate seqP _)) -> Just seqP
            _ignore -> Nothing
    return $ p {sequenceInDB = sequenceExists}

instance RenderPredicate BP.Postgres DBPredicate where
  renderQuery =
    \case
      DBHasTable p -> renderQuery @BP.Postgres p
      DBTableHasColumns p -> concat $ renderQuery @BP.Postgres <$> LHM.elems p
      DBHasEnum p -> renderQuery @BP.Postgres p
      DBHasSequence p -> renderQuery @BP.Postgres p
  mutatePredicate conn predMap =
    \case
      DBHasTable p -> DBHasTable <$> mutatePredicate @BP.Postgres conn predMap p
      DBTableHasColumns p ->
        DBTableHasColumns <$> mapM (mutatePredicate @BP.Postgres conn predMap) p
      DBHasEnum p -> DBHasEnum <$> mutatePredicate @BP.Postgres conn predMap p
      DBHasSequence p ->
        DBHasSequence <$> mutatePredicate @BP.Postgres conn predMap p
