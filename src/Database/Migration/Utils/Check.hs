module Database.Migration.Utils.Check where

import qualified Data.Foldable as DF
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import qualified Data.List as DL
import Database.Migration.Types
import qualified Database.Migration.Types.LinkedHashMap as LHM
import Database.Migration.Utils.Beam

lenientPredicateCheck ::
     Options
  -> DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> Maybe DBPredicate
lenientPredicateCheck Options {typeLenient} pd@(DBTableHasColumns colPreds) groupedDBPredicates =
  case typeLenient of
    Nothing -> Just pd
    Just lenientTypeCheck ->
      let lenientPreds =
            DF.foldl'
              (\acc p@ColumnPredicate {columnName} ->
                 if lenientColumnPredicateCheck
                      lenientTypeCheck
                      p
                      groupedDBPredicates
                   then acc
                   else LHM.insert columnName p acc)
              LHM.empty
              colPreds
       in if LHM.null lenientPreds
            then Nothing
            else Just $ DBTableHasColumns lenientPreds
lenientPredicateCheck Options {ignoreEnumOrder} pd@(DBHasEnum (EnumPredicate (EnumInfo enumName enumValues) _ _)) groupedDBPredicates =
  let valuesInDB =
        case LHM.lookup (mkTableName enumName) groupedDBPredicates of
          Just (DBHasEnum (EnumPredicate enumInfo _ _)) -> values enumInfo
          _ignore -> []
   in if ignoreEnumOrder && DL.sort enumValues == DL.sort valuesInDB
        then Nothing
        else Just pd
lenientPredicateCheck _ p _ = Just p

lenientColumnPredicateCheck ::
     ColumnTypeCheck
  -> ColumnPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> Bool
lenientColumnPredicateCheck lenientTypeCheck p groupedDBPredicates = do
  let cTable@(BM.QualifiedName _ tblName) = columnTable p
      tableName = mkTableName cTable
      colName = columnName p
  fromMaybe False $ do
    dbP <- LHM.lookup tableName groupedDBPredicates
    case dbP of
      DBHasTable (TablePredicate _ dbColPreds _) -> do
        dbCType <- columnType =<< LHM.lookup colName dbColPreds
        cType <- columnType p
        return $ lenientTypeCheck tblName colName cType dbCType
      _ignore -> Nothing
