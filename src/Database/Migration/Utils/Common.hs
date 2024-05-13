module Database.Migration.Utils.Common where

import qualified Data.Text as T
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromMaybe)

{-# INLINE quote #-}
quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""

{-# INLINE headMaybe #-}
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

sortArrUsingRefArr :: Eq a => [a] -> [a] -> [a]
sortArrUsingRefArr refArr =
  sortBy
    (\a b ->
       fromMaybe maxBound (elemIndex a refArr)
         `compare` fromMaybe maxBound (elemIndex b refArr))

