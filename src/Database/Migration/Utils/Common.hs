module Database.Migration.Utils.Common where

import qualified Data.Text as T

{-# INLINE quote #-}
quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""
