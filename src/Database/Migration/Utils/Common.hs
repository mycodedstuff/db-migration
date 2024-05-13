module Database.Migration.Utils.Common where

import qualified Data.Aeson as A
import Data.Bits (Bits((.|.), shiftL))
import qualified Data.ByteString as BS
import Data.List (elemIndex, sortBy)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

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

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = L.foldl' f 0 . BS.unpack
  where
    f n w = toInteger w .|. shiftL n 8

fromResult :: (String -> a) -> A.Result a -> a
fromResult defaultFn =
  \case
    A.Success a -> a
    A.Error err -> defaultFn err
