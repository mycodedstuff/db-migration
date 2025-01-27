module Database.Migration.Utils.Common where

import qualified Data.Aeson as A
import Data.Bits (Bits((.|.), shiftL))
import qualified Data.ByteString as BS
import Data.Char (isUpper)
import Data.List (sortBy)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Generics.Deriving.ConNames
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as Hashable

{-# INLINE quote #-}
quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""

{-# INLINE quoteIfAnyUpper #-}
quoteIfAnyUpper :: T.Text -> T.Text
quoteIfAnyUpper t =
  if T.any isUpper t
    then quote t
    else t

{-# INLINE headMaybe #-}
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

sortArrUsingRefArr :: (Eq a,Hashable.Hashable a) => [a] -> [a] -> [a]
sortArrUsingRefArr refArr arr = do
  let arrlen = L.length refArr
  let maxBoundLimit :: Int = maxBound 
  let indexMap = HM.fromList $ zip refArr [0..]
  sortBy
    (\a b ->
       fromMaybe maxBoundLimit (HM.lookup a indexMap)
         `compare` fromMaybe maxBoundLimit (HM.lookup b indexMap)) arr

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

{-# INLINE snoc #-}
snoc :: [a] -> a -> [a]
snoc arr a = arr ++ [a]

-- Returns list of constructor names for a type
constructorNames ::
     forall a. (Generic a, ConNames (Rep a))
  => Proxy a
  -> [T.Text]
constructorNames _ = T.pack <$> conNames (undefined :: a)
