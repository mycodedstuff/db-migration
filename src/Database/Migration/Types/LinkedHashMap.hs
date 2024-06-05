module Database.Migration.Types.LinkedHashMap where

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List (foldl')
import qualified Data.Sequence as DS
import GHC.Generics (Generic)
import Prelude hiding (map, seq)

data LinkedHashMap k v =
  LinkedHashMap !(HM.HashMap k Int) !(DS.Seq v)
  deriving (Generic, Show, Eq, Ord)

instance Foldable (LinkedHashMap k) where
  foldr f b0 (LinkedHashMap _ s) = foldr f b0 s

instance Functor (LinkedHashMap k) where
  fmap = map

instance Traversable (LinkedHashMap k) where
  traverse f (LinkedHashMap hMap seq) = LinkedHashMap hMap <$> traverse f seq

lookup :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> Maybe v
lookup key (LinkedHashMap hMap seq) = flip DS.lookup seq =<< HM.lookup key hMap

singleton :: Hashable k => k -> v -> LinkedHashMap k v
singleton k v = LinkedHashMap (HM.singleton k 0) (DS.singleton v)

insert :: (Eq k, Hashable k) => k -> v -> LinkedHashMap k v -> LinkedHashMap k v
insert k v (LinkedHashMap hMap seq) =
  case HM.lookup k hMap of
    Just i -> LinkedHashMap hMap (DS.update i v seq)
    Nothing -> LinkedHashMap (HM.insert k (DS.length seq) hMap) (seq DS.|> v)

insertWith ::
     (Eq k, Hashable k, Show k)
  => (v -> v -> v)
  -> k
  -> v
  -> LinkedHashMap k v
  -> LinkedHashMap k v
insertWith f k v (LinkedHashMap hMap seq) =
  case HM.lookup k hMap of
    Just i ->
      let oldV =
            case DS.lookup i seq of
              Just x -> x
              Nothing -> error $ "LinkedHashMap: lookup failed " ++ show k -- This path should be unreachable
          newV = f v oldV
       in LinkedHashMap hMap (DS.update i newV seq)
    Nothing -> LinkedHashMap (HM.insert k (DS.length seq) hMap) (seq DS.|> v)

elems :: LinkedHashMap k v -> [v]
elems (LinkedHashMap _ seq) = toList seq

empty :: LinkedHashMap k v
empty = LinkedHashMap HM.empty DS.empty

null :: LinkedHashMap k v -> Bool
null (LinkedHashMap hMap _) = HM.null hMap

fromList :: (Eq k, Hashable k) => [(k, v)] -> LinkedHashMap k v
fromList list =
  fst
    $ foldl'
        (\(LinkedHashMap hMap seq, i) (k, v) ->
           (LinkedHashMap (HM.insert k i hMap) (seq DS.|> v), i + 1))
        (LinkedHashMap HM.empty DS.empty, 0)
        list

map :: (a -> b) -> LinkedHashMap k a -> LinkedHashMap k b
map f (LinkedHashMap hMap seq) = LinkedHashMap hMap $ f <$> seq


adjust :: (Eq k, Hashable k) => (v -> v) -> k -> LinkedHashMap k v -> LinkedHashMap k v
adjust fn key (LinkedHashMap hMap seq) =
  case HM.lookup key hMap of
    Nothing -> LinkedHashMap hMap seq
    Just idx -> LinkedHashMap hMap $ DS.adjust' fn idx seq
