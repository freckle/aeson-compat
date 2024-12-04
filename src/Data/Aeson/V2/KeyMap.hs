{-# LANGUAGE CPP #-}

-- | Follows the latest @aeson-2.*@ version of "Data.Aeson.KeyMap"
module Data.Aeson.V2.KeyMap
  ( -- * @2.0.0.0@
    KeyMap
  , null
  , lookup
  , size
  , member
  , empty
  , singleton
  , insert
  , delete
  , alterF
  , difference
  , union
  , unionWith
  , unionWithKey
  , intersection
  , intersectionWith
  , intersectionWithKey
  , alignWith
  , alignWithKey
  , fromList
  , fromListWith
  , toList
  , toAscList
  , fromHashMap
  , toHashMap
  , fromHashMapText
  , toHashMapText
  , coercionToHashMap
  , fromMap
  , toMap
  , coercionToMap
  , map
  , mapKeyVal
  , traverse
  , traverseWithKey
  , foldr
  , foldr'
  , foldl
  , foldl'
  , foldMapWithKey
  , foldrWithKey
  , keys
  , filter
  , filterWithKey
  , mapMaybe
  , mapMaybeWithKey
  , Key

    -- * @2.0.2.0@
  , fromMapText
  , toMapText

    -- * @2.0.3.0@
  , elems

    -- * @2.1.0.0@
  , mapWithKey

    -- * @2.1.1.0@
  , (!?)
  , insertWith
  ) where

import Prelude
  ( Applicative
  , Eq (..)
  , Functor (..)
  , Maybe (..)
  , Monoid (..)
  , Semigroup (..)
  , error
  , id
  , ($)
  , (.)
  , (<$>)
  )

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.KeyMap
import Data.HashMap.Strict
  ( HashMap
#if !MIN_VERSION_aeson(2, 0, 3)
  , elems
#endif
#if !MIN_VERSION_aeson(2, 1, 0)
  , mapWithKey
#endif
#if !MIN_VERSION_aeson(2, 1, 1)
  , (!?)
  , insertWith
#endif
  )
#else
import Data.HashMap.Strict.Compat
import Data.Traversable (traverse)
#endif

import Data.Aeson.V2.Key (Key)
import qualified Data.Aeson.V2.Key as Key
import Data.Hashable (Hashable)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import Data.These (These (..))
import Data.Type.Coercion (Coercion (..))

#if !MIN_VERSION_aeson(2, 0, 0)
type KeyMap v = HashMap Key v

alignWith :: (These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWith f a b = f <$> align a b

-- In-line Data.Semialign instance to avoid dependency
align :: (Eq k, Hashable k) => HashMap k a -> HashMap k b -> HashMap k (These a b)
align m n = unionWith merge (map This m) (map That n)
 where
  merge (This a) (That b) = These a b
  merge _ _ = error "Align HashMap: merge"

alignWithKey :: (Key -> These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWithKey f a b =
  -- In-line Data.Semialign.Indexed instance to avoid dependency
  mapWithKey f (alignWith id a b)

toAscList :: HashMap Key v -> [(Key, v)]
toAscList = Map.toAscList . toMap

fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = id

toHashMap :: KeyMap v -> HashMap Key v
toHashMap = id

fromHashMapText :: HashMap Text v -> KeyMap v
fromHashMapText = mapKeys Key.toText

toHashMapText :: KeyMap v -> HashMap Text v
toHashMapText = mapKeys Key.fromText

coercionToHashMap :: Maybe (Coercion (HashMap Key v) (KeyMap v))
coercionToHashMap = Just Coercion

fromMap :: Map Key v -> KeyMap v
fromMap = Map.foldMapWithKey singleton

toMap :: KeyMap v -> Map Key v
toMap = foldMapWithKey Map.singleton

coercionToMap :: Maybe (Coercion (Map Key v) (KeyMap v))
coercionToMap = Nothing

mapKeyVal :: (Key -> Key) -> (v1 -> v2) -> KeyMap v1 -> KeyMap v2
mapKeyVal f g = mapKeys f . map g
#endif

#if !MIN_VERSION_aeson(2, 0, 2)
fromMapText :: Map Text v -> KeyMap v
fromMapText = fromMap . Map.mapKeys Key.fromText

toMapText :: KeyMap v -> Map Text v
toMapText = Map.mapKeys Key.toText . toMap
#endif
