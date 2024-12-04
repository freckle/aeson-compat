{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.HashMap.Strict.Compat
  ( HashMap
  , (!?)
  , alterF
  , delete
  , difference
  , elems
  , empty
  , filter
  , filterWithKey
  , foldMapWithKey
  , foldl
  , foldl'
  , foldr
  , foldr'
  , foldrWithKey
  , fromList
  , fromListWith
  , insert
  , insertWith
  , intersection
  , intersectionWith
  , intersectionWithKey
  , keys
  , lookup
  , map
  , mapKeys
  , mapMaybe
  , mapMaybeWithKey
  , mapWithKey
  , member
  , null
  , singleton
  , size
  , toList
  , traverseWithKey
  , union
  , unionWith
  , unionWithKey
  ) where

import Prelude
  ( Eq
  , Functor
  , Maybe (..)
  , Monoid
  , String
  , const
  , flip
  , maybe
  , mempty
  , ($)
  , (<$>)
  , (<>)
  )

import Data.HashMap.Strict
  ( HashMap
  , delete
  , difference
  , elems
  , empty
  , filter
  , filterWithKey
  , foldl'
  , foldr
  , foldrWithKey
  , fromList
  , fromListWith
  , insert
  , insertWith
  , intersection
  , intersectionWith
  , intersectionWithKey
  , keys
  , lookup
  , map
  , mapMaybe
  , mapMaybeWithKey
  , mapWithKey
  , member
  , null
  , singleton
  , size
  , toList
  , traverseWithKey
  , union
  , unionWith
  , unionWithKey
  )
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable, hash)

alterF
  :: (Functor f, Eq k, Hashable k)
  => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
#if MIN_VERSION_unordered_containers(0, 2, 10)
alterF = HashMap.alterF
#else
-- This is the real alterF, except that it will redundantly compute the hash for
-- the various operations, since we don't have access to the internal functions
-- that take an explicit hash argument. It's written in a weird way so it
-- requires only Functor; trying to make it readable needs Applicative.
alterF f = \ !k !m ->
  let
    mv = lookup k m
  in (<$> f mv) $ \case
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m

{-# ANN alterF ("HLint: ignore Redundant lambda" :: String) #-}
#endif

#if MIN_VERSION_unordered_containers(0, 2, 11)
foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldMapWithKey = HashMap.foldMapWithKey

foldl :: (a -> v -> a) -> a -> HashMap k v -> a
foldl = HashMap.foldl

foldr' :: (v -> a -> a) -> a -> HashMap k v -> a
foldr' = HashMap.foldr'

#else
foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldMapWithKey f = HashMap.foldrWithKey (\k v m -> m <> f k v) mempty

foldl :: (a -> v -> a) -> a -> HashMap k v -> a
foldl = HashMap.foldl' -- forgive me

foldr' :: (v -> a -> a) -> a -> HashMap k v -> a
foldr' = HashMap.foldr -- forgive me
#endif

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
#if MIN_VERSION_unordered_containers(0, 2, 14)
mapKeys = HashMap.mapKeys
#else
mapKeys f = foldMapWithKey $ \k v -> HashMap.singleton (f k) v
#endif

(!?) :: (Eq k, Hashable k) => HashMap k v -> k -> Maybe v
(!?) m k = lookup k m
{-# INLINE (!?) #-}
