{-# LANGUAGE ScopedTypeVariables #-}

module Abstract.Heap
  ( IsHeap(..)
  , fromFoldable
  , toList
  ) where

import Prelude

import Data.List (unfoldr)

class IsHeap t where
  empty :: Ord a => t a
  isEmpty :: Ord a => t a -> Bool
  insert :: Ord a => a -> t a -> t a
  merge :: Ord a => t a -> t a -> t a
  findMin :: Ord a => t a -> Maybe a
  deleteMin :: Ord a => t a -> Maybe (t a)

fromFoldable :: forall f t a . (Foldable f, IsHeap t, Ord a) => f a -> t a
fromFoldable = foldr insert empty

toList :: forall t a . (IsHeap t, Ord a) => t a -> [a]
toList = unfoldr step where step h = (,) <$> findMin h <*> deleteMin h
