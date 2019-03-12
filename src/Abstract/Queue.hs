{-# LANGUAGE ScopedTypeVariables #-}

module Abstract.Queue
  ( IsQueue(..)
  , fromFoldable
  , toList
  ) where

import Prelude hiding (head, tail)

import Data.List (unfoldr)

class IsQueue t where
  empty :: t a
  isEmpty :: t a -> Bool
  snoc :: t a -> a -> t a

  uncons :: t a -> Maybe (a, t a)
  uncons q = (,) <$> head q <*> tail q

  head :: t a -> Maybe a
  head = fmap fst . uncons

  tail :: t a -> Maybe (t a)
  tail = fmap snd . uncons

fromFoldable :: forall f t a . (Foldable f, IsQueue t) => f a -> t a
fromFoldable = foldl snoc empty

toList :: forall t a . IsQueue t => t a -> [a]
toList = unfoldr uncons
