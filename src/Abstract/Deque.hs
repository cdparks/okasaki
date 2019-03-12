{-# LANGUAGE ScopedTypeVariables #-}

module Abstract.Deque
  ( IsDeque(..)
  , fromFoldable
  , toList
  ) where

import Prelude hiding (head, tail, init, last)

import Data.List (unfoldr)

class IsDeque t where
  empty :: t a
  isEmpty :: t a -> Bool
  snoc :: t a -> a -> t a
  cons :: a -> t a -> t a

  uncons :: t a -> Maybe (a, t a)
  uncons q = (,) <$> head q <*> tail q

  head :: t a -> Maybe a
  head = fmap fst . uncons

  tail :: t a -> Maybe (t a)
  tail = fmap snd . uncons

  unsnoc :: t a -> Maybe (t a, a)
  unsnoc q = (,) <$> init q <*> last q

  init :: t a -> Maybe (t a)
  init = fmap fst . unsnoc

  last :: t a -> Maybe a
  last = fmap snd . unsnoc

fromFoldable :: forall f t a . (Foldable f, IsDeque t) => f a -> t a
fromFoldable = foldr cons empty

toList :: forall t a . IsDeque t => t a -> [a]
toList = unfoldr uncons
