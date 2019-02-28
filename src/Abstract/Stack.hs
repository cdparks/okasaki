module Abstract.Stack
  ( IsStack(..)
  , head
  , tail
  , isEmpty
  , fromFoldable
  , toList
  ) where

import Prelude hiding (head, tail)

import Data.Maybe (isNothing)

class IsStack t where
  cons :: a -> t a -> t a
  empty :: t a
  uncons :: t a -> Maybe (a, t a)

head :: IsStack t => t a -> Maybe a
head = fmap fst . uncons

tail :: IsStack t => t a -> Maybe (t a)
tail = fmap snd . uncons

isEmpty :: IsStack t => t a -> Bool
isEmpty = isNothing . uncons

fromFoldable :: (Foldable f, IsStack t) => f a -> t a
fromFoldable = foldr cons empty

toList :: IsStack t => t a -> [a]
toList x = case uncons x of
  Nothing -> []
  Just (h, t) -> h : toList t
