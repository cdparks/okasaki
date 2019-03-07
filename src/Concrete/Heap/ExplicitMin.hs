{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Heap.ExplicitMin
  ( Heap(..)
  ) where

import Prelude

import Abstract.Heap (IsHeap(..), fromFoldable, toList)
import Common.Show (showAsList)
import Control.Applicative ((<|>))
import Test.QuickCheck (Arbitrary(..))

data Heap h a
  = Empty
  | NonEmpty a (h a)

instance (IsHeap h, Ord a, Arbitrary a) => Arbitrary (Heap h a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance (IsHeap h, Ord a) => Eq (Heap h a) where
  lhs == rhs = toList lhs == toList rhs

instance (IsHeap h, Ord a) => Ord (Heap h a) where
  lhs `compare` rhs = toList lhs `compare` toList rhs

instance (IsHeap h, Ord a, Show a) => Show (Heap h a) where
  showsPrec = showAsList "fromFoldable" toList

instance IsHeap h => IsHeap (Heap h) where
  empty :: Ord a => Heap h a
  empty = Empty

  isEmpty :: Ord a => Heap h a -> Bool
  isEmpty = (== Empty)

  insert :: Ord a => a -> Heap h a -> Heap h a
  insert a Empty = NonEmpty a empty
  insert a (NonEmpty b h)
    | a < b = NonEmpty a $ insert b h
    | otherwise = NonEmpty b $ insert a h

  merge :: Ord a => Heap h a -> Heap h a -> Heap h a
  merge lhs Empty = lhs
  merge Empty rhs = rhs
  merge (NonEmpty a lhs) (NonEmpty b rhs)
    | a < b = NonEmpty a $ insert b $ lhs `merge` rhs
    | otherwise = NonEmpty b $ insert a $ lhs `merge` rhs

  findMin :: Ord a => Heap h a -> Maybe a
  findMin Empty = Nothing
  findMin (NonEmpty a _) = Just a

  deleteMin :: Ord a => Heap h a -> Maybe (Heap h a)
  deleteMin Empty = Nothing
  deleteMin (NonEmpty _ h) =
    NonEmpty <$> findMin h <*> deleteMin h <|> pure Empty
