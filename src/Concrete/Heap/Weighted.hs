{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Heap.Weighted
  ( Heap(..)
  , toList
  , weight
  , mkNode
  ) where

import Prelude

import Abstract.Heap (IsHeap(..), fromFoldable)
import Common.Show (showAsList)
import Data.List (unfoldr)
import Test.QuickCheck (Arbitrary(..))

data Heap a
  = Empty
  | Node {-# UNPACK #-} !Int a (Heap a) (Heap a)

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance Ord a => Eq (Heap a) where
  lhs == rhs = toList lhs == toList rhs

instance Ord a => Ord (Heap a) where
  lhs `compare` rhs = toList lhs `compare` toList rhs

instance (Ord a, Show a) => Show (Heap a) where
  showsPrec = showAsList "fromFoldable" toList

instance IsHeap Heap where
  empty :: Ord a => Heap a
  empty = Empty

  isEmpty :: Ord a => Heap a -> Bool
  isEmpty = (== Empty)

  insert :: Ord a => a -> Heap a -> Heap a
  insert a h = Node 1 a Empty Empty `merge` h

  merge :: Ord a => Heap a -> Heap a -> Heap a
  merge lhs Empty = lhs
  merge Empty rhs = rhs
  merge lhs@(Node _ x a b) rhs@(Node _ y c d)
    | x <= y = mkNode x a (merge b rhs)
    | otherwise = mkNode y c (merge lhs d)

  findMin :: Ord a => Heap a -> Maybe a
  findMin Empty = Nothing
  findMin (Node _ a _ _) = Just a

  deleteMin :: Ord a => Heap a -> Maybe (Heap a)
  deleteMin Empty = Nothing
  deleteMin (Node _ _ a b) = Just $ a `merge` b

weight :: Heap a -> Int
weight Empty = 0
weight (Node w _ _ _) = w

mkNode :: a -> Heap a -> Heap a -> Heap a
mkNode a lhs rhs
  | leftWeight < rightWeight = Node totalWeight a rhs lhs
  | otherwise = Node totalWeight a lhs rhs
 where
  leftWeight = weight lhs
  rightWeight = weight rhs
  totalWeight = leftWeight + rightWeight

toList :: Ord a => Heap a -> [a]
toList = unfoldr step where step h = (,) <$> findMin h <*> deleteMin h
