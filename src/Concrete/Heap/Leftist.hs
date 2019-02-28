{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Heap.Leftist
  ( Heap(..)
  , toList
  , rank
  , mkNode
  ) where

import Prelude

import Abstract.Heap (IsHeap(..), fromFoldable)
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
  showsPrec d s =
    showParen (d > appPrec)
    $ showString "fromFoldable "
    . showsPrec (appPrec + 1) (toList s)
   where
    appPrec = 10

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

rank :: Heap a -> Int
rank Empty = 0
rank (Node r _ _ _) = r

mkNode :: a -> Heap a -> Heap a -> Heap a
mkNode a lhs rhs
  | leftRank < rightRank = Node (leftRank + 1) a rhs lhs
  | otherwise = Node (rightRank + 1) a lhs rhs
 where
  leftRank = rank lhs
  rightRank = rank rhs

toList :: Ord a => Heap a -> [a]
toList = unfoldr step where step h = (,) <$> findMin h <*> deleteMin h
