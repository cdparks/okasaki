{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Heap.Binomial
  ( Heap(..)
  , Tree(..)
  , rank
  , root
  , link
  , insertTree
  , removeMinTree
  , mergeTree
  ) where

import Prelude

import Abstract.Heap (IsHeap(..), fromFoldable, toList)
import Common.Show (showAsList)
import Test.QuickCheck (Arbitrary(..))

data Tree a = Node {-# UNPACK #-} !Int a [Tree a]

newtype Heap a = Heap { unHeap :: [Tree a] }

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
  empty = Heap []

  isEmpty :: Ord a => Heap a -> Bool
  isEmpty = null . unHeap

  insert :: Ord a => a -> Heap a -> Heap a
  insert a = Heap . insertTree (Node 0 a []) . unHeap

  merge :: Ord a => Heap a -> Heap a -> Heap a
  merge (Heap lhs) (Heap rhs) = Heap $ mergeTree lhs rhs

  findMin :: Ord a => Heap a -> Maybe a
  findMin (Heap ts) = root . fst <$> removeMinTree ts

  deleteMin :: Ord a => Heap a -> Maybe (Heap a)
  deleteMin h = do
    (Node _ _ xs, ys) <- removeMinTree $ unHeap h
    pure $ Heap $ mergeTree (reverse xs) ys

link :: Ord a => Tree a -> Tree a -> Tree a
link lhs@(Node r x cs) rhs@(Node _ y ds)
  | x <= y = Node (r + 1) x (rhs : cs)
  | otherwise = Node (r + 1) y (lhs : ds)

insertTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insertTree x [] = [x]
insertTree x ts@(y : ys)
  | rank x < rank y = x : ts
  | otherwise = insertTree (link x y) ys

removeMinTree :: Ord a => [Tree a] -> Maybe (Tree a, [Tree a])
removeMinTree [] = Nothing
removeMinTree (x : xs) = case removeMinTree xs of
  Nothing -> Just (x, [])
  Just (y, ys)
    | root x < root y -> Just (x, xs)
    | otherwise -> Just (y, x : ys)

mergeTree :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mergeTree lhs [] = lhs
mergeTree [] rhs = rhs
mergeTree lhs@(x : xs) rhs@(y : ys)
  | leftRank < rightRank = x : mergeTree xs rhs
  | rightRank < leftRank = y : mergeTree lhs ys
  | otherwise = insertTree (link x y) $ mergeTree xs ys
 where
  leftRank = rank x
  rightRank = rank y

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ a _) = a
