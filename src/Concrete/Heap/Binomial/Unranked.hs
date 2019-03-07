{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Heap.Binomial.Unranked
  ( Heap(..)
  , Tree(..)
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

data Tree a = Node a [Tree a]

data Ranked a = Ranked {-# UNPACK #-} !Int a

newtype Heap a = Heap { unHeap :: [Ranked (Tree a)] }

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
  insert a = Heap . insertTree (Ranked 0 $ Node a []) . unHeap

  merge :: Ord a => Heap a -> Heap a -> Heap a
  merge (Heap lhs) (Heap rhs) = Heap $ mergeTree lhs rhs

  findMin :: Ord a => Heap a -> Maybe a
  findMin (Heap ts) = root . fst <$> removeMinTree ts

  deleteMin :: Ord a => Heap a -> Maybe (Heap a)
  deleteMin h = do
    (Ranked n (Node _ xs), ys) <- removeMinTree $ unHeap h
    let decRank = Ranked $ n - 1
    pure $ Heap $ mergeTree (reverse $ decRank <$> xs) ys

link :: Ord a => Ranked (Tree a) -> Ranked (Tree a) -> Ranked (Tree a)
link (Ranked r lhs@(Node x cs)) (Ranked _ rhs@(Node y ds))
  | x <= y = incRank $ Node x (rhs : cs)
  | otherwise = incRank $ Node y (lhs : ds)
  where incRank = Ranked (r + 1)

insertTree :: Ord a => Ranked (Tree a) -> [Ranked (Tree a)] -> [Ranked (Tree a)]
insertTree x [] = [x]
insertTree x ts@(y : ys)
  | rank x < rank y = x : ts
  | otherwise = insertTree (link x y) ys

removeMinTree
  :: Ord a => [Ranked (Tree a)] -> Maybe (Ranked (Tree a), [Ranked (Tree a)])
removeMinTree [] = Nothing
removeMinTree (x : xs) = case removeMinTree xs of
  Nothing -> Just (x, [])
  Just (y, ys)
    | root x < root y -> Just (x, xs)
    | otherwise -> Just (y, x : ys)

mergeTree
  :: Ord a => [Ranked (Tree a)] -> [Ranked (Tree a)] -> [Ranked (Tree a)]
mergeTree lhs [] = lhs
mergeTree [] rhs = rhs
mergeTree lhs@(x : xs) rhs@(y : ys)
  | leftRank < rightRank = x : mergeTree xs rhs
  | rightRank < leftRank = y : mergeTree lhs ys
  | otherwise = insertTree (link x y) $ mergeTree xs ys
 where
  leftRank = rank x
  rightRank = rank y

rank :: Ranked a -> Int
rank (Ranked r _) = r

root :: Ranked (Tree a) -> a
root (Ranked _ (Node a _)) = a
