<!--
```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import qualified Abstract.Heap as Heap
import Concrete.Heap.Leftist (Heap(..))
import qualified Concrete.Heap.Leftist as Leftist
import Test.Hspec
import Test.QuickCheck
```
-->

# Chapter 3

Run `stack run chapter-03` to see test cases.


## Exercise 3.1

> Prove that the right spine of a leftist heap of size _n_ contains at
> most _floor(log₂(n + 1))_  elements

**TODO**

## Exercise 3.2

> Define `insert` directly rather than via a call to `merge`

```haskell
insert :: Ord a => a -> Heap a -> Heap a
insert a Empty = Node 1 a Empty Empty
insert a node@(Node _ b lhs rhs)
  | a <= b = Leftist.mkNode a Empty node
  | otherwise = Leftist.mkNode b lhs (insert a rhs)
```

## Exercise 3.3

> Implement `fromList` that creates a singleton heap for each element of
> a list and then merges them pairwise

See [Initializing a height biased leftist tree](https://en.wikipedia.org/wiki/Leftist_tree#Initializing_a_height_biased_leftist_tree)

```haskell
-- Quick queue type
data Q a = Q [a] [a]
  deriving (Functor)

fromList :: Ord a => [a] -> Heap a
fromList [] = Empty
fromList xs = loop $ one <$> Q xs []
 where
  one x = Node 1 x Empty Empty
  loop (Q (h1:h2:hs) bs) = loop $ Q hs $ h1 `Heap.merge` h2 : bs
  loop (Q [h] []) = h
  loop (Q hs bs) = loop $ Q (hs ++ reverse bs) []
```

## Exercise 3.4

See `src/Concrete/Heap/Weighted.hs`

## Appendix

### Properties

```haskell
behavesLikeHeapInsert
  :: forall a. Ord a
  => (a -> Heap a -> Heap a)
  -> a
  -> Heap a
  -> Bool
behavesLikeHeapInsert f element h =
  f element h == Heap.insert element h

behavesLikeHeapFromFoldable
  :: forall f a. (Foldable f, Ord a)
  => (f a -> Heap a)
  -> f a
  -> Bool
behavesLikeHeapFromFoldable f xs =
  f xs == Heap.fromFoldable xs
```

### Main

```haskell
main :: IO ()
main = hspec $ do
  describe "Main.insert" $
    it "behaves like IsHeap.insert " $
      property (behavesLikeHeapInsert @Int insert)
  describe "Main.fromList" $
    it "behaves like IsHeap.fromFoldable" $
      property (behavesLikeHeapFromFoldable @[] @Int fromList)
```
