<!--
```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import Abstract.Heap (IsHeap)
import qualified Abstract.Heap as Heap
import qualified Concrete.Heap.Leftist as Leftist
import qualified Concrete.Heap.Binomial as Binomial
import Control.Applicative ((<|>))
import Test.Hspec
import Test.QuickCheck
```
-->

# Chapter 3

Run `stack run chapter-03` to see test cases.


## Leftist Heaps

### Exercise 3.1

> Prove that the right spine of a leftist heap of size _n_ contains at
> most _floor(log₂(n + 1))_  elements

**TODO**

### Exercise 3.2

> Define `insert` directly rather than via a call to `merge`

```haskell
insert :: Ord a => a -> Leftist.Heap a -> Leftist.Heap a
insert a Leftist.Empty = Leftist.Node 1 a Leftist.Empty Leftist.Empty
insert a node@(Leftist.Node _ b lhs rhs)
  | a <= b = Leftist.mkNode a Leftist.Empty node
  | otherwise = Leftist.mkNode b lhs (insert a rhs)
```

### Exercise 3.3

> Implement `fromList` that creates a singleton heap for each element of
> a list and then merges them pairwise

See [Initializing a height biased leftist tree](https://en.wikipedia.org/wiki/Leftist_tree#Initializing_a_height_biased_leftist_tree)

```haskell
-- Quick queue type
data Q a = Q [a] [a]
  deriving (Functor)

fromList :: Ord a => [a] -> Leftist.Heap a
fromList [] = Leftist.Empty
fromList xs = loop $ one <$> Q xs []
 where
  one x = Leftist.Node 1 x Leftist.Empty Leftist.Empty
  loop (Q (h1:h2:hs) bs) = loop $ Q hs $ h1 `Heap.merge` h2 : bs
  loop (Q [h] []) = h
  loop (Q hs bs) = loop $ Q (hs ++ reverse bs) []
```

### Exercise 3.4

See `src/Concrete/Heap/Weighted.hs`

## Binomial Heaps

### Exercise 3.5

> Define `findMin` directly rather than via a call to `removeMinTree`

```haskell
findMin :: Ord a => Binomial.Heap a -> Maybe a
findMin (Binomial.Heap ts) = loop ts
 where
  loop [] = Nothing
  loop (Binomial.Node _ x _: xs) = min x <$> loop xs <|> pure x
```

### Exercise 3.6

> Reimplement binomial heaps without redundant rank annotations

See `src/Concrete/Heap/Binomial/Unranked.hs`

### Exercise 3.7

> Implement `ExplicitMin` so that `findMin` takes _O(1)_ and
> `insert`, `merge`, and `deleteMin` take _O(log₂(n))_

See `src/Concrete/Heap/ExplicitMin.hs`

## Red-Black Trees

### Exercise 3.8

### Exercise 3.9

### Exercise 3.10

## Appendix

### Properties

```haskell
behavesLikeHeapInsert
  :: forall h a. (IsHeap h, Ord a, Eq (h a))
  => (a -> h a -> h a)
  -> a
  -> h a
  -> Bool
behavesLikeHeapInsert f element h =
  f element h == Heap.insert element h

behavesLikeHeapFromFoldable
  :: forall f h a. (Foldable f, IsHeap h, Ord a, Eq (h a))
  => (f a -> h a)
  -> f a
  -> Bool
behavesLikeHeapFromFoldable f xs =
  f xs == Heap.fromFoldable xs

behavesLikeHeapFindMin
  :: forall h a. (IsHeap h, Ord a, Eq (h a))
  => (h a -> Maybe a)
  -> h a
  -> Bool
behavesLikeHeapFindMin f h =
  f h == Heap.findMin h
```

### Main

```haskell
main :: IO ()
main = hspec $ do
  describe "Main.insert" $
    it "behaves like IsHeap.insert " $
      property (behavesLikeHeapInsert @Leftist.Heap @Int insert)
  describe "Main.fromList" $
    it "behaves like IsHeap.fromFoldable" $
      property (behavesLikeHeapFromFoldable @[] @Leftist.Heap @Int fromList)
  describe "Main.findMin" $
    it "behaves like IsHeap.findMin" $
      property (behavesLikeHeapFindMin @Binomial.Heap @Int findMin)
```
