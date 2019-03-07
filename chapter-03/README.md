<!--
```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import Abstract.Heap (IsHeap)
import qualified Abstract.Heap as Heap
import Abstract.Set (IsSet)
import qualified Abstract.Set as Set
import qualified Concrete.Heap.Leftist as Leftist
import qualified Concrete.Heap.Binomial as Binomial
import qualified Concrete.Set.RedBlack as RB
import Concrete.Set.RedBlack (Color(..))
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

Nah

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

> Prove that the maximum depth of a node in red-black tree of size _n_
> is at most _2×floor(log₂(n + 1))_

Not today

### Exercise 3.9

> Write `fromOrdList` to convert a sorted list with no duplicates into
> a red-black tree. This should run in _O(n)_ time

Eh, this looks like we're gonna have to split a list in the middle

### Exercise 3.10

> Split `balance` into two functions `lbalance` and `rbalance` to
> avoid unnecessary testing

```haskell
rbInsert :: Ord a => a -> RB.Set a -> RB.Set a
rbInsert a s =
  RB.Node RB.Black r lhs1 rhs1
 where
  RB.Node _ r lhs1 rhs1 = walk s
  walk RB.Empty = RB.Node Red a RB.Empty RB.Empty
  walk node@(RB.Node color x lhs0 rhs0)
    | a < x = lbalance color x (walk lhs0) rhs0
    | a > x = rbalance color x lhs0 (walk rhs0)
    | otherwise = node

lbalance :: Color -> a -> RB.Set a -> RB.Set a -> RB.Set a
lbalance Black z (RB.Node Red y (RB.Node Red x a b) c) d = RB.Node Red y (RB.Node Black x a b) (RB.Node Black z c d)
lbalance Black z (RB.Node Red x a (RB.Node Red y b c)) d = RB.Node Red y (RB.Node Black x a b) (RB.Node Black z c d)
lbalance color x lhs rhs = RB.Node color x lhs rhs

rbalance :: Color -> a -> RB.Set a -> RB.Set a -> RB.Set a
rbalance Black x a (RB.Node Red z (RB.Node Red y b c) d) = RB.Node Red y (RB.Node Black x a b) (RB.Node Black z c d)
rbalance Black x a (RB.Node Red y b (RB.Node Red z c d)) = RB.Node Red y (RB.Node Black x a b) (RB.Node Black z c d)
rbalance color x lhs rhs = RB.Node color x lhs rhs
```

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

behavesLikeSetInsert
  :: forall s a. (IsSet s a, Ord a, Eq (s a))
  => (a -> s a -> s a)
  -> a
  -> s a
  -> Bool
behavesLikeSetInsert f element s =
  f element s == Set.insert element s
```

### Main

```haskell
main :: IO ()
main = hspec $ do
  describe "Optimized Leaftist Heap insert" $
    it "behaves like IsHeap.insert " $
      property (behavesLikeHeapInsert @Leftist.Heap @Int insert)
  describe "Optimized Leftist Heap fromList" $
    it "behaves like IsHeap.fromFoldable" $
      property (behavesLikeHeapFromFoldable @[] @Leftist.Heap @Int fromList)
  describe "Optimized Binomial Heap findMin" $
    it "behaves like IsHeap.findMin" $
      property (behavesLikeHeapFindMin @Binomial.Heap @Int findMin)
  describe "Optimized Red-Black Set insert" $
    it "behaves like IsSet.insert" $
      property (behavesLikeSetInsert @RB.Set @Int rbInsert)
```
