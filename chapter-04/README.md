<!--
```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import qualified Data.List as List
import Concrete.Stream (Stream(..))
import Data.Foldable (toList)
import Test.Hspec
import Test.QuickCheck
```
-->

# Chapter 4

Run `stack run chapter-04` to see test cases.

## Streams

### Exercise 4.1

> Use the fact that `force $e` is equivalent to `e` to show that the
> two implementations of `drop` are equivalent

The lazier version of `drop` is as follows:

```sml
fun lazy drop (n, s) =
  let
    fun drop' (0, s) = s
      | drop' (n, $NIL) = $NIL
      | drop' (n, $CONS (x, s)) = drop' (n - 1, s)
  in
    drop' (n, s)
  end
```

The only difference is that we're delaying the call to `drop'`. If we
force that:

```sml
fun lazy drop (n, s) =
  let
    fun drop' (0, s) = s
      | drop' (n, $NIL) = $NIL
      | drop' (n, $CONS (x, s)) = drop' (n - 1, s)
  in
    force (drop' (n, s))
  end
```

It has the same effect as _not_ delaying `drop'`. This would look like
inlining `drop'`:

```sml
fun drop (0, s) = s
  | drop (n, $NIL) = $NIL
  | drop (n, $CONS (x, s)) = drop (n - 1, s)
```

which is the original definition.

### Exercise 4.2

> Implement `insertionSort` on `Stream`s

```haskell
insert :: Ord a => a -> Stream a -> Stream a
insert x Nil = Cons x Nil
insert x sorted@(Cons y ys)
  | x < y = Cons x sorted
  | otherwise = Cons y (insert x ys)

insertionSort :: Ord a => Stream a -> Stream a
insertionSort Nil = Nil
insertionSort (Cons x xs) = insert x $ insertionSort xs
```

## Appendix

### Properties

```haskell
behavesLikeListSort
  :: forall a. Ord a
  => (Stream a -> Stream a)
  -> Stream a
  -> Bool
behavesLikeListSort sort s =
  toList (sort s) == List.sort (toList s)
```

### Main

```haskell
main :: IO ()
main = hspec $
  describe "Stream insertionSort" $
    it "behaves like List.sort" $
      property (behavesLikeListSort @Int insertionSort)
```
