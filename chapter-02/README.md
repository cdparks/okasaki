<!--
```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude

import qualified Abstract.Set as Set
import Concrete.Set.Unbalanced (Set(..))
import qualified Concrete.Set.Unbalanced as Unbalanced
import Control.Exception (Exception, catch, throwIO)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck
```
-->

# Chapter 2

Run `stack run chapter-02` to see test cases.


## Exercise 2.1

> Write a function `suffixes` that returns all suffixes of a list in
> decreasing order of length

```haskell
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes ls@(_:xs) = ls : suffixes xs
```

This runs in linear time because we call `suffixes` once for each element
of the list. It also uses linear space because we allocate one `(:)` for each
`(:)` cell we traverse. Visually, given an argument `xs`:

```plaintext
 xs
  \
  (:)
 /   \
1    (:)
    /   \
   2    (:)
       /   \
      3    []
```

We build a new structure `result` that looks like this:

```plaintext
 xs              result
  \                |
  (:) <---------- (:)
 /   \             |
1    (:) <------- (:)
    /   \          |
   2    (:) <---- (:)
       /   \       |
      3    [] <-- (:)
                   |
                   []
```

Each new `(:)` cell points at an extant `(:)` cell from the input list.
We're sharing!


## Exercise 2.2

> Rewrite `member` to take no more than _d + 1_ comparisons by keeping
> track of a candidate element that _might_ be equal to the query element

```haskell
member :: Ord a => a -> Set a -> Bool
member a = walk Nothing
 where
  walk Nothing Empty = False
  walk (Just candidate) Empty = candidate == a
  walk candidate (Node x lhs rhs)
    | a < x = walk candidate lhs
    | otherwise = walk (Just x) rhs
```

## Exercise 2.3

> Rewrite `insert` using exceptions to avoid copying the entire search
> path for existing elements

```haskell
data Duplicate = Duplicate
  deriving (Eq, Show)

instance Exception Duplicate

insert :: forall a. Ord a => a -> Set a -> Set a
insert a s =
  unsafePerformIO $ walk s `catch` done
 where
  done :: Duplicate -> IO (Set a)
  done Duplicate = pure s

  walk :: Set a -> IO (Set a)
  walk Empty = pure $ Node a Empty Empty
  walk (Node x lhs rhs)
    | a < x = Node x <$> walk lhs <*> pure rhs
    | a > x = Node x lhs <$> walk rhs
    | otherwise = throwIO Duplicate
{-# NOINLINE insert #-}
```

An alternative here is to use continuation passing style

```haskell
insertCPS :: forall a. Ord a => a -> Set a -> Set a
insertCPS a s = walk s id
 where
  walk Empty k = k $ Node a Empty Empty
  walk (Node x lhs rhs) k
    | a < x = walk lhs $ \t -> k $ Node x t rhs
    | a > x = walk rhs $ \t -> k $ Node x lhs t
    | otherwise = s
```

## Exercise 2.4

> Write a version of `insert` that performs no unnecessary copying and
> uses no more than _d + 1_ comparisons

I'm gonna use CPS again for this one

```haskell
insertCPS2 :: forall a. Ord a => a -> Set a -> Set a
insertCPS2 a s = walk Nothing s id
 where
  walk mCandidate Empty k
    | Just candidate <- mCandidate, candidate == a = s
    | otherwise = k $ Node a Empty Empty
  walk mCandidate (Node x lhs rhs) k
    | a < x = walk mCandidate lhs $ \t -> k $ Node x t rhs
    | otherwise = walk (Just x) rhs $ \t -> k $ Node x lhs t
```

## Exercise 2.5

### Part A

> Write a function `complete` that creates a complete binary tree
> of depth _d_ with _x_ stored in every node

```haskell
complete :: Int -> a -> Set a
complete depth x
  | depth <= 0 = Empty
  | otherwise = Node x subtree subtree
 where
  subtree = complete (depth - 1) x
```

### Part B

> Extend this function to create balanced binary trees of arbitrary
> size. These won't always be completed, but should be as balanced as
> possible

```haskell
balanced :: Int -> a -> Set a
balanced size x =
  loop size
 where
  loop n
    | n <= 0 = Empty
    | even n
    , let half = n `div` 2 = Node x (loop (half - 1)) (loop half)
    | otherwise
    , let half = (n - 1) `div` 2 = Node x (loop half) (loop half)
```

## Exercise 2.6

> Adapt the `UnbalancedSet` functor to support finite maps rather than
> finite sets.

See `src/Abstract/Map.hs` and `src/Concrete/Map.hs`.

## Appendix

### Properties

```haskell

generatesOneMoreSuffix :: forall a. [a] -> Bool
generatesOneMoreSuffix xs =
  length (suffixes xs) == length xs + 1

suffixesDecreaseInLength :: forall a. [a] -> Bool
suffixesDecreaseInLength xs =
  all decreasing $ zip ys $ drop 1 ys
 where
  ys = suffixes xs
  decreasing (lhs, rhs) = length lhs == length rhs + 1

behavesLikeSetMember
  :: forall a. Ord a
  => (a -> Set a -> Bool)
  -> a
  -> Set a
  -> Bool
behavesLikeSetMember f element s =
  f element s == Set.member element s

behavesLikeSetInsert
  :: forall a. Ord a
  => (a -> Set a -> Set a)
  -> a
  -> Set a
  -> Bool
behavesLikeSetInsert f element s =
  f element s == Set.insert element s

generatesCompleteTree :: NonNegative (Small Int) -> Bool
generatesCompleteTree (NonNegative (Small d)) =
  -- Small isn't small enough :\
  d > 15 || numElements == 2 ^ d - 1
 where
  numElements = length $ Unbalanced.toList $ complete d ()

generatesSizedTree :: NonNegative (Small Int) -> Bool
generatesSizedTree (NonNegative (Small n)) =
  n == length (Unbalanced.toList $ balanced n ())

-- TODO: a property for actually checking that the generated trees are
-- balanced. I only eyeballed some samples
```

### Main

```haskell
main :: IO ()
main = hspec $ do
  describe "Main.suffixes" $ do
    it "generates a list with n + 1 suffixes" $
      property (generatesOneMoreSuffix @Int)
    it "generates a list where each suffix decreases in length" $
      property (suffixesDecreaseInLength @Int)
  describe "Main.member" $
    it "behaves like IsSet.member" $
      property (behavesLikeSetMember @Int member)
  describe "Main.insert" $
    it "behaves like IsSet.insert " $
      property (behavesLikeSetInsert @Int insert)
  describe "Main.insertCPS" $
    it "behaves like IsSet.insert " $
      property (behavesLikeSetInsert @Int insertCPS)
  describe "Main.insertCPS2" $
    it "behaves like IsSet.insert " $
      property (behavesLikeSetInsert @Int insertCPS2)
  describe "Main.complete" $
    it "creates a tree with 2 ^ d - 1 nodes" $
      property generatesCompleteTree
  describe "Main.balanced" $
    it "creates a tree with n nodes" $
      property generatesSizedTree
```
