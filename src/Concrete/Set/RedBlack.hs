{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Set.RedBlack
  ( Set(..)
  , Color(..)
  , toList
  ) where

import Prelude

import Abstract.Set (IsSet(..), fromFoldable)
import Common.Show (showAsList)
import Test.QuickCheck (Arbitrary(..))

data Color = Red | Black

data Set a = Empty | Node !Color a (Set a) (Set a)

instance Eq a => Eq (Set a) where
  lhs == rhs = toList lhs == toList rhs

instance Ord a => Ord (Set a) where
  lhs `compare` rhs = toList lhs `compare` toList rhs

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance Show a => Show (Set a) where
  showsPrec = showAsList "fromFoldable" toList

instance Ord a => IsSet Set a where
  empty :: Set a
  empty = Empty

  insert :: a -> Set a -> Set a
  insert a s =
    Node Black r lhs1 rhs1
   where
    Node _ r lhs1 rhs1 = walk s
    walk Empty = Node Red a Empty Empty
    walk node@(Node color x lhs0 rhs0)
      | a < x = balance color x (walk lhs0) rhs0
      | a > x = balance color x lhs0 (walk rhs0)
      | otherwise = node

  member :: a -> Set a -> Bool
  member a = walk
   where
    walk Empty = False
    walk (Node _ x lhs rhs)
      | a < x = walk lhs
      | a > x = walk rhs
      | otherwise = True

balance :: Color -> a -> Set a -> Set a -> Set a
balance Black z (Node Red y (Node Red x a b) c) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red x a (Node Red y b c)) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance color x lhs rhs = Node color x lhs rhs

toList :: Set a -> [a]
toList Empty = []
toList (Node _ a lhs rhs) = toList lhs <> [a] <> toList rhs

{-
Node Black 4
  (Node Black 2
    (Node Black 1
      Empty
      Empty)
    (Node Black 3
      Empty
      Empty))
  (Node Black 6
    (Node Black 5
      Empty
      Empty)
    (Node Red 8
      (Node Black 7
        Empty
        Empty)
      (Node Black 9
        Empty
        (Node Red 10
          Empty
          Empty))))

[1,2,3,4,5,6,7,8,9,10]
-}
