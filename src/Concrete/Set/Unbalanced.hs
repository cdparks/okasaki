{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Set.Unbalanced
  ( Set(..)
  , toList
  ) where

import Prelude

import Abstract.Set (IsSet(..), fromFoldable)
import Common.Show (showAsList)
import Test.QuickCheck (Arbitrary(..))

data Set a = Empty | Node a (Set a) (Set a)
  deriving (Eq, Ord)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance Show a => Show (Set a) where
  showsPrec = showAsList "fromFoldable" toList

instance Ord a => IsSet Set a where
  empty :: Set a
  empty = Empty

  insert :: a -> Set a -> Set a
  insert a = walk
   where
    walk Empty = Node a Empty Empty
    walk node@(Node x lhs rhs)
      | a < x = Node x (walk lhs) rhs
      | a > x = Node x lhs (walk rhs)
      | otherwise = node

  member :: a -> Set a -> Bool
  member a = walk
   where
    walk Empty = False
    walk (Node x lhs rhs)
      | a < x = walk lhs
      | a > x = walk rhs
      | otherwise = True

toList :: Set a -> [a]
toList Empty = []
toList (Node a lhs rhs) = toList lhs <> [a] <> toList rhs
