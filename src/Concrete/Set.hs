{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Concrete.Set
  ( Set
  , toList
  ) where

import Prelude

import Abstract.Set (IsSet(..))

data Set a = Empty | Node a (Set a) (Set a)
  deriving (Eq, Ord)

instance Show a => Show (Set a) where
  showsPrec d s =
    showParen (d > appPrec)
    $ showString "fromList "
    . showsPrec (appPrec + 1) (toList s)
   where
    appPrec = 10

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
