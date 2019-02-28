{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Map.Unbalanced
  ( Map(..)
  , toList
  ) where

import Prelude

import Abstract.Map (IsMap(..), fromFoldable)
import Common.Show (showAsList)
import Test.QuickCheck (Arbitrary(..))

data Map k v = Empty | Node k v (Map k v) (Map k v)
  deriving (Eq, Ord)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance (Show k, Show v) => Show (Map k v) where
  showsPrec = showAsList "fromFoldable" toList

instance Ord k => IsMap Map k v where
  empty :: Map k v
  empty = Empty

  bind :: k -> v -> Map k v -> Map k v
  bind k v = walk
   where
    walk Empty = Node k v Empty Empty
    walk (Node x r lhs rhs)
      | k < x = Node x r (walk lhs) rhs
      | k > x = Node x r lhs (walk rhs)
      | otherwise = Node k v lhs rhs

  lookup :: k -> Map k v -> Maybe v
  lookup k = walk
   where
    walk Empty = Nothing
    walk (Node x r lhs rhs)
      | k < x = walk lhs
      | k > x = walk rhs
      | otherwise = Just r

toList :: Map k v -> [(k, v)]
toList Empty = []
toList (Node k v lhs rhs) = toList lhs <> [(k, v)] <> toList rhs
