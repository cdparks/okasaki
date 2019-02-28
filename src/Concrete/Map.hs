{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Map
  ( Map(..)
  , toList
  ) where

import Prelude

import Abstract.Map (IsMap(..), fromFoldable)
import Test.QuickCheck (Arbitrary(..))

data Map k v = Empty | Node k v (Map k v) (Map k v)
  deriving (Eq, Ord, Show)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = fromFoldable @[] <$> arbitrary

{-
instance Show a => Show (Map a) where
  showsPrec d s =
    showParen (d > appPrec)
    $ showString "fromFoldable "
    . showsPrec (appPrec + 1) (toList s)
   where
    appPrec = 10
-}

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
