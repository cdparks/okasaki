{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Queue.Batched
  ( Queue(..)
  ) where

import Prelude

import Abstract.Queue (IsQueue(..), fromFoldable, toList)
import Common.Show (showAsList)
import Test.QuickCheck (Arbitrary(..))

data Queue a = Queue
  { _front :: [a]
  , _back :: [a]
  }

instance Show a => Show (Queue a) where
  showsPrec = showAsList "fromFoldable" toList

instance Eq a => Eq (Queue a) where
  lhs == rhs = toList lhs == toList rhs

instance Ord a => Ord (Queue a) where
  lhs `compare` rhs = toList lhs `compare` toList rhs

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance IsQueue Queue where
  empty :: Queue a
  empty = Queue [] []

  isEmpty :: Queue a -> Bool
  isEmpty (Queue [] []) = True
  isEmpty _ = False

  snoc :: Queue a -> a -> Queue a
  snoc (Queue fs bs) x = fixup fs $ x:bs

  uncons :: Queue a -> Maybe (a, Queue a)
  uncons (Queue [] _) = Nothing
  uncons (Queue (x:xs) bs) = Just (x, fixup xs bs)

fixup :: [a] -> [a] -> Queue a
fixup [] bs = Queue (reverse bs) []
fixup fs bs = Queue fs bs
