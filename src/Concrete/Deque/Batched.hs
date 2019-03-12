{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Deque.Batched
  ( Deque(..)
  ) where

import Prelude

import Abstract.Deque (IsDeque(..), fromFoldable, toList)
import Common.Show (showAsList)
import Test.QuickCheck (Arbitrary(..))

data Deque a = Deque
  { _front :: [a]
  , _back :: [a]
  }

instance Show a => Show (Deque a) where
  showsPrec = showAsList "fromFoldable" toList

instance Eq a => Eq (Deque a) where
  lhs == rhs = toList lhs == toList rhs

instance Ord a => Ord (Deque a) where
  lhs `compare` rhs = toList lhs `compare` toList rhs

instance Arbitrary a => Arbitrary (Deque a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance IsDeque Deque where
  empty :: Deque a
  empty = Deque [] []

  isEmpty :: Deque a -> Bool
  isEmpty (Deque [] []) = True
  isEmpty _ = False

  cons :: a -> Deque a -> Deque a
  cons x (Deque fs bs) = fixup (x:fs) bs

  snoc :: Deque a -> a -> Deque a
  snoc (Deque fs bs) x = fixup fs (x:bs)

  uncons :: Deque a -> Maybe (a, Deque a)
  uncons (Deque [] [x]) = Just (x, Deque [] [])
  uncons (Deque [] _) = Nothing
  uncons (Deque (x:xs) bs) = Just (x, fixup xs bs)

  unsnoc :: Deque a -> Maybe (Deque a, a)
  unsnoc (Deque [x] []) = Just (Deque [] [], x)
  unsnoc (Deque _ []) = Nothing
  unsnoc (Deque fs (x:xs)) = Just (fixup fs xs, x)

fixup :: [a] -> [a] -> Deque a
fixup [] bs = Deque (reverse fs) bs2 where ~(bs2, fs) = split bs
fixup fs [] = Deque fs2 (reverse bs) where ~(fs2, bs) = split fs
fixup fs bs = Deque fs bs

split :: [a] -> ([a], [a])
split xs = loop xs xs
 where
  loop ys [] = ([], ys)
  loop (y : ys) (_ : _ : zs) = let ~(lhs, rhs) = loop ys zs in (y : lhs, rhs)
  loop (y : ys) (_ : zs) = let ~(lhs, rhs) = loop ys zs in (y : lhs, rhs)
  loop _ _ = error "impossible"
