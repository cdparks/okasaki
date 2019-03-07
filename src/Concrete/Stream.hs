{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Stream
  ( Stream(..)
  , fromFoldable
  ) where

import Prelude hiding (take, drop, reverse)

import Abstract.Stream (IsStream(..))
import Common.Show (showAsList)
import Data.Foldable (toList)
import Test.QuickCheck (Arbitrary(..))

data Stream a
  = Nil
  | Cons a (Stream a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Stream a) where
  showsPrec = showAsList "fromFoldable" toList

instance Arbitrary a => Arbitrary (Stream a) where
  arbitrary = fromFoldable @[] <$> arbitrary

instance IsStream Stream where
  append :: Stream a -> Stream a -> Stream a
  append Nil ys = ys
  append (Cons x xs) ys = Cons x $ append xs ys

  take :: Int -> Stream a -> Stream a
  take _ Nil = Nil
  take n ~(Cons x xs)
    | n <= 0 = Nil
    | otherwise = Cons x $ take (n - 1) xs

  drop :: Int -> Stream a -> Stream a
  drop _ Nil = Nil
  drop n ~ys@(Cons _ xs)
    | n <= 0 = ys
    | otherwise = drop (n - 1) xs

  reverse :: Stream a -> Stream a
  reverse = loop Nil
   where
    loop acc Nil = acc
    loop acc (Cons x xs) = loop (Cons x acc) xs

fromFoldable :: Foldable f => f a -> Stream a
fromFoldable = foldr Cons Nil
