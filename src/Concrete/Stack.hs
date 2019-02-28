{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Concrete.Stack
  ( Stack(..)
  )
where

import Prelude hiding (head, tail)

import Abstract.Stack (IsStack(..))

newtype Stack a = Stack { getStack :: [a] }
  deriving newtype (Eq, Ord)
  deriving stock (Show)

instance IsStack Stack where
  empty :: Stack a
  empty = Stack []

  cons :: a -> Stack a -> Stack a
  cons x (Stack xs) = Stack $ x : xs

  uncons :: Stack a -> Maybe (a, Stack a)
  uncons (Stack xs) = case xs of
    [] -> Nothing
    y:ys -> Just (y, Stack ys)
