module Abstract.Stream
  ( IsStream(..)
  ) where

import Prelude hiding (take, drop, reverse)

class IsStream t where
  append :: t a -> t a -> t a
  take :: Int -> t a -> t a
  drop :: Int -> t a -> t a
  reverse :: t a -> t a
