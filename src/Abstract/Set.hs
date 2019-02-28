{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Abstract.Set
  ( IsSet(..)
  , fromFoldable
  ) where

import Prelude

class IsSet t a | t a -> a where
  empty :: t a
  insert :: a -> t a -> t a
  member :: a -> t a -> Bool

fromFoldable :: forall f t a . (Foldable f, IsSet t a) => f a -> t a
fromFoldable = foldr insert empty
