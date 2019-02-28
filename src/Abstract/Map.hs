{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Abstract.Map
  ( IsMap(..)
  , fromFoldable
  ) where

import Prelude

class IsMap t k v | t k v -> k, t k v -> v where
  empty :: t k v
  bind :: k -> v -> t k v -> t k v
  lookup :: k -> t k v -> Maybe v

fromFoldable :: forall f t k v . (Foldable f, IsMap t k v) => f (k, v) -> t k v
fromFoldable = foldr (uncurry bind) empty
