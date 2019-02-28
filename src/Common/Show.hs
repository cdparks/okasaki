module Common.Show
  ( showAsList
  ) where

import Prelude

showAsList :: Show a => String -> (x -> [a]) -> Int -> x -> ShowS
showAsList constructor toList d x =
  showParen (d > appPrec) $ showString constructor . showChar ' ' . showsPrec
    (appPrec + 1)
    (toList x)
  where appPrec = 10
