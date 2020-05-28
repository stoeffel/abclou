module Data.Functor.Extra
  ( updateIf
  ) where

import Prelude

updateIf :: forall f a. Functor f => Eq a => a -> (a -> a) -> f a -> f a
updateIf x f =
  map \y ->
    if y == x then
      f y
    else
      y
