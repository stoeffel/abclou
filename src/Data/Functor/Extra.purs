module Data.Functor.Extra
  ( updateIf
  ) where

import Prelude

updateIf :: forall f a. Functor f => (a -> Boolean) -> (a -> a) -> f a -> f a
updateIf pred f =
  map \y ->
    if pred y then
      f y
    else
      y
