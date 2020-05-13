module Effect.Random.Extra
  ( randomElement
  ) where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Maybe (fromMaybe, Maybe(..))

randomElement :: forall a. NonEmptyArray a -> Effect a
randomElement xs = do
  let fallback = AN.head xs
  index <- randomInt 0 (AN.length xs)
  pure $ fromMaybe fallback (xs !! index)
