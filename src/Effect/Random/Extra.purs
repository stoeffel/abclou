module Effect.Random.Extra
  ( randomUniqElements
  , randomElement
  ) where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Maybe (fromMaybe, Maybe(..))

randomUniqElements :: forall a. Eq a 
  => Int 
  -> NonEmptyArray a 
  -> Effect (Maybe (NonEmptyArray a ))
randomUniqElements 0 xs = pure Nothing
randomUniqElements 1 xs = Just <<< AN.singleton <$> randomElement xs
randomUniqElements n xs = do
    x <- randomElement xs
    case AN.fromArray $ AN.filter (_ /= x) xs of
      Just xs' -> do
         rest <- randomUniqElements (n - 1) xs'
         pure $ AN.cons x <$> rest
      Nothing -> pure Nothing

randomElement :: forall a. NonEmptyArray a -> Effect a
randomElement xs = do
  let fallback = AN.head xs
  index <- randomInt 0 (AN.length xs)
  pure $ fromMaybe fallback (xs !! index)
