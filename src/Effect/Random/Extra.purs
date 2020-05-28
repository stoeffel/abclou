module Effect.Random.Extra
  ( randomUniqElements
  , randomWeighted
  ) where

import Prelude
import Effect (Effect)
import Effect.Random (random, randomInt)
import Data.Array as A
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..), fst, snd)

randomUniqElements ::
  forall a.
  Eq a =>
  Int ->
  NonEmptyArray (Tuple Number a) ->
  Effect (Maybe (NonEmptyArray a))
randomUniqElements 0 xs = pure Nothing

randomUniqElements 1 xs = Just <<< AN.singleton <$> randomWeighted xs

randomUniqElements n xs = do
  x <- randomWeighted xs
  case AN.fromArray $ AN.filter ((_ /= x) <<< snd) xs of
    Just xs' -> do
      rest <- randomUniqElements (n - 1) xs'
      pure $ AN.cons x <$> rest
    Nothing -> pure Nothing

randomWeighted :: forall a. NonEmptyArray (Tuple Number a) -> Effect a
randomWeighted xs = do
  r <- (_ * sum (abs <<< fst <$> xs)) <$> random
  let
    { head: y, tail: ys } = AN.uncons xs
  pure $ getWeighted y ys r

getWeighted :: forall a. Tuple Number a -> Array (Tuple Number a) -> Number -> a
getWeighted x xs total = case A.uncons xs of
  Nothing -> snd x
  Just { head: y, tail: ys } ->
    if total <= fst x then
      snd x
    else
      getWeighted y ys (total - abs (fst x))
