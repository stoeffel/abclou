module Sounds (load, play, Sounds(..), def, Sound, Key(..)) where

import Prelude
import Assets as Assets
import Control.Parallel (parallel, sequential)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Affjax as A
import Affjax.ResponseFormat as ARF
import Audio.WebAudio.AudioBufferSourceNode as WA
import Audio.WebAudio.BaseAudioContext as WB
import Audio.WebAudio.Types as WT
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Decode as Decode
import Data.Argonaut.Encode as Encode

type Sounds
  = Map Key Sound

def :: Sounds
def = Map.empty

data Key
  = Tada
  | Nope
  | Quack
  | Xylophone

derive instance soundTypesEq :: Eq Key

derive instance soundTypesOrd :: Ord Key

instance soundTypesShow :: Show Key where
  show Tada = "Tada"
  show Nope = "Nope"
  show Quack = "Quack"
  show Xylophone = "Xylophone"

instance decodeJsonKey :: Decode.DecodeJson Key where
  decodeJson =
    Right
      <<< Argonaut.caseJsonString Tada
          ( case _ of
              "Tada" -> Tada
              "Nope" -> Nope
              "Quack" -> Quack
              _ -> Xylophone
          )

instance encodeJsonKey :: Encode.EncodeJson Key where
  encodeJson = Encode.encodeJson <<< show

data Sound
  = Sound WT.AudioContext WT.AudioBuffer

load :: Aff Sounds
load = do
  ctx <- liftEffect WB.newAudioContext
  sounds <-
    sequential
      $ traverse
          ( \(Tuple k v) ->
              parallel
                $ map (Tuple k <<< Sound ctx)
                $ loadSoundBuffer ctx
                $ Assets.for v
          )
          [ Tuple Tada Assets.tada
          , Tuple Nope Assets.nope
          , Tuple Quack Assets.quackSound
          , Tuple Xylophone Assets.xylophoneSound
          ]
  pure $ Map.fromFoldable sounds

play :: Key -> Sounds -> Effect Unit
play k = play' <<< Map.lookup k
  where
  play' :: Maybe Sound -> Effect Unit
  play' Nothing = pure unit

  play' (Just (Sound ctx buffer)) = do
    startTime <- WB.currentTime ctx
    src <- WB.createBufferSource ctx
    dst <- WB.destination ctx
    _ <- WT.connect src dst
    _ <- WA.setBuffer buffer src
    WA.startBufferSource WA.defaultStartOptions src

loadSoundBuffer :: WT.AudioContext -> String -> Aff WT.AudioBuffer
loadSoundBuffer ctx filename = do
  res <-
    A.request
      $ A.defaultRequest
          { url = filename
          , method = Left GET
          , responseFormat = ARF.arrayBuffer
          }
  case res of
    Left err -> liftEffect $ throw $ A.printError err
    Right { body } -> WB.decodeAudioDataAsync ctx body
