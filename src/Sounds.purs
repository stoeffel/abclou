module Sounds (load, play, Sounds(..), def, Sound) where

import Prelude

import Assets as Assets

import Control.Parallel (parallel, sequential)

import Data.Array ((!!))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

import Affjax as A
import Affjax.ResponseFormat as ARF
import Audio.WebAudio.AudioBufferSourceNode as WA
import Audio.WebAudio.BaseAudioContext as WB
import Audio.WebAudio.Types as WT


type Sounds =
  { tada :: Maybe Sound
  , nope :: Maybe Sound
  }

def :: Sounds
def =
  { tada: Nothing
  , nope: Nothing
  }

data Sound = Sound WT.AudioContext WT.AudioBuffer

load :: Aff Sounds
load = do
  ctx <- liftEffect WB.newAudioContext
  sounds <- sequential $ traverse 
    ( parallel <<<
      map (Sound ctx) <<<
      loadSoundBuffer ctx <<<
      Assets.for
    )
    [ Assets.tada, Assets.nope ]
  pure 
    { tada: sounds !! 0
    , nope: sounds !! 1
    }

play :: Maybe Sound -> Effect Unit
play Nothing = pure unit
play (Just (Sound ctx buffer)) = do
  startTime <- WB.currentTime ctx
  src <- WB.createBufferSource ctx
  dst <- WB.destination ctx
  _ <- WT.connect src dst
  _ <- WA.setBuffer buffer src
  WA.startBufferSource WA.defaultStartOptions src

loadSoundBuffer :: WT.AudioContext -> String -> Aff WT.AudioBuffer
loadSoundBuffer ctx filename = do
  res <- A.request $ A.defaultRequest 
    { url = filename
    , method = Left GET
    , responseFormat = ARF.arrayBuffer 
    }
  case res of
    Left err -> liftEffect $ throw $ A.printError err
    Right { body } -> WB.decodeAudioDataAsync ctx body