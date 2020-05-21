module Keyboard
  ( startListening
  , stopListening
  , awaitKey
  ) where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import React.SyntheticEvent as R


foreign import startListening :: Effect Unit
foreign import stopListening :: Effect Unit

foreign import _awaitKey :: EffectFnAff R.SyntheticKeyboardEvent

awaitKey :: Aff R.SyntheticKeyboardEvent
awaitKey = fromEffectFnAff _awaitKey
