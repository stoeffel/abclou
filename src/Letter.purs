module Letter (Slot, Message(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot p = forall q. H.Slot q Message p

type Input = String

data Message = Selected

data Action 
  = Select
  | HandleInput String

type State = { selected :: Boolean, letter :: String }

component :: forall q m. H.Component HH.HTML q Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction 
        , receive = Just <<< HandleInput
        }
    }

initialState :: Input -> State
initialState letter = { selected: false, letter }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let label = state.letter in
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just Select
      ]
      [ HH.text label ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Select -> H.raise Selected
  HandleInput newLetter -> do
    state <- H.get
    when (state.letter /= newLetter) $ H.put state { letter = newLetter }
 
