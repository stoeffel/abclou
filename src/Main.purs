module Main where

import Prelude

import Letter as L

import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action
  = Increase 
  | HandleLetterUpdate L.Message

type State =
  { test :: Int
  , correct :: String
  }

type ChildSlots =
  ( letter :: L.Slot Int
  )

_letter = SProxy :: SProxy "letter"

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction}
    }

initialState :: forall i. i -> State
initialState _ = { test: 0, correct: "A" }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.ul_
        [ HH.slot _letter 0 L.component "A" (Just <<< HandleLetterUpdate)
        , HH.slot _letter 1 L.component "B" (Just <<< HandleLetterUpdate)
        ]
    , HH.button
      [ HP.title ("+1 == " <> show state.test)
      , HE.onClick  \_ -> Just Increase
      ]
      [ HH.text ("+1 == " <> show state.test) ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Increase -> H.modify_ \st -> st { test = st.test + 1 }
  HandleLetterUpdate msg -> H.modify_ \st -> st { test = st.test + 1 }
