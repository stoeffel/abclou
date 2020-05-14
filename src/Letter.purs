module Letter 
  ( Slot
  , Message(..)
  , Letter
  , Input
  , IsEnabled(..)
  , disable
  , state
  , State
  , letters
  , fallback
  , component
  ) where

import Prelude

import Effect (Effect)
import Effect.Random.Extra (randomElement)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Maybe (fromMaybe, Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type Slot p = forall q. H.Slot q Message p

data Letter = Letter String

derive instance letterEq :: Eq Letter
derive instance letterOrd :: Ord Letter

type Input = 
  { letter :: Letter
  , isEnabled :: IsEnabled
  }

data IsEnabled = Enabled | Disabled

derive instance isEnabledEq :: Eq IsEnabled

data Message = Selected Letter

data Action 
  = Select
  | HandleInput Input

type State = 
  { selected :: Boolean
  , letter :: Letter 
  , isEnabled :: IsEnabled
  }

state :: Letter -> State
state letter = { letter, isEnabled: Enabled, selected: false }

disable :: State -> State
disable state = state { isEnabled = Disabled }

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
initialState { letter, isEnabled } = { selected: false, isEnabled, letter }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let (Letter label) = state.letter in
    HH.button
      [ HP.title label
      , HP.enabled (state.isEnabled == Enabled)
      , HE.onClick \_ -> Just Select
      ]
      [ HH.text label ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Select -> do
    st <- H.get
    H.put st { selected = not st.selected }
    H.raise $ Selected st.letter
  HandleInput {letter, isEnabled} -> do
    st <- H.get
    when (st.isEnabled /= isEnabled) $ H.put st { isEnabled = isEnabled }
 
fallback :: NonEmptyArray Letter
fallback = AN.singleton (Letter "a")

letters :: NonEmptyArray Letter
letters = 
  Letter <$> AN.cons' "a"
    [ "b", "c", "d", "e"
    , "f", "g", "h", "i"
    , "j", "k", "l", "m"
    , "n", "o", "p", "q"
    , "r", "s", "t", "u"
    , "v", "w", "x", "y"
    , "z" ]
