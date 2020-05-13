module Letter 
  ( Slot
  , Message(..)
  , Letter
  , random
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

type Input = Letter

data Message = Selected

data Action 
  = Select
  | HandleInput Letter

type State = { selected :: Boolean, letter :: Letter }

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
  let (Letter label) = state.letter in
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
 
random :: Effect Letter
random = 
  randomElement
    $ Letter
    <$> AN.cons' "a"
    [ "b", "c", "d", "e"
    , "f", "g", "h", "i"
    , "j", "k", "l", "m"
    , "n", "o", "p", "q"
    , "r", "s", "t", "u"
    , "v", "w", "x", "y"
    , "z" ]
