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
  , letter
  , description
  ) where

import Prelude

import Effect (Effect)
import Effect.Random.Extra (randomElement)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (toUpper)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CSS as CSS
import CSS.Common as CSS

type Slot p = forall q. H.Slot q Message p

data Letter = Letter String String String

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

data ActionDesc
  = HandleInputDesc Letter

type State = 
  { selected :: Boolean
  , letter :: Letter 
  , isEnabled :: IsEnabled
  }

state :: Letter -> State
state letter = { letter, isEnabled: Enabled, selected: false }

disable :: State -> State
disable state = state { isEnabled = Disabled }

letter :: forall q m. H.Component HH.HTML q Input Message m
letter =
  H.mkComponent
    { initialState
    , render: renderLetter
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction 
        , receive = Just <<< HandleInput
        }
    }

description :: forall q m. H.Component HH.HTML q Letter Message m
description =
  H.mkComponent
    { initialState: identity
    , render: renderDescription
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleActionDesc
        , receive = Just <<< HandleInputDesc
        }
    }

initialState :: Input -> State
initialState { letter, isEnabled } = { selected: false, isEnabled, letter }

renderDescription :: forall m. Letter -> H.ComponentHTML ActionDesc () m
renderDescription (Letter _ desc imgSrc) =
  HH.div 
    [ HC.style $ do
        CSS.display CSS.flex 
        CSS.alignItems CSS.center 
        CSS.flexDirection CSS.column
    ]
    [ HH.h3_ [HH.text desc]
    , HH.img
        [ HP.alt desc
        , HP.src imgSrc
        , HC.style $ do
            CSS.maxWidth $ CSS.pct 80.0
            CSS.maxHeight $ CSS.pct 80.0
        ]
    ]

renderLetter :: forall m. State -> H.ComponentHTML Action () m
renderLetter state =
  let (Letter label _ _) = state.letter in
    HH.button
      [ HP.title (toUpper label)
      , HP.enabled (state.isEnabled == Enabled)
      , HC.style $ do
         CSS.width $ CSS.pct 80.0
         CSS.height $ CSS.pct 80.0
      , HE.onClick \_ -> Just Select
      ]
      [ HH.text (toUpper label ) ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Select -> do
    st <- H.get
    H.put st { selected = not st.selected }
    H.raise $ Selected st.letter
  HandleInput {letter, isEnabled} -> do
    st <- H.get
    when (st.isEnabled /= isEnabled) $ H.put st { isEnabled = isEnabled }

handleActionDesc :: forall m. ActionDesc -> H.HalogenM Letter ActionDesc () Message m Unit
handleActionDesc = case _ of
  HandleInputDesc letter -> do
    st <- H.get
    when (st /= letter) $ H.put letter
 
a = Letter "a" "Aff" "https://via.placeholder.com/700"

fallback :: NonEmptyArray Letter
fallback = AN.singleton a

letters :: NonEmptyArray Letter
letters = 
   AN.cons' a $
    [ Letter "b" "Bär" "https://via.placeholder.com/700"
    , Letter "c" "Clown" "https://via.placeholder.com/700"
    , Letter "d" "Dame" "https://via.placeholder.com/700"
    , Letter "e" "Elch" "https://via.placeholder.com/700"
    , Letter "f" "Fuchs" "https://via.placeholder.com/700"
    , Letter "g" "Giraffe" "https://via.placeholder.com/700"
    , Letter "h" "Hund" "https://via.placeholder.com/700"
    , Letter "i" "Igel" "https://via.placeholder.com/700"
    , Letter "j" "Jäger" "https://via.placeholder.com/700"
    , Letter "k" "Karate" "https://via.placeholder.com/700"
    , Letter "l" "Lache" "https://via.placeholder.com/700"
    , Letter "m" "Mama" "https://via.placeholder.com/700"
    , Letter "n" "Nase" "https://via.placeholder.com/700"
    , Letter "o" "Ohr" "https://via.placeholder.com/700"
    , Letter "p" "Papa" "https://via.placeholder.com/700"
    , Letter "q" "Quack" "https://via.placeholder.com/700"
    , Letter "r" "Raggete" "https://via.placeholder.com/700"
    , Letter "s" "Stern" "https://via.placeholder.com/700"
    , Letter "t" "Tanze" "https://via.placeholder.com/700"
    , Letter "u" "Uhu" "https://via.placeholder.com/700"
    , Letter "v" "Velo" "https://via.placeholder.com/700"
    , Letter "w" "Winter" "https://via.placeholder.com/700"
    , Letter "x" "Xylophone" "https://via.placeholder.com/700"
    , Letter "y" "Yak" "https://via.placeholder.com/700"
    , Letter "z" "Zug" "https://via.placeholder.com/700"
    ]
