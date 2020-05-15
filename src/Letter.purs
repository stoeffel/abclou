module Letter 
  ( Slot
  , Message(..)
  , Letter
  , Input
  , IsEnabled(..)
  , disable
  , wrong
  , isWrong
  , state
  , State
  , letters
  , fallback
  , letter
  , description
  ) where

import Prelude

import Assets as Assets

import Data.Unit
import Effect (Effect)
import Effect.Random.Extra (randomElement)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Data.NonEmpty ((:|))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import CSS as CSS
import CSS.Common as CSS

type Slot p = forall q. H.Slot q Message p

data Letter = Letter String String Assets.Asset

derive instance letterEq :: Eq Letter
derive instance letterOrd :: Ord Letter

type Input = 
  { letter :: Letter
  , isEnabled :: IsEnabled
  }

data IsEnabled = Enabled | Wrong | Disabled

derive instance isEnabledEq :: Eq IsEnabled

instance isEnabledShow :: Show IsEnabled where
  show Enabled = "enabled"
  show Disabled = "disabled"
  show Wrong = "wrong"

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

wrong :: State -> State
wrong state = state { isEnabled = Wrong }

isWrong :: State -> Boolean
isWrong state = state.isEnabled == Wrong

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
        , HP.src ( Assets.for imgSrc )
        , HC.style $ do
            CSS.width $ CSS.px 400.0
            CSS.height $ CSS.px 400.0
        ]
    ]

color1 = CSS.rgba 223 124 168 0.94
color2 = CSS.rgba 162 90 122 0.94

renderLetter :: forall m. State -> H.ComponentHTML Action () m
renderLetter state =
  let (Letter label _ _) = state.letter in
    HH.button
      [ HP.title (toUpper label)
      , HP.enabled (state.isEnabled == Enabled)
      , HP.id_ label
      , HP.classes 
        [ HH.ClassName "letter"
        , HH.ClassName $ show state.isEnabled 
        ]
      , HC.style $ do
         CSS.fontSize $ CSS.em 6.0
         CSS.width $ CSS.pct 80.0
         CSS.height $ CSS.pct 80.0
         CSS.maxWidth $ CSS.px 150.0
         CSS.maxHeight $ CSS.px 150.0
         CSS.borderRadius (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) 
         CSS.margin (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
         case state.isEnabled of
           Enabled -> CSS.backgroundColor color1
           Disabled -> CSS.backgroundColor color2
           Wrong -> CSS.backgroundColor color2
      , HE.onClick \_ -> Just Select
      ]
      [ HH.text (toUpper label) ]

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
 
a = Letter "a" "Aff" Assets.aff

fallback :: NonEmptyArray Letter
fallback = AN.singleton a

letters :: NonEmptyArray Letter
letters = 
   AN.cons' a $
    [ Letter "b" "Bär" Assets.aff
    , Letter "c" "Clown" Assets.aff
    , Letter "d" "Dame" Assets.aff
    , Letter "e" "Elch" Assets.aff
    , Letter "f" "Fuchs" Assets.aff
    , Letter "g" "Giraffe" Assets.aff
    , Letter "h" "Hund" Assets.aff
    , Letter "i" "Igel" Assets.aff
    , Letter "j" "Jäger" Assets.aff
    , Letter "k" "Karate" Assets.aff
    , Letter "l" "Lache" Assets.aff
    , Letter "m" "Mama" Assets.aff
    , Letter "n" "Nase" Assets.aff
    , Letter "o" "Ohr" Assets.aff
    , Letter "p" "Papa" Assets.aff
    , Letter "q" "Quack" Assets.aff
    , Letter "r" "Raggete" Assets.aff
    , Letter "s" "Stern" Assets.aff
    , Letter "t" "Tanze" Assets.aff
    , Letter "u" "Uhu" Assets.aff
    , Letter "v" "Velo" Assets.aff
    , Letter "w" "Winter" Assets.aff
    , Letter "x" "Xylophone" Assets.aff
    , Letter "y" "Yak" Assets.aff
    , Letter "z" "Zug" Assets.aff
    ]
