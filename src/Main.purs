module Main where

import Prelude
import Assets as Assets

import Data.Array as A
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Unit

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import CSS as CSS
import CSS.Common (center) as CSS
import CSS.Flexbox as FB

import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Console (log)
import Effect.Random (randomInt)
import Effect.Random.Extra (randomElement, randomUniqElements)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data Action
  = Initialize
  | SelectLetter Letter

data Message
  = Initialized 
  | Answered Letter
 
data Game 
  = NotStarted
  | Started Attempts Quiz
  | TryAgain Letter Quiz
  | Correct Letter

data Attempts
  = First
  | Second Letter
  | Third Letter Letter


type Quiz =
  { correct :: Letter
  , letters :: NonEmptyArray Letter
  }

type Letter =
  { character :: Char
  , word :: String
  , asset :: Assets.Asset
  }

data LetterState = Enabled | Wrong | Disabled

derive instance letterStateEq :: Eq LetterState

instance letterStateShow :: Show LetterState where
  show Enabled = "enabled"
  show Disabled = "disabled"
  show Wrong = "wrong"

attemptToState :: Attempts -> Letter -> LetterState
attemptToState First _ = Enabled
attemptToState (Second a) b
  | a == b = Wrong
  | otherwise = Enabled
attemptToState (Third a b) c
  | a == c = Disabled
  | b == c = Wrong
  | otherwise = Enabled

component :: forall q i . H.Component HH.HTML q i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize}
    }

initialState :: forall i. i -> Game
initialState _ = NotStarted


type View c m = H.ComponentHTML Action c m

color1 :: CSS.Color
color1 = CSS.rgba 223 124 168 0.94

color2 :: CSS.Color
color2 = CSS.rgba 162 90 122 0.94

color3 :: CSS.Color
color3 = CSS.rgba 131 73 99 1.0

color4 :: CSS.Color
color4 = CSS.rgba 112 77 78 1.0

render :: forall c m. Game -> View c m
render NotStarted = container [HH.text "Ein Moment..."]
render (Started attempt quiz) = container [viewQuiz attempt quiz]
render (Correct letter) = container [HH.text "Correct!"]
render (TryAgain _ _) = container [HH.text "Try again"]

container :: forall c m. Array (View c m) -> View c m
container = 
  HH.div 
  [ HC.style $ do
      CSS.display CSS.flex 
      CSS.alignItems CSS.center 
      CSS.flexDirection CSS.column
      CSS.height $ CSS.pct 100.0
  ]
  <<< A.cons (
    HH.h1 
    [ HC.style $ do
        CSS.color color3
        CSS.fontSize $ CSS.em 4.0
        CSS.marginBottom CSS.nil
    ]
    [ HH.text "ABC LOU" ]
  )

viewQuiz :: forall c m. Attempts -> Quiz -> View c m
viewQuiz attempt quiz =
  HH.div 
    [ HC.style $ do
        CSS.alignItems CSS.stretch 
        CSS.display CSS.flex 
        CSS.flexDirection CSS.column
        CSS.justifyContent CSS.spaceBetween 
        FB.flex 2 0 CSS.nil
    ]
    [ viewDescription quiz.correct
    , HH.ul
        [ HC.style $ do
            CSS.alignItems CSS.stretch
            CSS.display CSS.flex 
            CSS.justifyContent CSS.spaceBetween
            CSS.margin ( CSS.em 1.2 ) CSS.nil CSS.nil CSS.nil
            CSS.padding CSS.nil CSS.nil CSS.nil CSS.nil
            FB.flex 2 0 CSS.nil
        ]
        $ AN.toArray $ viewLetter attempt <$> quiz.letters
    ]

viewDescription :: forall c m. Letter -> View c m
viewDescription {word, asset} =
  HH.div 
    [ HC.style $ do
        CSS.alignItems CSS.center 
        CSS.display CSS.flex 
        CSS.flexDirection CSS.column
    ]
    [ HH.h3_ [HH.text word]
    , HH.img
        [ HP.alt word
        , HP.src (Assets.for asset)
        , HC.style $ do
            CSS.height $ CSS.px 400.0
            CSS.width $ CSS.px 400.0
        ]
    ]

viewLetter :: forall c m. Attempts -> Letter -> View c m
viewLetter attempt letter@{character} =
  let state = attemptToState attempt letter in
  HH.button
    [ HP.title (SCU.singleton character)
    , HP.enabled (state == Enabled)
    , HP.id_ (SCU.singleton character)
    , HP.classes 
        [ HH.ClassName "letter"
        , HH.ClassName (show state)
        ]
    , HC.style $ do
        CSS.borderRadius (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) 
        CSS.fontSize $ CSS.em 6.0
        CSS.height $ CSS.pct 80.0
        CSS.margin (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
        CSS.maxHeight $ CSS.px 150.0
        CSS.maxWidth $ CSS.px 150.0
        CSS.width $ CSS.pct 80.0
        case state of
          Disabled -> CSS.backgroundColor color2
          Enabled -> CSS.backgroundColor color1
          Wrong -> CSS.backgroundColor color2
    , HE.onClick \_ -> Just (SelectLetter letter)
    ]
    [ HH.text (SCU.singleton character) ]


handleAction ::  forall c. Action -> H.HalogenM Game Action c Message Aff Unit
handleAction = case _ of
  Initialize -> do
    quiz <- H.liftEffect randomQuiz
    _ <- H.liftEffect (log "HELLO")
    H.put $ Started First quiz
    H.raise Initialized
  SelectLetter letter -> do
    next <- H.modify (maybeNewGame letter)
    case next of
      Correct _ -> H.liftAff nextGame >>= H.put
      _ -> H.put next
    H.raise $ Answered letter


nextGame :: Aff Game
nextGame = do
  delay $ Milliseconds 1500.0
  Started First <$> H.liftEffect randomQuiz

maybeNewGame :: Letter -> Game -> Game
maybeNewGame answer game@(Started attempt quiz)
  | quiz.correct.character == answer.character = Correct answer
  | otherwise = flip Started quiz $
      case attempt of
        First -> Second answer
        Second firstAnswer -> Third firstAnswer answer
        a -> a
maybeNewGame _ game = game
  
randomQuiz :: Effect Quiz
randomQuiz = do
  let letterA = AN.singleton $ AN.head allLetters
  letters <- M.fromMaybe letterA <$> randomUniqElements 3 allLetters
  correct <- randomElement letters
  pure { correct, letters }

allLetters :: NonEmptyArray Letter
allLetters = 
  let
    a :: Letter
    a = { character: 'A' , word: "Aff" , asset: Assets.aff }
   in AN.cons' a $
     [ { character: 'B', word: "Bär", asset: Assets.aff }
     , { character: 'C', word: "Clown", asset: Assets.aff }
     , { character: 'D', word: "Dame", asset: Assets.aff }
     , { character: 'E', word: "Elch", asset: Assets.aff }
     , { character: 'F', word: "Fuchs", asset: Assets.aff }
     , { character: 'G', word: "Giraffe", asset: Assets.aff }
     , { character: 'H', word: "Hund", asset: Assets.aff }
     , { character: 'I', word: "Igel", asset: Assets.aff }
     , { character: 'J', word: "Jäger", asset: Assets.aff }
     , { character: 'K', word: "Karate", asset: Assets.aff }
     , { character: 'L', word: "Lache", asset: Assets.aff }
     , { character: 'M', word: "Mama", asset: Assets.aff }
     , { character: 'N', word: "Nase", asset: Assets.aff }
     , { character: 'O', word: "Ohr", asset: Assets.aff }
     , { character: 'P', word: "Papa", asset: Assets.aff }
     , { character: 'Q', word: "Quack", asset: Assets.aff }
     , { character: 'R', word: "Raggete", asset: Assets.aff }
     , { character: 'S', word: "Stern", asset: Assets.aff }
     , { character: 'T', word: "Tanze", asset: Assets.aff }
     , { character: 'U', word: "Uhu", asset: Assets.aff }
     , { character: 'V', word: "Velo", asset: Assets.aff }
     , { character: 'W', word: "Winter", asset: Assets.aff }
     , { character: 'X', word: "Xylophone", asset: Assets.aff }
     , { character: 'Y', word: "Yak", asset: Assets.aff }
     , { character: 'Z', word: "Zug", asset: Assets.aff }
    ]
