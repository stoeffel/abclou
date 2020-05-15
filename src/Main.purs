module Main where

import Prelude

import Assets as Assets

import Data.Array as A
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Symbol (SProxy(..))
import Data.String (toUpper)
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
import CSS.Flexbox as CSS.FB

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
  | Started Quiz
  | TryAgain Letter Quiz
  | Correct Letter

type Quiz =
  { correct :: Letter
  , letters :: NonEmptyArray Letter
  }

type Letter =
  { character :: String
  , word :: String
  , asset :: Assets.Asset
  , state :: LetterState
  }

data LetterState = Enabled | Wrong | Disabled

derive instance letterStateEq :: Eq LetterState

instance letterStateShow :: Show LetterState where
  show Enabled = "enabled"
  show Disabled = "disabled"
  show Wrong = "wrong"

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

render :: forall c m. Game -> View c m
render NotStarted = container [HH.text "Ein Moment..."]
render (Correct letter) = container [HH.text "Correct!"]
render (Started quiz) = container [viewQuiz quiz]
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

color1 :: CSS.Color
color1 = CSS.rgba 223 124 168 0.94

color2 :: CSS.Color
color2 = CSS.rgba 162 90 122 0.94

color3 :: CSS.Color
color3 = CSS.rgba 131 73 99 1.0

color4 :: CSS.Color
color4 = CSS.rgba 112 77 78 1.0

viewQuiz :: forall c m. Quiz -> View c m
viewQuiz quiz =
  HH.div 
  [ HC.style $ do
      CSS.display CSS.flex 
      CSS.alignItems CSS.stretch 
      CSS.justifyContent CSS.spaceBetween 
      CSS.flexDirection CSS.column
      CSS.FB.flex 2 0 CSS.nil
  ]
    [ viewDescription quiz.correct
    , HH.ul
        [ HC.style $ do
            CSS.margin ( CSS.em 1.2 ) CSS.nil CSS.nil CSS.nil
            CSS.display CSS.flex 
            CSS.justifyContent CSS.spaceBetween
            CSS.alignItems CSS.stretch
            CSS.padding CSS.nil CSS.nil CSS.nil CSS.nil
            CSS.FB.flex 2 0 CSS.nil
        ]
        $ AN.toArray $ viewLetter <$> quiz.letters
    ]

viewDescription :: forall c m. Letter -> View c m
viewDescription {word, asset} =
  HH.div 
    [ HC.style $ do
        CSS.display CSS.flex 
        CSS.alignItems CSS.center 
        CSS.flexDirection CSS.column
    ]
    [ HH.h3_ [HH.text word]
    , HH.img
        [ HP.alt word
        , HP.src (Assets.for asset)
        , HC.style $ do
            CSS.width $ CSS.px 400.0
            CSS.height $ CSS.px 400.0
        ]
    ]

viewLetter :: forall c m. Letter -> View c m
viewLetter letter@{character, state} =
  HH.button
    [ HP.title (toUpper character)
    , HP.enabled (state == Enabled)
    , HP.id_ character
    , HP.classes 
      [ HH.ClassName "letter"
      , HH.ClassName $ show state 
      ]
    , HC.style $ do
        CSS.fontSize $ CSS.em 6.0
        CSS.width $ CSS.pct 80.0
        CSS.height $ CSS.pct 80.0
        CSS.maxWidth $ CSS.px 150.0
        CSS.maxHeight $ CSS.px 150.0
        CSS.borderRadius (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) 
        CSS.margin (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
        case state of
          Enabled -> CSS.backgroundColor color1
          Disabled -> CSS.backgroundColor color2
          Wrong -> CSS.backgroundColor color2
    , HE.onClick \_ -> Just (SelectLetter letter)
    ]
    [ HH.text (toUpper character) ]

a :: Letter
a =
  { character: "a"
  , word: "Aff"
  , asset: Assets.aff
  , state: Enabled
  }

letterA :: NonEmptyArray Letter
letterA = AN.singleton a

allLetters :: NonEmptyArray Letter
allLetters = 
   AN.cons' a $
     [ { character: "b", word: "Bär", asset: Assets.aff, state: Enabled }
     , { character: "c", word: "Clown", asset: Assets.aff, state: Enabled }
     , { character: "d", word: "Dame", asset: Assets.aff, state: Enabled }
     , { character: "e", word: "Elch", asset: Assets.aff, state: Enabled }
     , { character: "f", word: "Fuchs", asset: Assets.aff, state: Enabled }
     , { character: "g", word: "Giraffe", asset: Assets.aff, state: Enabled }
     , { character: "h", word: "Hund", asset: Assets.aff, state: Enabled }
     , { character: "i", word: "Igel", asset: Assets.aff, state: Enabled }
     , { character: "j", word: "Jäger", asset: Assets.aff, state: Enabled }
     , { character: "k", word: "Karate", asset: Assets.aff, state: Enabled }
     , { character: "l", word: "Lache", asset: Assets.aff, state: Enabled }
     , { character: "m", word: "Mama", asset: Assets.aff, state: Enabled }
     , { character: "n", word: "Nase", asset: Assets.aff, state: Enabled }
     , { character: "o", word: "Ohr", asset: Assets.aff, state: Enabled }
     , { character: "p", word: "Papa", asset: Assets.aff, state: Enabled }
     , { character: "q", word: "Quack", asset: Assets.aff, state: Enabled }
     , { character: "r", word: "Raggete", asset: Assets.aff, state: Enabled }
     , { character: "s", word: "Stern", asset: Assets.aff, state: Enabled }
     , { character: "t", word: "Tanze", asset: Assets.aff, state: Enabled }
     , { character: "u", word: "Uhu", asset: Assets.aff, state: Enabled }
     , { character: "v", word: "Velo", asset: Assets.aff, state: Enabled }
     , { character: "w", word: "Winter", asset: Assets.aff, state: Enabled }
     , { character: "x", word: "Xylophone", asset: Assets.aff, state: Enabled }
     , { character: "y", word: "Yak", asset: Assets.aff, state: Enabled }
     , { character: "z", word: "Zug", asset: Assets.aff, state: Enabled }
    ]
handleAction ::  forall c. Action -> H.HalogenM Game Action c Message Aff Unit
handleAction = case _ of
  Initialize -> do
    quiz <- H.liftEffect randomQuiz
    _ <- H.liftEffect (log "HELLO")
    H.modify_ \_ -> Started quiz
    H.raise Initialized
  SelectLetter letter -> do
    next <- H.modify (maybeNewGame letter)
    finally <- H.liftAff (nextGame next)
    H.put finally
    H.raise $ Answered letter

nextGame :: Game -> Aff Game
nextGame (Correct _) = do
  intermission
  Started <$> H.liftEffect randomQuiz
nextGame game@(TryAgain _ next) = pure $ Started next
nextGame game = pure game

maybeNewGame :: Letter -> Game -> Game
maybeNewGame answer game@(Started quiz)
  | quiz.correct.character == answer.character = Correct answer
  | otherwise = TryAgain answer $ quiz {letters = disable answer <$> quiz.letters}
maybeNewGame _ game = game
  
disable :: Letter -> Letter -> Letter
disable a b
  | a.character == b.character = b {state = Wrong}
  | b.state == Wrong = b {state = Disabled}
  | otherwise = b

intermission :: Aff Unit
intermission = delay $ Milliseconds 1500.0

randomQuiz :: Effect Quiz
randomQuiz = do
  letters <- fromMaybe letterA <$> randomUniqElements 3 allLetters
  correct <- randomElement letters
  pure { correct, letters }
