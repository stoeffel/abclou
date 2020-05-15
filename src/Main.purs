module Main where

import Prelude

import Letter as L

import Data.Array as A
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe, Maybe(..))
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
import CSS.Common as CSS
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
  = LetterMessage L.Message
  | Initialize

data Message
  = Initialized 
  | Answered L.Letter
 
data Game 
  = NotStarted
  | Started Quiz
  | TryAgain L.Letter Quiz
  | Correct L.Letter

type Quiz =
  { correct :: L.State
  , letters :: NonEmptyArray L.State
  }

type ChildSlots =
  ( letter :: L.Slot Int
  , description :: L.Slot Int
  )

_description = SProxy :: SProxy "description"
_letter = SProxy :: SProxy "letter"

component :: forall q i . H.Component HH.HTML q i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize}
    }

initialState :: forall i. i -> Game
initialState _ = NotStarted

type View m = H.ComponentHTML Action ChildSlots m

render :: forall m. Game -> View m
render NotStarted = container [HH.text "Ein Moment..."]
render (Correct letter) = container [HH.text "Correct!"]
render (Started quiz) = container [viewQuiz quiz]
render (TryAgain _ _) = container [HH.text "Try again"]

container :: forall m. Array (View m) -> View m
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

color3 = CSS.rgba 131 73 99 1.0
color4 = CSS.rgba 112 77 78 1.0

viewQuiz :: forall m. Quiz -> View m
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
        $ AN.toArray $ mapWithIndex viewLetter quiz.letters
    ]

viewDescription :: forall m. L.State -> View m
viewDescription {letter, letterState} =
  HH.slot _description 0 L.description letter (const Nothing)

viewLetter :: forall m. Int -> L.State -> View m
viewLetter index state@{letter} =
  HH.slot _letter index L.letter { letter: state.letter, letterState: state.letterState }
    (Just <<< LetterMessage)

handleAction ::  Action -> H.HalogenM Game Action ChildSlots Message Aff Unit
handleAction = case _ of
  Initialize -> do
    quiz <- H.liftEffect randomQuiz
    _ <- H.liftEffect (log "HELLO")
    H.modify_ \_ -> Started quiz
    H.raise Initialized
  LetterMessage msg -> handleLetterMessage msg

handleLetterMessage :: L.Message -> H.HalogenM Game Action ChildSlots Message Aff Unit
handleLetterMessage = case _ of
  L.Selected letter -> do
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

maybeNewGame :: L.Letter -> Game -> Game
maybeNewGame answer game@(Started quiz)
  | quiz.correct.letter == answer = Correct answer
  | otherwise = TryAgain answer $ quiz {letters = disable answer <$> quiz.letters}
maybeNewGame _ game = game
  
disable :: L.Letter -> L.State -> L.State
disable l st
  | st.letter == l = L.wrong st
  | L.isWrong st = L.disable st
  | otherwise = st

intermission :: Aff Unit
intermission = delay $ Milliseconds 1500.0

randomQuiz :: Effect Quiz
randomQuiz = do
  letters <- map L.state <$> fromMaybe L.fallback <$> randomUniqElements 3 L.letters
  correct <- randomElement letters
  pure { correct, letters }
