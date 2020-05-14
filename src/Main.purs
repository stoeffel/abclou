module Main where

import Prelude

import Letter as L

import Data.Array as A
import Data.Array.NonEmpty as AN
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Foldable (foldM)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Unit

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Random (randomInt)
import Effect.Random.Extra (randomElement)


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
  | Correct L.Letter

type Quiz =
  { correct :: L.Letter
  , letters :: NonEmptyArray L.Letter
  }

type ChildSlots =
  ( letter :: L.Slot L.Letter
  )

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
render NotStarted = HH.text "Ein Moment..."
render (Correct letter) = HH.text "Correct!"
render (Started quiz) = viewQuiz quiz

viewQuiz :: forall m. Quiz -> View m
viewQuiz quiz =
  HH.div_
    [ viewLetter quiz.correct
    , HH.ul_ $ AN.toArray $
        viewLetter <$> quiz.letters
    ]

viewLetter :: forall m. L.Letter -> View m
viewLetter letter =
  HH.slot _letter letter L.component letter 
    (Just <<< LetterMessage)

handleAction ::  Action -> H.HalogenM Game Action ChildSlots Message Aff Unit
handleAction = case _ of
  Initialize -> do
    quiz <- H.liftEffect randomQuiz
    H.modify_ \_ -> Started quiz
    H.raise Initialized
  LetterMessage msg -> handleLetterMessage msg

handleLetterMessage :: L.Message -> H.HalogenM Game Action ChildSlots Message Aff Unit
handleLetterMessage = case _ of
  L.Selected letter -> do
    H.modify_ (maybeCorrect letter)
    H.raise $ Answered letter

maybeCorrect :: L.Letter -> Game -> Game
maybeCorrect _ NotStarted = NotStarted
maybeCorrect _ game@(Correct _) = game
maybeCorrect answer game@(Started quiz)
  | quiz.correct == answer = Correct answer
  | otherwise = game -- TODO TryAgain

randomQuiz :: Effect Quiz
randomQuiz = do
  let max = 3
  first <- L.random
  letters <- foldM 
    (\acc _ -> L.random >>= (pure <<< AN.snoc acc ))
    (AN.singleton first) (A.range 1 max)
  correct <- randomElement letters
  pure { correct, letters }
