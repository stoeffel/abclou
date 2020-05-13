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


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action
  = LetterMessage L.Letter L.Message
  | Initialize

data Message
  = Initialized 

data Game 
  = NotStarted
  | Started Quiz

type Quiz =
  { correct :: Int
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

render :: forall m. Game -> H.ComponentHTML Action ChildSlots m
render NotStarted = HH.text "Ein Moment..."
render (Started quiz) =
  HH.div_
    [ HH.text (show quiz.correct)
    , HH.ul_ $ AN.toArray $
        viewLetter <$> quiz.letters
    ]

viewLetter :: forall m. L.Letter -> H.ComponentHTML Action ChildSlots m
viewLetter letter =
  HH.slot _letter letter L.component letter 
    (Just <<< LetterMessage letter)

handleAction ::  Action -> H.HalogenM Game Action ChildSlots Message Aff Unit
handleAction = case _ of
  Initialize -> do
    quiz <- H.liftEffect randomQuiz
    H.modify_ \_ -> Started quiz
    H.raise Initialized
  LetterMessage letter msg -> pure unit

randomQuiz :: Effect Quiz
randomQuiz = do
  let max = 3
  correct <- randomInt 0 max
  first <- L.random
  letters <- foldM 
    (\acc _ -> L.random >>= (pure <<< AN.snoc acc ))
    (AN.singleton first) (A.range 1 max)
  pure { correct, letters }
