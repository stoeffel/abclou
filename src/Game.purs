module Game where

import Prelude

import Assets as Assets
import Keyboard as Keyboard
import Letter as Letter
import Letter (Letter)
import Sounds as Sounds
import Sounds (Sounds)

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Functor.Extra (updateIf)
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.String as S
import Data.Time.Duration (Milliseconds(..))

import Control.Alt ((<|>))
import Control.Monad.Loops (iterateUntil)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff.Class (liftAff)
import Effect.Aff (Aff)
import Effect.Aff as Aff

import Concur.Core.DevTools as CD
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)

import React.SyntheticEvent as R


data Action
  = Loaded Sounds
  | Answered Letter
  | NextGame Letter

type Model = 
  { game :: Focused Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
  }

type Focused a =
  { prev :: Maybe a
  , curr :: a
  , next :: a
  }

data Game 
  = NotStarted
  | Started Attempts Quiz
  | Correct Letter

data Attempts
  = First
  | Second Letter
  | Third Letter Letter

derive instance attemptsEq :: Eq Attempts

type Quiz =
  { correct :: Letter
  , letters :: NonEmptyArray Letter
  }

initialState :: Model
initialState = 
  { game: 
    { prev: Nothing
    , curr: NotStarted
    , next: NotStarted
    }
  , letters: Letter.all
  , sounds: Sounds.def 
  }

main :: Effect Unit
main =
  runWidgetInDom "app" $ do
    _ <- liftEffect CD.connectDevTools
    render initialState

render :: forall a. Model -> Widget HTML a
render model = do
  action <- 
    M.maybe (D.text "") viewGame model.game.prev
    <|> widgetGame model.game.curr model.sounds
    <|> viewGame model.game.next
  render =<< liftEffect (update action model)

update :: Action -> Model -> Effect Model
update action model = case action of
  Loaded sounds -> do
    first <- newQuiz Nothing model.letters 
    pure model 
      { game =
          { prev: Nothing
          , curr: Started First first
          , next: Correct first.correct
          }
      , sounds = sounds
      }
  Answered letter -> answeredCorrectly letter model
  NextGame letter -> do
    pure model
      { game =
          { prev: Just model.game.curr
          , curr: model.game.next
          , next: Correct $ case model.game.next of
              Started _ {correct} -> correct
              Correct correct -> correct
              NotStarted -> letter
          }
      }

newQuiz :: Maybe Letter -> NonEmptyArray Letter -> Effect Quiz
newQuiz lastAnswer letters = 
  iterateUntil ((_ /= lastAnswer) <<< Just <<< _.correct)
      (Letter.random letters)

answeredCorrectly :: Letter -> Model -> Effect Model
answeredCorrectly answer model@{ letters, game : { curr: Started attempt quiz } }
  | Letter.sameLetter quiz.correct answer = do
      next <- newQuiz (Just quiz.correct) model.letters 
      pure model 
        { game =
          { prev: Just model.game.curr
          , curr: model.game.next
          , next: Started First next
          }
      , letters = updateIf quiz.correct (Letter.adjustFrequency (-2.0)) letters 
      }
  | otherwise = 
      pure model
        { letters = updateIf quiz.correct (Letter.adjustFrequency 5.0) letters
        , game = model.game {
            curr = flip Started quiz $
              case attempt of
                First -> Second answer
                Second firstAnswer -> Third firstAnswer answer
                a -> a
            }
        }
answeredCorrectly _ model = pure model

widgetGame :: Game -> Sounds -> Widget HTML Action
widgetGame NotStarted _ = widgetLoading 
widgetGame (Started attempt quiz) sounds = widgetQuiz attempt quiz sounds
widgetGame (Correct letter) sounds = widgetCorrect letter sounds

viewGame :: forall a. Game -> Widget HTML a
viewGame NotStarted = viewLoading 
viewGame (Started attempt quiz) = viewQuiz attempt quiz
viewGame (Correct letter) = viewCorrect letter

container :: forall a. Widget HTML a -> Array (Widget HTML a) -> Widget HTML a
container title = D.div [ P.className "container" ] <<< A.cons title

viewTitle :: forall a. Widget HTML a
viewTitle = D.h1 [] [ D.text "ABC LOU" ]

widgetLoading :: Widget HTML Action
widgetLoading = liftAff load <|> viewLoading
  where
    load :: Aff Action
    load = do
      sounds <- Sounds.load
      liftEffect $ Keyboard.startListening
      pure (Loaded sounds)

viewLoading :: forall a. Widget HTML a
viewLoading = container viewTitle [D.text "..."]

widgetQuiz :: Attempts -> Quiz -> Sounds -> Widget HTML Action
widgetQuiz attempt quiz sounds = do
  if attempt /= First then
    liftEffect $ Sounds.play sounds.nope
  else 
    pure unit
  liftEffect $ Sounds.playFor sounds $ Letter.sound quiz.correct
  container viewTitle
    [ viewWordImage quiz.correct
    , viewLetters $ widgetLetter attempt <$> quiz.letters
    ]

viewQuiz :: forall a. Attempts -> Quiz -> Widget HTML a
viewQuiz attempt quiz = do
  container viewTitle
    [ viewWordImage quiz.correct
    , viewLetters $ viewLetter [] attempt <$> quiz.letters
    ]

widgetCorrect :: Letter -> Sounds -> Widget HTML Action
widgetCorrect letter sounds = do
  liftEffect $ Sounds.play sounds.tada
  (liftAff onLetterPress) <|> (liftAff delayed) <|> viewCorrect letter
  pure $ NextGame letter
  where
    delayed :: Aff Unit
    delayed = Aff.delay (Milliseconds 5000.0)

    onLetterPress :: Aff Unit
    onLetterPress = do
      fiber <- Aff.forkAff Keyboard.awaitKey
      ev <- Aff.joinFiber fiber
      key <- liftEffect $ R.key ev
      if key == " " then
        pure unit
      else 
        onLetterPress

viewCorrect :: forall a. Letter -> Widget HTML a
viewCorrect letter =
  container 
    ( D.div [ P.className "correct-word" ]
        [ star unit
        , D.h1 [] [ D.text $ Letter.word letter ]
        , star unit
        ]
    )
    [ viewWordImage letter ]

star :: forall a. Unit -> Widget HTML a
star _ =
  D.img 
      [ P.className "correct-star"
      , P.src $ Assets.for Assets.correct
      ]

viewWordImage :: forall a. Letter -> Widget HTML a
viewWordImage letter =
  D.img
    [ P.className "word-image"
    , P.alt $ Letter.word letter
    , P.title $ Letter.word letter
    , P.src $ Letter.asset letter
    ]

viewLetters :: forall a. NonEmptyArray (Widget HTML a) -> Widget HTML a
viewLetters = D.ul [ P.className "letters" ] <<< AN.toArray

widgetLetter :: Attempts -> Letter -> Widget HTML Action
widgetLetter attempt letter = do
  (liftAff onLetterPress) <|> viewLetter [unit <$ P.onClick] attempt letter
  pure $ Answered letter
  where
    onLetterPress :: Aff Unit
    onLetterPress = do
      ev <- Keyboard.awaitKey
      key <- liftEffect $ R.key ev
      if S.toUpper key == Letter.character letter then
        pure unit
      else
        onLetterPress

viewLetter :: forall a. Array (P.ReactProps a) -> Attempts -> Letter -> Widget HTML a
viewLetter attrs attempt letter =
  D.button
    ([ P.title (Letter.character letter)
    , P.disabled (attemptToClass attempt letter /= Nothing)
    , P._id (Letter.character letter)
    , P.classList [ Just $ "letter" , attemptToClass attempt letter ]
    ] <> attrs)
    [ D.text (Letter.character letter) ]

attemptToClass :: Attempts -> Letter -> Maybe String
attemptToClass First _ = Nothing
attemptToClass (Second a) b
  | Letter.sameLetter a b = Just "wrong"
  | otherwise = Nothing
attemptToClass (Third a b) c
  | Letter.sameLetter a c = Just "disabled"
  | Letter.sameLetter b c = Just "wrong"
  | otherwise = Nothing

