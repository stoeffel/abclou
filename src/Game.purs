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
import Data.String as S
import Data.Time.Duration (Milliseconds(..))

import Concur.Core.DevTools as CD
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)

import React.SyntheticEvent as R

import Control.Alt ((<|>))
import Control.Monad.Loops (iterateUntil)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff.Class (liftAff)
import Effect.Aff (Aff)
import Effect.Aff as Aff


data Action
  = Loaded Sounds
  | SelectLetter Letter
  | NextGame (Maybe Letter)

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
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

data LetterState = Enabled | Wrong | Disabled

derive instance letterStateEq :: Eq LetterState

instance letterStateShow :: Show LetterState where
  show Enabled = "enabled"
  show Disabled = "disabled"
  show Wrong = "wrong"

attemptToState :: Attempts -> Letter -> LetterState
attemptToState First _ = Enabled
attemptToState (Second a) b
  | Letter.sameLetter a b = Wrong
  | otherwise = Enabled
attemptToState (Third a b) c
  | Letter.sameLetter a c = Disabled
  | Letter.sameLetter b c = Wrong
  | otherwise = Enabled

initialState :: Model
initialState = { game: NotStarted, letters: Letter.all, sounds: Sounds.def }

main :: Effect Unit
main =
  runWidgetInDom "app" $ do
    _ <- liftEffect CD.connectDevTools
    render initialState

render :: forall a. Model -> Widget HTML a
render model = do
  action <- view model
  render =<< liftEffect (update action model)

update :: Action -> Model -> Effect Model
update action model = case action of
  Loaded sounds ->
    update (NextGame Nothing) model { sounds = sounds }
  SelectLetter letter -> 
    pure $ answeredCorrectly letter model
  NextGame letter -> do
    game <- nextGame letter model.letters 
    pure model { game = game }

nextGame :: Maybe Letter -> NonEmptyArray Letter -> Effect Game
nextGame lastAnswer letters = 
  Started First 
  <$> iterateUntil ((_ /= lastAnswer) <<< Just <<< _.correct)
      (Letter.random letters)

answeredCorrectly :: Letter -> Model -> Model
answeredCorrectly answer model@{ letters, game : Started attempt quiz }
  | Letter.sameLetter quiz.correct answer = model 
      { game = Correct quiz.correct
      , letters = updateIf quiz.correct (Letter.adjustFrequency (-2.0)) letters 
      }
  | otherwise = 
      model
        { letters = updateIf quiz.correct (Letter.adjustFrequency 5.0) letters
        , game = flip Started quiz $
            case attempt of
              First -> Second answer
              Second firstAnswer -> Third firstAnswer answer
              a -> a
        }
answeredCorrectly _ model = model

view :: Model -> Widget HTML Action
view { game: NotStarted } = container [ viewLoading ] 
view { game: Started attempt quiz, sounds } = container [ viewQuiz attempt quiz sounds ]
view { game: Correct letter, sounds, letters } = container [ viewCorrect letter sounds ]

container :: forall a. Array (Widget HTML a) -> Widget HTML a
container = D.div [ P.className "container" ] <<< A.cons (D.h1 [] [ D.text "ABC LOU" ])

viewLoading :: Widget HTML Action
viewLoading = liftAff load <|> D.text "..."
  where
    load :: Aff Action
    load = do
      sounds <- Sounds.load
      liftEffect $ Keyboard.startListening
      pure (Loaded sounds)

viewQuiz :: Attempts -> Quiz -> Sounds -> Widget HTML Action
viewQuiz attempt quiz sounds = do
  if attempt /= First then
    liftEffect $ Sounds.play sounds.nope
  else 
    pure unit
  D.div [ P.className "quiz" ]
    [ viewWordImage quiz.correct
    , viewLetters attempt quiz.letters
    ]

viewCorrect :: Letter -> Sounds -> Widget HTML Action
viewCorrect letter sounds = do
  liftEffect $ Sounds.play sounds.tada
  (liftAff onLetterPress) <|> (liftAff delayed) <|> viewCorrect'
  pure $ NextGame $ Just letter
  where
    delayed :: Aff Unit
    delayed = Aff.delay (Milliseconds 10000.0)

    viewCorrect' :: Widget HTML Unit
    viewCorrect' =
      D.a [ unit <$ P.onClick , P.className "correct" ]
        [ D.div
            [ P.className "image-container" ]
            [ viewWordImage letter
            , D.img 
              [ P.className "correct-star"
              , P.src $ Assets.for Assets.star 
              ]
            ]
        , D.h2 [] [ D.text $ Letter.word letter ]
        ]

    onLetterPress :: Aff Unit
    onLetterPress = do
      fiber <- Aff.forkAff Keyboard.awaitKey
      ev <- Aff.joinFiber fiber
      key <- liftEffect $ R.key ev
      if key == " " then
        pure unit
      else 
        onLetterPress

viewWordImage :: forall a. Letter -> Widget HTML a
viewWordImage letter =
  D.img
    [ P.alt $ Letter.word letter
    , P.title $ Letter.word letter
    , P.src $ Letter.asset letter
    , P.height "400"
    , P.width "400"
    ]

viewLetters :: Attempts -> NonEmptyArray Letter -> Widget HTML Action
viewLetters attempt letters =
  D.ul [ P.className "letters" ] $ AN.toArray $ viewLetter attempt <$> letters

viewLetter :: Attempts -> Letter -> Widget HTML Action
viewLetter attempt letter = do
  (liftAff onLetterPress) <|> viewLetter'
  pure $ SelectLetter letter
  where
    state = attemptToState attempt letter

    viewLetter' :: Widget HTML Unit
    viewLetter' =
      D.button
        [ P.title (Letter.character letter)
        , P.disabled (state /= Enabled)
        , P._id (Letter.character letter)
        , P.classList [ Just $ "letter" , Just $ show state ]
        , unit <$ P.onClick
        ]
        [ D.text (Letter.character letter) ]

    onLetterPress :: Aff Unit
    onLetterPress = do
      ev <- Keyboard.awaitKey
      key <- liftEffect $ R.key ev
      if S.toUpper key == Letter.character letter then
        pure unit
      else
        onLetterPress
