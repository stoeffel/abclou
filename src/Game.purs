module Game where

import Prelude

import Keyboard as Keyboard
import Letter as Letter
import Letter (Letter)
import Sounds as Sounds
import Sounds (Sounds)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Functor.Extra (updateIf)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Time.Duration (Milliseconds(..))

import Control.Alt ((<|>))
import Control.Monad.Loops (iterateUntil)

import Record (merge)

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
  | SelectLetter Letter
  | NextGame (Maybe Letter)

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
  }

data Game 
  = Loading
  | AbcLou Quiz

data Attempt
  = First
  | Second Letter
  | Third Letter Letter
  | Correct Letter

derive instance attemptsEq :: Eq Attempt

type Quiz =
  { correct :: Letter
  , letters :: NonEmptyArray Letter
  , attempt :: Attempt 
  }

initialState :: Model
initialState = { game: Loading, letters: Letter.all, sounds: Sounds.def }

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
nextGame lastAnswer letters = do
  quizData <- iterateUntil ((_ /= lastAnswer) <<< Just <<< _.correct)
      (Letter.random letters)
  pure $ AbcLou $ merge quizData {attempt: First}

answeredCorrectly :: Letter -> Model -> Model
answeredCorrectly answer model@{ letters, game : AbcLou quiz }
  | Letter.sameLetter quiz.correct answer = model 
      { game = AbcLou quiz { attempt = Correct quiz.correct }
      , letters = updateIf quiz.correct (Letter.adjustFrequency (-2.0)) letters 
      }
  | otherwise = 
      model
        { letters = updateIf quiz.correct (Letter.adjustFrequency 5.0) letters
        , game = AbcLou quiz 
            { attempt = case quiz.attempt of
              First -> Second answer
              Second firstAnswer -> Third firstAnswer answer
              a -> a
            }
        }
answeredCorrectly _ model = model

view :: Model -> Widget HTML Action
view { game: Loading } = viewLoading 
view { game: AbcLou quiz, sounds } = 
  D.div [ P.classList [ Just "app", maybeCorrectClass ] ] 
    [ D.div [ P.className "container" ] 
      [ viewTitle title
      , content
      , viewLetters quiz
      , D.small
          [ P.className "attributation" ]
          [ D.text "Graphics by J. Moffitt"
          , D.text " / "
          , D.text "Code @ github.com/stoeffel/abclou"
          ]
      ]
    ]
  where
    {content, title, maybeCorrectClass} =
      case quiz.attempt of
        Correct letter ->
          { content: viewCorrect letter sounds
          , title: CorrectTitle letter
          , maybeCorrectClass: Just "correct"
          }
        _ ->
          { content: viewQuiz quiz sounds
          , title: AppTitle
          , maybeCorrectClass: Nothing
          }

data Title
  = AppTitle
  | CorrectTitle Letter

viewTitle :: forall a. Title -> Widget HTML a
viewTitle title =
  D.h1 [P.classList [maybeStar]] [ D.text titleText ]
  where
    titleText = case title of
      AppTitle -> "ABC LOU"
      CorrectTitle letter -> Letter.word letter
    maybeStar = case title of
      AppTitle -> Nothing
      CorrectTitle _ -> Just "correct-star"

viewLoading :: Widget HTML Action
viewLoading = liftAff load <|> D.text "..."
  where
    load :: Aff Action
    load = do
      sounds <- Sounds.load
      liftEffect $ Keyboard.startListening
      pure (Loaded sounds)

viewQuiz :: Quiz -> Sounds -> Widget HTML Action
viewQuiz quiz sounds = do
  if quiz.attempt /= First then
    liftEffect $ Sounds.play Sounds.Nope sounds
  else 
    pure unit
  case Letter.sound quiz.correct of
    Just sound -> liftEffect $ Sounds.play sound sounds
    Nothing -> pure unit
  viewWordImage quiz.correct

viewCorrect :: Letter -> Sounds -> Widget HTML Action
viewCorrect letter sounds = do
  liftEffect $ Sounds.play Sounds.Tada sounds
  liftAff delayed <|> viewWordImage letter
  pure $ NextGame $ Just letter
  where
    delayed :: Aff Unit
    delayed = Aff.delay (Milliseconds 3000.0)

viewWordImage :: forall a. Letter -> Widget HTML a
viewWordImage letter =
  D.div [ P.className "word-image-container" ]
    [ D.img
        [ P.className "word-image"
        , P.alt $ Letter.word letter
        , P.title $ Letter.word letter
        , P.src $ Letter.asset letter
        , P.key $ Letter.character letter <> "-image"
        ]
    ]

viewLetters :: Quiz -> Widget HTML Action
viewLetters {attempt, letters} =
  D.ul [ P.className "letters" ]
    $ AN.toArray
    $ viewLetter attempt
   <$> letters

viewLetter :: Attempt -> Letter -> Widget HTML Action
viewLetter attempt letter = do
  (liftAff onLetterPress) <|> viewLetter'
  pure $ SelectLetter letter
  where
    viewLetter' :: Widget HTML Unit
    viewLetter' =
      D.button
        [ P.title (Letter.character letter)
        , P.disabled (attemptToClass attempt letter /= Nothing)
        , P._id (Letter.character letter)
        , P.classList [ Just $ "letter" , attemptToClass attempt letter ]
        , unit <$ P.onClick
        ]
        [ D.text (Letter.character letter) ]

    attemptToClass :: Attempt -> Letter -> Maybe String
    attemptToClass First _ = Nothing
    attemptToClass (Second a) b
      | Letter.sameLetter a b = Just "wrong"
      | otherwise = Nothing
    attemptToClass (Third a b) c
      | Letter.sameLetter a c = Just "disabled"
      | Letter.sameLetter b c = Just "wrong"
      | otherwise = Nothing
    attemptToClass (Correct a) b
      | Letter.sameLetter a b = Just "correct"
      | otherwise = Just "unfocused"

    onLetterPress :: Aff Unit
    onLetterPress = do
      ev <- Keyboard.awaitKey
      key <- liftEffect $ R.key ev
      if S.toUpper key == Letter.character letter then
        pure unit
      else
        onLetterPress
