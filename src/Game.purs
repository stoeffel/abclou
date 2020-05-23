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
  | SelectLetter Letter
  | NextGame (Maybe Letter)

type Model = 
  { game :: Game -- TODO add field for next game
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
view { game: NotStarted } = viewLoading 
view { game: Started attempt quiz, sounds } = viewQuiz attempt quiz sounds
view { game: Correct letter, sounds, letters } = viewCorrect letter sounds

container :: forall a. Widget HTML a -> Widget HTML a -> Widget HTML a
container title content =
  D.div [ P.className "container" ] 
    [ title
    , content
    , D.small
        [ P.className "attributation" ]
        [ D.text "Graphics by J. Moffitt"
        , D.text " / "
        , D.text "Code @ github.com/stoeffel/abclou"
        ]
    ]


viewTitle :: forall a. Widget HTML a
viewTitle = D.div [ P.className "title-container" ] [D.h1 [] [ D.text "ABC LOU" ]]

viewLoading :: Widget HTML Action
viewLoading = liftAff load <|> container viewTitle (D.text "...")
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
  liftEffect $ Sounds.playFor sounds $ Letter.sound quiz.correct
  container viewTitle $ fading (Letter.character quiz.correct) FadeIn
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
    delayed = Aff.delay (Milliseconds 3000.0)

    viewCorrect' :: Widget HTML Unit
    viewCorrect' =
      container 
        ( D.div [ P.className "title-container" ]
            [ star
            , D.h1 [] [ D.text $ Letter.word letter ]
            , star
            ]
        ) $ fading (Letter.character letter) FadeOut
        [ viewWordImage letter
        , D.ul [ P.className "letters" ]
            [ D.h3 [ P.className "correct-letter", P._id (Letter.character letter) ] 
                [ D.text (Letter.character letter) ]
            ]
        ]

    star =
      D.img 
          [ P.className "correct-star"
          , P.src $ Assets.for Assets.correct
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

data Fading
  = FadeIn
  | FadeOut

fading :: forall a. String -> Fading -> Array (Widget HTML a) -> Widget HTML a
fading k fade children = do
  let config = case fade of
        FadeIn -> {delay: 50.0, class: "fade-in", start: "in"}
        FadeOut -> {delay: 2700.0, class: "fade-out", start: "out"}
  liftAff (delayed config.delay) <|> (unit <$ view' config.start Nothing children)
  view' config.start (Just config.class) children
  where
    view' :: forall b. String -> Maybe String -> Array (Widget HTML b) -> Widget HTML b
    view' x y = D.div [ P.classList [Just "fading", Just x, y] , P.key k ]

    delayed :: Number -> Aff Unit
    delayed = Aff.delay <<< Milliseconds

viewWordImage :: forall a. Letter -> Widget HTML a
viewWordImage letter =
  D.div [ P.className "word-image-container" ]
    [ D.img
        [ P.className "word-image"
        , P.alt $ Letter.word letter
        , P.title $ Letter.word letter
        , P.src $ Letter.asset letter
        ]
    ]

viewLetters :: Attempts -> NonEmptyArray Letter -> Widget HTML Action
viewLetters attempt letters =
  D.ul [ P.className "letters" ] $ AN.toArray $ viewLetter attempt <$> letters

viewLetter :: Attempts -> Letter -> Widget HTML Action
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

    attemptToClass :: Attempts -> Letter -> Maybe String
    attemptToClass First _ = Nothing
    attemptToClass (Second a) b
      | Letter.sameLetter a b = Just "wrong"
      | otherwise = Nothing
    attemptToClass (Third a b) c
      | Letter.sameLetter a c = Just "disabled"
      | Letter.sameLetter b c = Just "wrong"
      | otherwise = Nothing

    onLetterPress :: Aff Unit
    onLetterPress = do
      ev <- Keyboard.awaitKey
      key <- liftEffect $ R.key ev
      if S.toUpper key == Letter.character letter then
        pure unit
      else
        onLetterPress
