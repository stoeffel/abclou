module Game where

import Prelude

import Keyboard as Keyboard
import Letter as Letter
import Letter (Letter)
import Sounds as Sounds
import Sounds (Sounds)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Foldable (oneOf)
import Data.Functor.Extra (updateIf)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Foreign (unsafeToForeign)

import Control.Alt ((<|>))
import Control.Monad.Loops (iterateUntil)

import Record (merge)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
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
import Routing.PushState (makeInterface, matches, PushStateInterface) as Routing
import Routing.Match (Match, lit, end, root) as Routing

data Action
  = Loaded { sounds :: Sounds, page :: Page }
  | SelectLetter Letter
  | NextGame (Maybe Letter)
  | GoTo Page
  | SettingsAction SettingsAction

data SettingsAction
  = SettingsNoOp

data Page
  = AbcLouPage
  | SettingsPage

type Nav = Routing.PushStateInterface

pages :: Routing.Match Page
pages =
  Routing.root *> oneOf
    [ SettingsPage <$ Routing.lit "settings"
    , pure AbcLouPage
    ] <* Routing.end

class IsPage a where
  documentTitle :: a -> String
  url :: a -> String

instance pageIsPage :: IsPage Page where
  documentTitle AbcLouPage = "ABCLOU"
  documentTitle SettingsPage = "ABCLOU - Settings"
  url AbcLouPage = "/"
  url SettingsPage = "/settings"

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
  }

data Game 
  = Loading Page
  | AbcLou Quiz
  | Settings

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

initialState :: Page -> Model
initialState page = { game: Loading page, letters: Letter.all, sounds: Sounds.def }

main :: Effect Unit
main = do
  nav <- Routing.makeInterface
  runPage <- nav # Routing.matches pages \_ page ->
    runWidgetInDom "app" do
      liftEffect $ log $ documentTitle page
      _ <- liftEffect CD.connectDevTools
      render nav (initialState page)
  runPage

render :: forall a. Nav -> Model -> Widget HTML a
render nav model = do
  action <- view model
  render nav =<< liftEffect (update nav action model)

update :: Nav -> Action -> Model -> Effect Model
update nav action model = case action of
  Loaded {page, sounds} ->
    update nav (GoTo page) model { sounds = sounds }
  GoTo page -> do
    nav.pushState (unsafeToForeign {}) $ url page
    case page of
      AbcLouPage -> update nav (NextGame Nothing) model
      SettingsPage -> pure model { game = Settings }
  SelectLetter letter -> 
    pure $ answeredCorrectly letter model
  NextGame letter -> do
    game <- nextGame letter model.letters 
    pure model { game = game }
  SettingsAction settingsAction ->
    case settingsAction of
      SettingsNoOp -> pure model

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
view { game: Loading page } = Loaded <<< merge { page } <$> viewLoading 
view { game: Settings } = 
  layout
    { backPage: AbcLouPage
    , title: PageTitle "Settings"
    , content: [ SettingsAction <$> viewSettings ] 
    , additionalClass: Nothing
    }
view { game: AbcLou quiz, sounds } = 
  layout
    $ merge { backPage: SettingsPage }
    $ case quiz.attempt of
        Correct letter ->
          { content: 
              [ viewCorrect letter sounds
              , viewLetters quiz
              ]
          , title: CorrectTitle letter
          , additionalClass: Just "correct"
          }
        _ ->
          { content:
              [ viewQuiz quiz sounds
              , viewLetters quiz
              ]
          , title: AppTitle
          , additionalClass: Nothing
          }

layout :: 
  { additionalClass :: Maybe String
  , backPage :: Page
  , content :: Array (Widget HTML Action)
  , title :: Title
  }
  -> Widget HTML Action
layout { additionalClass, title, content, backPage } =
  D.div [ P.classList [ Just "app", additionalClass ] ] 
    [ D.div [ P.className "container" ] 
        $ [ viewTitle title ]
       <> content
       <> [ pure (GoTo backPage) <* D.button
              [ P.onClick, P.className "settings-button" ]
              [ D.text "Settings" ]
          ]
    ]

data Title
  = AppTitle
  | PageTitle String
  | CorrectTitle Letter

viewTitle :: forall a. Title -> Widget HTML a
viewTitle title =
  D.h1 [P.classList [maybeStar]] [ D.text $ S.toUpper titleText ]
  where
    titleText = case title of
      AppTitle -> "ABC LOU"
      PageTitle str -> str
      CorrectTitle letter -> Letter.word letter
    maybeStar = case title of
      AppTitle -> Nothing
      PageTitle _ -> Just "page-title"
      CorrectTitle _ -> Just "correct-star"

viewLoading :: Widget HTML {sounds :: Sounds}
viewLoading = liftAff load <|> D.text "..."
  where
    load :: Aff {sounds :: Sounds}
    load = do
      sounds <- Sounds.load
      liftEffect $ Keyboard.startListening
      pure {sounds}

viewSettings :: Widget HTML SettingsAction
viewSettings =
  pure SettingsNoOp <* viewSettings'
  where
    viewSettings' =
      D.ul [ P.className "settings-page" ]
        [ D.li [] [ D.text "Graphics by J. Moffitt" ]
        , D.li []
            [ D.a
              [ P.href "https://github.com/stoeffel/abclou"
              , P.target "_blank"
              ]
              [ D.text "Code @ github.com/stoeffel/abclou" ]
            ]
        ]

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
