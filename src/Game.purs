module Game where

import Prelude

import Keyboard as Keyboard
import Letter as Letter
import Letter (Letter)
import Sounds as Sounds
import Sounds (Sounds)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Either
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
import Routing (match) as Routing
import Routing.PushState (makeInterface, matches, PushStateInterface) as Routing
import Routing.Match (Match, lit, end, root) as Routing

data Action
  = Loaded { sounds :: Sounds, page :: Page }
  | SelectLetter Letter
  | NextGame (Maybe Letter)
  | GoTo Page
  | SettingsAction SettingsAction

data SettingsAction
  = ToggleSound

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

pageURL :: Page -> String
pageURL AbcLouPage = "/"
pageURL SettingsPage = "/settings"

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
  , settings :: Settings
  }

data Game 
  = Loading Page
  | AbcLou Quiz
  | Settings

type Settings =
  { soundIsEnabled :: IsEnabled
  }

data IsEnabled = Enabled | Disabled

derive instance isEnabledEq :: Eq IsEnabled

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
initialState page =
  { game: Loading page
  , letters: Letter.all
  , sounds: Sounds.def
  , settings: { soundIsEnabled: Enabled }
  }

main :: Effect Unit
main = do
  nav <- Routing.makeInterface
  run <- nav.listen \{pathname} -> do
    _ <- nav # Routing.matches pages \_ -> runApp nav 
    pure unit
  _ <- nav # Routing.matches pages \_ -> runApp nav 
  run
  where
    runApp nav page = runWidgetInDom "app" do
      _ <- liftEffect CD.connectDevTools
      render nav (initialState page)

render :: forall a. Nav -> Model -> Widget HTML a
render nav model = do
  action <- view model
  render nav =<< liftEffect (update nav action model)

update :: Nav -> Action -> Model -> Effect Model
update nav action model = case action of
  Loaded {page, sounds} ->
    case page of
      AbcLouPage -> update nav (NextGame Nothing) model
      SettingsPage -> pure model { game = Settings }
  GoTo page ->
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
      ToggleSound -> pure model 
        { settings = model.settings
            { soundIsEnabled =
              if model.settings.soundIsEnabled == Enabled then
                Disabled
              else
                Enabled
            }
        }


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
view { game: Settings, settings } = 
  layout
    { backPage: AbcLouPage
    , title: PageTitle "Settings"
    , content: [ SettingsAction <$> viewSettings settings ] 
    , additionalClass: Nothing
    }
view { game: AbcLou quiz, sounds, settings } = 
  let soundPlayer = mkSoundPlayer sounds settings.soundIsEnabled in
  layout
    $ merge { backPage: SettingsPage }
    $ case quiz.attempt of
        Correct letter ->
          { content: 
              [ viewCorrect letter soundPlayer
              , viewLetters quiz
              ]
          , title: CorrectTitle letter
          , additionalClass: Just "correct"
          }
        _ ->
          { content:
              [ viewQuiz quiz soundPlayer
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

viewSettings :: Settings -> Widget HTML SettingsAction
viewSettings { soundIsEnabled }=
  viewSettings'
  where
    viewSettings' =
      D.div []
        [ ToggleSound <$ D.input 
            [ P._type "checkbox"
            , P.name "sound"
            , P.checked (soundIsEnabled == Enabled)
            , P.onChange
            ]
        , D.label
            [ P.htmlFor "sound" ]
            [ D.text "Sound on/off" ]
        , D.ul [ P.className "settings-page" ]
            [ D.li [] [ D.text "Graphics by J. Moffitt" ]
            , D.li []
                [ D.a
                  [ P.href "https://github.com/stoeffel/abclou"
                  , P.target "_blank"
                  ]
                  [ D.text "Code @ github.com/stoeffel/abclou" ]
                ]
            ]
        ]

viewQuiz :: Quiz -> SoundPlayer -> Widget HTML Action
viewQuiz quiz soundPlayer = do
  if quiz.attempt /= First then
    liftEffect $ soundPlayer Sounds.Nope
  else 
    pure unit
  case Letter.sound quiz.correct of
    Just sound -> liftEffect $ soundPlayer sound
    Nothing -> pure unit
  viewWordImage quiz.correct

viewCorrect :: Letter -> SoundPlayer -> Widget HTML Action
viewCorrect letter soundPlayer = do
  liftEffect $ soundPlayer Sounds.Tada
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

type SoundPlayer = Sounds.Key -> Effect Unit

mkSoundPlayer :: Sounds -> IsEnabled -> SoundPlayer
mkSoundPlayer _ Disabled _ = pure unit
mkSoundPlayer sounds Enabled key =
  Sounds.play key sounds
