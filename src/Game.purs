module Game where

import Prelude
import Keyboard as Keyboard
import Letter as Letter
import Letter (Letter, Letters)
import Sounds as Sounds
import Sounds (Sounds)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Decode as Decode
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Encode as Encode
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser as Parser
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Either
import Data.Foldable (oneOf)
import Data.Functor.Extra (updateIf)
import Data.Newtype
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Foreign (unsafeToForeign)
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.Loops (iterateUntil)
import Record (merge)
import Effect (Effect)
import Effect.Console (log)
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
import Routing.PushState (makeInterface, matches, PushStateInterface) as Routing
import Routing.Match (Match, lit, end, root) as Routing
import Web.HTML as Web
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

data Action
  = Loaded (LoadedData ( page :: PageId ))
  | SelectLetter Letter
  | NextQuiz (Maybe Quiz)
  | GoTo PageId
  | SettingsAction SettingsAction

type LoadedData a
  = { sounds :: Sounds, settings :: Settings, maybeQuiz :: Maybe Quiz | a }

data SettingsAction
  = ToggleSound

data PageId
  = AbcLouPage
  | SettingsPage

instance pageIdShow :: Show PageId where
  show AbcLouPage = "AbcLouPage"
  show SettingsPage = "SettingsPage"

type Nav
  = Routing.PushStateInterface

pages :: Routing.Match PageId
pages =
  Routing.root
    *> oneOf
        [ SettingsPage <$ Routing.lit "settings"
        , pure AbcLouPage
        ]
    <* Routing.end

pageURL :: PageId -> String
pageURL AbcLouPage = "/"

pageURL SettingsPage = "/settings"

type Model
  = { page :: Page
    , sounds :: Sounds
    , settings :: Settings
    }

data Page
  = Loading PageId
  | AbcLou Quiz
  | Settings

type Settings
  = { soundIsEnabled :: IsEnabled
    }

data IsEnabled
  = Enabled
  | Disabled

derive instance isEnabledEq :: Eq IsEnabled

instance decodeJsonIsEnabled :: Decode.DecodeJson IsEnabled where
  decodeJson =
    Right
      <<< Argonaut.caseJsonString Enabled
          ( case _ of
              "Enabled" -> Enabled
              _ -> Disabled
          )

instance encodeJsonIsEnabled :: Encode.EncodeJson IsEnabled where
  encodeJson Enabled = Encode.encodeJson "Enabled"
  encodeJson Disabled = Encode.encodeJson "Disabled"

data Attempt
  = First
  | Second Letter
  | Third Letter Letter
  | Correct Letter

derive instance attemptsEq :: Eq Attempt

instance decodeJsonAttempt :: Decode.DecodeJson Attempt where
  decodeJson json = do
    x <- Decode.decodeJson json
    attempt <- x .: "attempt"
    case attempt of
      "first" -> pure First
      "second" -> do
        l <- x .: "letter"
        pure $ Second l
      "third" -> do
        l <- x .: "letter1"
        m <- x .: "letter2"
        pure $ Third l m
      "correct" -> do
        l <- x .: "letter"
        pure $ Correct l
      _ -> Left "Unknown attempt"

instance encodeJsonAttempt :: Encode.EncodeJson Attempt where
  encodeJson First = "attempt" := "first" ~> Argonaut.jsonEmptyObject
  encodeJson (Second l) = "attempt" := "second" ~> "letter" := l ~> Argonaut.jsonEmptyObject
  encodeJson (Third l m) = "attempt" := "third" ~> "letter1" := l ~> "letter2" := m ~> Argonaut.jsonEmptyObject
  encodeJson (Correct l) = "attempt" := "correct" ~> "letter" := l ~> Argonaut.jsonEmptyObject

type Quiz
  = { correct :: Letter
    , letters :: Letters
    , attempt :: Attempt
    , alphabet :: Letters
    }

initialState :: PageId -> Model
initialState page =
  { page: Loading page
  , sounds: Sounds.def
  , settings: defSettings
  }

defSettings :: Settings
defSettings = { soundIsEnabled: Enabled }

main :: Effect Unit
main = do
  nav <- Routing.makeInterface
  run <-
    nav
      # Routing.matches pages \_ page ->
          runWidgetInDom "app" do
            liftEffect $ log "reload"
            _ <- liftEffect CD.connectDevTools
            render nav (initialState page)
  run

render :: forall a. Nav -> Model -> Widget HTML a
render nav model = do
  action <- view model
  render nav =<< liftAff (update nav action model)

update :: Nav -> Action -> Model -> Aff Model
update nav action model = case action of
  Loaded { page, sounds, settings, maybeQuiz } -> do
    let
      newModel = model { sounds = sounds, settings = settings }
    case { page, maybeQuiz } of
      { page: AbcLouPage, maybeQuiz: Nothing } -> update nav (NextQuiz Nothing) newModel
      { page: AbcLouPage, maybeQuiz: Just quiz } -> pure newModel { page = AbcLou quiz }
      { page: SettingsPage } -> pure newModel { page = Settings }
  GoTo page -> do
    liftEffect $ nav.pushState (unsafeToForeign {}) $ pageURL page
    loadedData <- load
    update nav (Loaded $ merge { page } loadedData) model
  SelectLetter letter ->
    pure
      model
        { page =
          case model.page of
            AbcLou quiz -> AbcLou $ answeredCorrectly letter quiz
            p -> p
        }
  NextQuiz previous ->
    liftEffect do
      quiz <- nextQuiz previous
      storeItem StoreQuiz $ Just quiz
      pure model { page = AbcLou quiz }
  SettingsAction ToggleSound ->
    liftEffect do
      let
        settings =
          model.settings
            { soundIsEnabled =
              case model.settings.soundIsEnabled of
                Enabled -> Disabled
                Disabled -> Enabled
            }
      storeItem StoreSettings settings
      pure model { settings = settings }

data StoreKey
  = StoreSettings
  | StoreQuiz

instance storeKeyShow :: Show StoreKey where
  show StoreSettings = "abclou-settings"
  show StoreQuiz = "abclou-quiz"

loadItem :: forall a. Decode.DecodeJson a => StoreKey -> a -> Effect a
loadItem k def = do
  json <- Storage.getItem (show k) =<< Window.localStorage =<< Web.window
  let
    maybeV = hush <<< Decode.decodeJson =<< hush <<< Parser.jsonParser =<< json
  pure $ fromMaybe def maybeV

storeItem :: forall a. Encode.EncodeJson a => StoreKey -> a -> Effect Unit
storeItem k x = do
  let
    json = Argonaut.stringify $ Encode.encodeJson x
  Storage.setItem (show k) json =<< Window.localStorage =<< Web.window

nextQuiz :: Maybe Quiz -> Effect Quiz
nextQuiz maybePrev = do
  let
    { alphabet, prevCorrect } = case maybePrev of
      Just q -> { alphabet: q.alphabet, prevCorrect: Just q.correct }
      Nothing -> { alphabet: wrap Letter.all, prevCorrect: Nothing }
  { correct, letters } <-
    iterateUntil
      (not <<< fromMaybe false <<< lift2 Letter.sameLetter prevCorrect <<< Just <<< _.correct)
      (Letter.random $ unwrap alphabet)
  pure { attempt: First, correct, letters: wrap letters, alphabet }

answeredCorrectly :: Letter -> Quiz -> Quiz
answeredCorrectly answer quiz =
  let
    { attempt, frequency } =
      if Letter.sameLetter quiz.correct answer then
        { attempt: Correct quiz.correct
        , frequency: -1.0
        }
      else
        { attempt:
            case quiz.attempt of
              First -> Second answer
              Second firstAnswer -> Third firstAnswer answer
              a -> a
        , frequency: 2.0
        }
  in
    quiz
      { attempt = attempt
      , alphabet =
        wrap
          $ updateIf quiz.correct
              (Letter.adjustFrequency frequency)
          $ unwrap quiz.alphabet
      }

view :: Model -> Widget HTML Action
view { page: Loading page } = viewLoading page

view { page: Settings, settings } =
  layout
    { backPage: Just AbcLouPage
    , title: PageTitle "Settings"
    , content: [ SettingsAction <$> viewSettings settings ]
    , additionalClass: Just "settings-page"
    }

view { page: AbcLou quiz, sounds, settings } =
  let
    soundPlayer = mkSoundPlayer sounds settings.soundIsEnabled
  in
    layout
      $ merge { backPage: Just SettingsPage }
      $ case quiz.attempt of
          Correct letter ->
            { content:
                [ viewCorrect quiz soundPlayer
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
  , backPage :: Maybe PageId
  , content :: Array (Widget HTML Action)
  , title :: Title
  } ->
  Widget HTML Action
layout { additionalClass, title, content, backPage } =
  D.div [ P.classList [ Just "app", additionalClass ], P.key "main-layout" ]
    [ D.div [ P.className "container", P.key "main-container" ]
        $ [ viewTitle title ]
        <> content
        <> case backPage of
            Nothing -> []
            Just backPage' ->
              [ pure (GoTo backPage')
                  <* D.button
                      [ P.onClick
                      , P.classList
                          [ Just "navigaton-button"
                          , Just $ show backPage'
                          ]
                      ]
                      []
              ]
    ]

data Title
  = AppTitle
  | PageTitle String
  | CorrectTitle Letter

viewTitle :: forall a. Title -> Widget HTML a
viewTitle title = D.h1 [ P.classList [ maybeStar ] ] [ D.text $ S.toUpper titleText ]
  where
  titleText = case title of
    AppTitle -> "ABC LOU"
    PageTitle str -> str
    CorrectTitle letter -> Letter.word letter

  maybeStar = case title of
    AppTitle -> Nothing
    PageTitle _ -> Just "page-title"
    CorrectTitle _ -> Just "correct-star"

viewLoading :: PageId -> Widget HTML Action
viewLoading page =
  ( Loaded <<< merge { page }
      <$> liftAff load
  )
    <|> layout
        { backPage: Nothing
        , title: AppTitle
        , content: []
        , additionalClass: Nothing
        }

load :: Aff (LoadedData ())
load = do
  sounds <- Sounds.load
  liftEffect $ Keyboard.startListening
  settings <- liftEffect $ loadItem StoreSettings defSettings
  maybeQuiz <- liftEffect $ loadItem StoreQuiz Nothing
  pure { sounds, settings, maybeQuiz }

viewSettings :: Settings -> Widget HTML SettingsAction
viewSettings { soundIsEnabled } = viewSettings'
  where
  viewSettings' =
    D.div [ P.className "settings-content" ]
      [ ToggleSound
          <$ D.input
              [ P._type "checkbox"
              , P._id "sound"
              , P.className "sound"
              , P.checked (soundIsEnabled == Enabled)
              , P.onChange
              ]
      , D.label
          [ P.htmlFor "sound" ]
          [ D.text
              $ case soundIsEnabled of
                  Enabled -> "Sound on"
                  Disabled -> "Sound off"
          ]
      , D.h3 [] [ D.text "Info" ]
      , D.ul [ P.className "settings-info" ]
          [ D.li []
              [ D.a
                  [ P.href "https://github.com/stoeffel/abclou"
                  , P.target "_blank"
                  ]
                  [ D.text "Code @ github.com/stoeffel/abclou" ]
              ]
          , D.li []
              [ D.a
                  [ P.href "https://www.fiverr.com/jamesmoffittart"
                  , P.target "_blank"
                  ]
                  [ D.text "Graphics by J. Moffitt" ]
              ]
          , D.li []
              [ D.a
                  [ P.href "https://www.fiverr.com/conania"
                  , P.target "_blank"
                  ]
                  [ D.text "Icons by Canonia" ]
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

viewCorrect :: Quiz -> SoundPlayer -> Widget HTML Action
viewCorrect quiz@{ correct } soundPlayer = do
  liftEffect $ soundPlayer Sounds.Tada
  liftAff delayed <|> viewWordImage correct
  pure $ NextQuiz $ Just quiz
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
viewLetters { attempt, letters } =
  D.ul [ P.className "letters" ]
    $ AN.toArray
    $ viewLetter attempt
    <$> unwrap letters

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
      , P.classList [ Just $ "letter", attemptToClass attempt letter ]
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

type SoundPlayer
  = Sounds.Key -> Effect Unit

mkSoundPlayer :: Sounds -> IsEnabled -> SoundPlayer
mkSoundPlayer _ Disabled _ = pure unit

mkSoundPlayer sounds Enabled key = Sounds.play key sounds
