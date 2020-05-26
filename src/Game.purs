module Game where

import Prelude

import Keyboard as Keyboard
import Letter as Letter
import Letter (Letter)
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
import Routing (match) as Routing
import Routing.PushState (makeInterface, matches, PushStateInterface) as Routing
import Routing.Match (Match, lit, end, root) as Routing

import Web.HTML as Web
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

data Action
  = Loaded (LoadedData ( page :: PageId ))
  | SelectLetter Letter
  | NextQuiz (Maybe Letter)
  | GoTo PageId
  | SettingsAction SettingsAction

type LoadedData a = {sounds :: Sounds, settings :: Settings, maybeQuiz :: Maybe Quiz | a}

data SettingsAction
  = ToggleSound

data PageId
  = AbcLouPage
  | SettingsPage

instance pageIdShow :: Show PageId where
  show AbcLouPage = "AbcLouPage"
  show SettingsPage = "SettingsPage"

type Nav = Routing.PushStateInterface

pages :: Routing.Match PageId
pages =
  Routing.root *> oneOf
    [ SettingsPage <$ Routing.lit "settings"
    , pure AbcLouPage
    ] <* Routing.end

pageURL :: PageId -> String
pageURL AbcLouPage = "/"
pageURL SettingsPage = "/settings"

type Model = 
  { page :: Page
  , letters :: Letters
  , sounds :: Sounds
  , settings :: Settings
  }

data Page 
  = Loading PageId
  | AbcLou Quiz
  | Settings

type Settings =
  { soundIsEnabled :: IsEnabled
  }

data IsEnabled = Enabled | Disabled

derive instance isEnabledEq :: Eq IsEnabled

instance decodeJsonIsEnabled :: Decode.DecodeJson IsEnabled where
  decodeJson = Right <<< Argonaut.caseJsonString Enabled
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

type Quiz =
  { correct :: Letter
  , letters :: Letters
  , attempt :: Attempt 
  }

newtype Letters = Letters (NonEmptyArray Letter)
derive instance lettersNewtype :: Newtype Letters _

instance decodeJsonLetters :: Decode.DecodeJson Letters where
  decodeJson json = 
    case Decode.decodeJson json of
      Right x ->
        case AN.fromArray x of
          Just v -> Right $ wrap v
          Nothing -> Left "Letters can't be empty"
      Left err -> Left err

instance encodeJsonLetters :: Encode.EncodeJson Letters where
  encodeJson = Encode.encodeJson <<< AN.toArray <<< unwrap

initialState :: PageId -> Model
initialState page =
  { page: Loading page
  , letters: wrap Letter.all
  , sounds: Sounds.def
  , settings: defSettings
  }

defSettings :: Settings
defSettings = { soundIsEnabled: Enabled }

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
  Loaded {page, sounds, settings, maybeQuiz} ->
    let newModel = model { sounds = sounds, settings = settings } in
    case {page, maybeQuiz} of
      { page: AbcLouPage, maybeQuiz: Nothing } -> update nav (NextQuiz Nothing) newModel
      { page: AbcLouPage, maybeQuiz: Just quiz } -> pure newModel { page = AbcLou quiz }
      { page: SettingsPage } -> pure newModel { page = Settings }
  GoTo page -> do
    nav.pushState (unsafeToForeign {}) $ pageURL page
    pure model
  SelectLetter letter -> 
    pure $ answeredCorrectly letter model
  NextQuiz letter -> do
    quiz <- nextQuiz letter model.letters 
    storeItem StoreQuiz $ Just quiz
    pure model { page = AbcLou quiz }
  SettingsAction settingsAction -> do
    let newModel = 
          case settingsAction of
            ToggleSound -> model 
              { settings = model.settings
                  { soundIsEnabled =
                    case model.settings.soundIsEnabled of
                      Enabled -> Disabled
                      Disabled -> Enabled
                  }
              }
    storeItem StoreSettings newModel.settings
    pure newModel

data StoreKey = StoreSettings | StoreQuiz

instance storeKeyShow :: Show StoreKey where
  show StoreSettings = "abclou-settings"
  show StoreQuiz = "abclou-quiz"

loadItem :: forall a. Decode.DecodeJson a => StoreKey -> a -> Effect a
loadItem k def = do
  json <- Storage.getItem (show k) =<< Window.localStorage =<< Web.window
  let maybeV = hush <<< Decode.decodeJson =<< hush <<< Parser.jsonParser =<< json
  pure $ fromMaybe def maybeV

storeItem :: forall a. Encode.EncodeJson a => StoreKey -> a -> Effect Unit
storeItem k x = do
  let json = Argonaut.stringify $ Encode.encodeJson x
  Storage.setItem (show k) json =<< Window.localStorage =<< Web.window

nextQuiz :: Maybe Letter -> Letters -> Effect Quiz
nextQuiz lastAnswer letters = do
  {correct, letters} <- iterateUntil ((_ /= lastAnswer) <<< Just <<< _.correct)
      (Letter.random $ unwrap letters)
  pure {attempt: First, correct, letters: wrap letters}

answeredCorrectly :: Letter -> Model -> Model
answeredCorrectly answer model@{ letters, page : AbcLou quiz }
  | Letter.sameLetter quiz.correct answer = model 
      { page = AbcLou quiz { attempt = Correct quiz.correct }
      , letters = wrap $ updateIf quiz.correct (Letter.adjustFrequency (-2.0)) $ unwrap letters 
      }
  | otherwise = 
      model
        { letters = wrap $ updateIf quiz.correct (Letter.adjustFrequency 5.0) $ unwrap letters
        , page = AbcLou quiz 
            { attempt = case quiz.attempt of
              First -> Second answer
              Second firstAnswer -> Third firstAnswer answer
              a -> a
            }
        }
answeredCorrectly _ model = model

view :: Model -> Widget HTML Action
view { page: Loading page } = Loaded <<< merge { page } <$> viewLoading 
view { page: Settings, settings } = 
  layout
    { backPage: AbcLouPage
    , title: PageTitle "Settings"
    , content: [ SettingsAction <$> viewSettings settings ] 
    , additionalClass: Nothing
    }
view { page: AbcLou quiz, sounds, settings } = 
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
  , backPage :: PageId
  , content :: Array (Widget HTML Action)
  , title :: Title
  }
  -> Widget HTML Action
layout { additionalClass, title, content, backPage } =
  D.div [ P.classList [ Just "app", additionalClass ], P.key $ show backPage ] 
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

viewLoading :: Widget HTML (LoadedData ())
viewLoading = liftAff load <|> D.text "..."
  where
    load :: Aff (LoadedData ())
    load = do
      sounds <- Sounds.load
      liftEffect $ Keyboard.startListening
      settings <- liftEffect $ loadItem StoreSettings defSettings
      maybeQuiz <- liftEffect $ loadItem StoreQuiz Nothing
      pure {sounds, settings, maybeQuiz}

viewSettings :: Settings -> Widget HTML SettingsAction
viewSettings { soundIsEnabled }=
  viewSettings'
  where
    viewSettings' =
      D.div [ P.className "settings-content" ]
        [ ToggleSound <$ D.input 
            [ P._type "checkbox"
            , P.name "sound"
            , P.checked (soundIsEnabled == Enabled)
            , P.onChange
            ]
        , D.label
            [ P.htmlFor "sound" ]
            [ D.text $ case soundIsEnabled of
                Enabled -> "Sound on"
                Disabled -> "Sound off"
            ]
        , D.ul [ P.className "settings-info" ]
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
  pure $ NextQuiz $ Just letter
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
mkSoundPlayer sounds Enabled key = Sounds.play key sounds
