module Main where

import Prelude

import Assets as Assets
import Letter as Letter
import Letter (Letter)
import Sounds as Sounds
import Sounds (Sounds)

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Functor.Extra (updateIf)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple))

import Control.Monad.Loops (iterateUntil)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)

import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Exception (error)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data Action
  = Initialize
  | SelectLetter Letter
  | Continue
  | HandleKey H.SubscriptionId KeyboardEvent

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
  , fiber :: Maybe (Fiber Game)
  }

data Game 
  = NotStarted
  | Started Attempts Quiz
  | TryAgain Letter Quiz
  | Correct Letter

data Attempts
  = First
  | Second Letter
  | Third Letter Letter

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

component :: forall q i m . H.Component HH.HTML q i m Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize}
    }

initialState :: forall i. i -> Model
initialState _ = { game: NotStarted, letters: Letter.all, sounds: Sounds.def, fiber: Nothing }


type View c m = H.ComponentHTML Action c m

render :: forall c m. Model -> View c m
render { game: NotStarted } = container [HH.text "Ein Moment..."]
render { game: Started attempt quiz } = container [viewQuiz attempt quiz]
render { game: Correct letter } = container [viewCorrect letter]
render { game: TryAgain _ _ } = container [HH.text "Try again"]

container :: forall c m. Array (View c m) -> View c m
container = 
  HH.div [ HP.class_ $ HH.ClassName "container" ]
  <<< A.cons (HH.h1 [] [ HH.text "ABC LOU" ])

viewQuiz :: forall c m. Attempts -> Quiz -> View c m
viewQuiz attempt quiz =
  HH.div [ HP.class_ $ HH.ClassName "quiz" ]
    [ viewWordImage quiz.correct
    , viewLetters attempt quiz.letters
    ]

viewCorrect :: forall c m. Letter -> View c m
viewCorrect letter =
  HH.a 
    [ HE.onClick \_ -> Just Continue 
    , HP.class_ $ HH.ClassName "correct"
    ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "image-container" ]
        [ viewWordImage letter
        , HH.img 
          [ HP.class_ $ HH.ClassName "correct-star"
          , HP.src $ Assets.for Assets.star 
          ]
        ]
    , HH.h2 [] [ HH.text $ Letter.word letter ]
    ]

viewLetters :: forall c m. Attempts -> NonEmptyArray Letter -> View c m
viewLetters attempt letters =
  HK.ul [ HP.class_ $ HH.ClassName "letters" ]
    $ AN.toArray $ viewLetter attempt <$> letters

viewWordImage :: forall c m. Letter -> View c m
viewWordImage letter =
  HH.img
    [ HP.alt $ Letter.word letter
    , HP.title $ Letter.word letter
    , HP.src $ Letter.asset letter
    , HP.height 400
    , HP.width 400
    ]

viewLetter :: forall c m. Attempts -> Letter -> Tuple String (View c m)
viewLetter attempt letter =
  let state = attemptToState attempt letter in
  Tuple (Letter.character letter) $
  HH.button
    [ HP.title (Letter.character letter)
    , HP.enabled (state == Enabled)
    , HP.id_ (Letter.character letter)
    , HP.classes 
        [ HH.ClassName "letter"
        , HH.ClassName (show state)
        ]
    , HE.onClick \_ -> Just (SelectLetter letter)
    ]
    [ HH.text (Letter.character letter) ]


handleAction ::  forall c m. Action -> H.HalogenM Model Action c m Aff Unit
handleAction = case _ of
  Initialize -> do
    {letters} <- H.get
    sounds <- H.liftAff Sounds.load
    game <- H.liftEffect (newGame Nothing letters)
    H.modify_ _ { game = game, sounds = sounds }
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey id ev -> do
     {letters} <- H.get 
     case Letter.find (KE.key ev) letters of
       Just letter -> do
         H.liftEffect $ log $ Letter.word letter
         handleSelectLetter letter
       Nothing -> pure unit
  SelectLetter letter -> handleSelectLetter letter
  Continue -> do
    model <- H.get
    case model.game of
      Correct answer -> do
        H.liftAff $ case model.fiber of
          Nothing -> pure unit
          Just fiber -> Aff.killFiber (error "cancelled") fiber
        game <- H.liftEffect (newGame (Just answer) model.letters)
        H.put model { game = game }
      _ -> pure unit

handleSelectLetter :: forall c m. Letter -> H.HalogenM Model Action c m Aff Unit
handleSelectLetter letter = do
  {game, letters, sounds} <- H.modify (answeredCorrectly letter)
  case game of
    Correct answer -> do
        H.liftEffect $ Sounds.play sounds.tada
        fiber <- H.liftAff $ Aff.forkAff (
            Aff.delay (Milliseconds 10000.0)
            *> H.liftEffect (newGame (Just answer) letters) 
          )
        H.modify_ _ { fiber = Just fiber }
        finally <- H.liftAff $ Aff.joinFiber fiber
        H.modify_ _ { game = finally }
    _ -> H.liftEffect $ Sounds.play sounds.nope

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

newGame :: Maybe Letter -> NonEmptyArray Letter -> Effect Game
newGame lastAnswer letters = 
  Started First 
  <$> iterateUntil ((/=) lastAnswer <<< Just <<< _.correct)
  (Letter.random letters)
