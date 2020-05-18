module Main where

import Prelude

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

import Control.Monad.Loops (iterateUntil)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
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

import CSS as CSS
import CSS.Common (center) as CSS
import CSS.Flexbox as FB

import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Console (log)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data Action
  = Initialize
  | SelectLetter Letter
  | HandleKey H.SubscriptionId KeyboardEvent

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
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
initialState _ = { game: NotStarted, letters: Letter.all, sounds: Sounds.def }


type View c m = H.ComponentHTML Action c m

color1 :: CSS.Color
color1 = CSS.rgba 223 124 168 0.94

color2 :: CSS.Color
color2 = CSS.rgba 162 90 122 0.94

color3 :: CSS.Color
color3 = CSS.rgba 131 73 99 1.0

color4 :: CSS.Color
color4 = CSS.rgba 112 77 78 1.0

render :: forall c m. Model -> View c m
render { game: NotStarted } = container [HH.text "Ein Moment..."]
render { game: Started attempt quiz } = container [viewQuiz attempt quiz]
render { game: Correct letter } = container [viewCorrect letter]
render { game: TryAgain _ _ } = container [HH.text "Try again"]

container :: forall c m. Array (View c m) -> View c m
container = 
  HH.div 
  [ HC.style $ do
      CSS.display CSS.flex 
      CSS.alignItems CSS.center 
      CSS.flexDirection CSS.column
      CSS.height $ CSS.pct 100.0
  ]
  <<< A.cons (
    HH.h1 
    [ HC.style $ do
        {-- CSS.color color3 --}
        CSS.fontSize $ CSS.em 4.0
        CSS.marginBottom CSS.nil
    ]
    [ HH.text "ABC LOU" ]
  )

viewQuiz :: forall c m. Attempts -> Quiz -> View c m
viewQuiz attempt quiz =
  HH.div 
    [ HC.style $ do
        CSS.alignItems CSS.stretch 
        CSS.display CSS.flex 
        CSS.flexDirection CSS.column
        CSS.justifyContent CSS.spaceBetween 
        FB.flex 2 0 CSS.nil
    ]
    [ viewWordImage quiz.correct
    , viewLetters attempt quiz.letters
    ]

viewCorrect :: forall c m. Letter -> View c m
viewCorrect letter =
  HH.div 
    [ HC.style $ do
        CSS.alignItems CSS.center 
        CSS.display CSS.flex 
        CSS.flexDirection CSS.column
        CSS.justifyContent CSS.spaceBetween 
    ]
    [ viewWordImage letter
    , HH.h2 
        [ HC.style $ do
            CSS.fontSize $ CSS.em 4.0
        ]
        [ HH.text $ Letter.word letter ]
    ]
viewLetters :: forall c m. Attempts -> NonEmptyArray Letter -> View c m
viewLetters attempt letters =
  HH.ul
    [ HC.style $ do
        CSS.alignItems CSS.stretch
        CSS.display CSS.flex 
        CSS.justifyContent CSS.spaceBetween
        CSS.margin ( CSS.em 1.2 ) CSS.nil CSS.nil CSS.nil
        CSS.padding CSS.nil CSS.nil CSS.nil CSS.nil
        FB.flex 2 0 CSS.nil
    ]
    $ AN.toArray $ viewLetter attempt <$> letters

viewWordImage :: forall c m. Letter -> View c m
viewWordImage letter =
  HH.img
    [ HP.alt $ Letter.word letter
    , HP.title $ Letter.word letter
    , HP.src $ Letter.asset letter
    , HC.style $ do
        CSS.height $ CSS.px 400.0
        CSS.width $ CSS.px 400.0
    ]

viewLetter :: forall c m. Attempts -> Letter -> View c m
viewLetter attempt letter =
  let state = attemptToState attempt letter in
  HH.button
    [ HP.title (Letter.character letter)
    , HP.enabled (state == Enabled)
    , HP.id_ (Letter.character letter)
    , HP.classes 
        [ HH.ClassName "letter"
        , HH.ClassName (show state)
        ]
    , HC.style $ do
        CSS.borderRadius (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) 
        CSS.fontSize $ CSS.em 6.0
        CSS.height $ CSS.pct 80.0
        CSS.margin (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
        CSS.maxHeight $ CSS.px 150.0
        CSS.maxWidth $ CSS.px 150.0
        CSS.width $ CSS.pct 80.0
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

handleSelectLetter :: forall c m. Letter -> H.HalogenM Model Action c m Aff Unit
handleSelectLetter letter = do
  {game, letters, sounds} <- H.modify (answeredCorrectly letter)
  case game of
    Correct answer -> do
        H.liftEffect $ Sounds.play sounds.tada
        finally <- H.liftAff $ H.liftEffect (newGame (Just answer) letters) <* delay (Milliseconds 1500.0)
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
