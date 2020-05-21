module Game where

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
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple))

import Concur.Core.DevTools as CD
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)

import Control.Alt ((<|>))
import Control.Monad.Loops (iterateUntil)

import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (liftAff)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Exception (error)


data Action
  = Initialize
  | SelectLetter Letter
  | NextGame Letter
  {-- | HandleKey H.SubscriptionId KeyboardEvent --}

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
  , sounds :: Sounds
  , fiber :: Maybe (Fiber Game)
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
initialState = { game: NotStarted, letters: Letter.all, sounds: Sounds.def, fiber: Nothing }

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

main :: Effect Unit
main =
  runWidgetInDom "app" $ do
    _ <- liftEffect CD.connectDevTools
    sounds <- liftAff Sounds.load
    game <- liftEffect (newGame Nothing initialState.letters)
    render initialState { game = game, sounds = sounds } 

render :: forall a. Model -> Widget HTML a
render model = do
  action <- view model
  render =<< liftEffect (update action model)

update :: Action -> Model -> Effect Model
update action model = case action of
  SelectLetter letter -> 
    pure $ answeredCorrectly letter model
  NextGame letter -> do
      next <- newGame (Just letter) model.letters 
      pure model { game = next }
  _ -> pure model

view :: Model -> Widget HTML Action
view model@{ game: NotStarted } = container [D.text "Ein Moment..."] 
view { game: Started attempt quiz, sounds } = container [viewQuiz attempt quiz sounds]
view { game: Correct letter, sounds, letters } = container [ viewCorrect letter sounds ]

container :: forall a. Array (Widget HTML a) -> Widget HTML a
container = D.div [ P.className "container" ] <<< A.cons (D.h1 [] [ D.text "ABC LOU" ])

viewQuiz :: Attempts -> Quiz -> Sounds -> Widget HTML Action
viewQuiz attempt quiz sounds = do
  if attempt /= First then
    liftEffect $ Sounds.play sounds.nope
  else pure unit
  D.div [ P.className "quiz" ]
    [ viewWordImage quiz.correct
    , viewLetters attempt quiz.letters
    ]

viewCorrect :: Letter -> Sounds -> Widget HTML Action
viewCorrect letter sounds = do
  liftEffect $ Sounds.play sounds.tada
  liftAff (NextGame letter <$ Aff.delay (Milliseconds 10000.0))
    <|> D.a 
      [ NextGame letter <$ P.onClick
      , P.className "correct"
      ]
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
  D.ul [ P.className "letters" ]
    $ AN.toArray $ viewLetter attempt <$> letters

viewLetter :: Attempts -> Letter -> Widget HTML Action
viewLetter attempt letter =
  let state = attemptToState attempt letter in
  SelectLetter letter <$ D.button
    [ P.title (Letter.character letter)
    , P.disabled (state /= Enabled)
    , P._id (Letter.character letter)
    , P.classList [ Just $ "letter" , Just $ show state ]
    , P.onClick
    ]
    [ D.text (Letter.character letter) ]
