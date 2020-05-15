module Main where

import Prelude
import Assets as Assets

import Data.Array as A
import Data.Array.NonEmpty ((!!), NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.Tuple (Tuple(..), snd)
import Data.String as S
import Data.String.NonEmpty as SN
import Data.String.NonEmpty.CodeUnits as SNC
import Data.String.CodeUnits as SCU
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Unit

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import CSS as CSS
import CSS.Common (center) as CSS
import CSS.Flexbox as FB

import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Console (log)
import Effect.Random (randomInt)
import Effect.Random.Extra (randomWeighted, randomUniqElements)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data Action
  = Initialize
  | InitializeCorrectForDebugging
  | SelectLetter Letter

type Model = 
  { game :: Game
  , letters :: NonEmptyArray Letter
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

type Letter =
  { character :: Char
  , word :: String
  , asset :: Assets.Asset
  , frequency :: Number
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
  | a == b = Wrong
  | otherwise = Enabled
attemptToState (Third a b) c
  | a == c = Disabled
  | b == c = Wrong
  | otherwise = Enabled

component :: forall q i m . H.Component HH.HTML q i m Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize}
    }

initialState :: forall i. i -> Model
initialState _ = { game: NotStarted, letters: initialLetters }

initialLetters :: NonEmptyArray Letter
initialLetters = 
  let
    mkLetter :: SN.NonEmptyString -> Assets.Asset -> Letter
    mkLetter word asset =
      { character: (SNC.uncons word).head
      , word: SN.toString word
      , asset
      , frequency: 1.0 
      }
    a :: Letter
    a = mkLetter (SN.nes (SProxy :: SProxy "Aff")) Assets.aff
   in AN.cons' a $
     [ mkLetter (SN.nes (SProxy :: SProxy "Bär")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Clown")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Dame")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Elch")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Fuchs")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Giraffe")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Hund")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Igel")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Jäger")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Karate")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Lache")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Mama")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Nase")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Ohr")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Papa")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Quack")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Raggete")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Stern")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Tanze")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Uhu")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Velo")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Winter")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Xylophone")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Yak")) Assets.aff
     , mkLetter (SN.nes (SProxy :: SProxy "Zug")) Assets.aff
    ]


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
        [ HH.text letter.word ]
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
viewWordImage {word, asset} =
  HH.img
    [ HP.alt word
    , HP.title word
    , HP.src (Assets.for asset)
    , HC.style $ do
        CSS.height $ CSS.px 400.0
        CSS.width $ CSS.px 400.0
    ]

viewLetter :: forall c m. Attempts -> Letter -> View c m
viewLetter attempt letter@{character} =
  let state = attemptToState attempt letter in
  HH.button
    [ HP.title (SCU.singleton character)
    , HP.enabled (state == Enabled)
    , HP.id_ (SCU.singleton character)
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
        case state of
          Disabled -> CSS.backgroundColor color2
          Enabled -> CSS.backgroundColor color1
          Wrong -> CSS.backgroundColor color2
    , HE.onClick \_ -> Just (SelectLetter letter)
    ]
    [ HH.text (SCU.singleton character) ]


handleAction ::  forall c m. Action -> H.HalogenM Model Action c m Aff Unit
handleAction = case _ of
  Initialize -> do
    {letters} <- H.get
    game <- H.liftEffect (newGame letters)
    H.modify_ _ { game = game }
  InitializeCorrectForDebugging -> do
    {letters} <- H.get
    let a = AN.head letters
    H.modify_ _ { game = Correct a }
  SelectLetter letter -> do
    {game, letters} <- H.modify (answeredCorrectly letter)
    case game of
      Correct _ -> do
         finally <- H.liftAff $ H.liftEffect (newGame letters) <* delay (Milliseconds 1500.0)
         H.modify_ _ { game = finally }
      _ -> pure unit

answeredCorrectly :: Letter -> Model -> Model
answeredCorrectly answer model@{ letters, game : Started attempt quiz }
  | quiz.correct.character == answer.character =
      model { game = Correct answer, letters = updateFrequency (-2.0) quiz.correct <$> letters }
  | otherwise = 
      model
        { letters = updateFrequency 5.0 quiz.correct <$> letters
        , game = flip Started quiz $
            case attempt of
              First -> Second answer
              Second firstAnswer -> Third firstAnswer answer
              a -> a
        }
answeredCorrectly _ model = model

updateFrequency :: Number -> Letter -> Letter -> Letter
updateFrequency delta a b
  | a == b = b { frequency = clamp 1.0 5.0 $ b.frequency + delta }
  | otherwise = b

newGame :: NonEmptyArray Letter -> Effect Game
newGame letters = Started First <$> randomQuiz letters

randomQuiz :: NonEmptyArray Letter -> Effect Quiz
randomQuiz letters = do
  let letterA = AN.singleton $ AN.head letters
  let toFrequencyTuple x@{frequency} = Tuple frequency x
  letters <- M.fromMaybe letterA <$> randomUniqElements 3 (toFrequencyTuple <$> letters)
  correct <- randomWeighted (toFrequencyTuple <$> letters)
  pure { correct, letters }

