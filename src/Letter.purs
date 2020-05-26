module Letter 
  ( Letter
  , all
  , character
  , word
  , sound
  , asset
  , adjustFrequency
  , random
  , find
  , sameLetter
  )
  where

import Prelude

import Assets as Assets
import Sounds as Sounds

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Either
import Data.Foldable as F
import Data.Maybe as M
import Data.Maybe (Maybe(..))
import Data.Newtype
import Data.String as S
import Data.String.NonEmpty as SN
import Data.String.NonEmpty.CodeUnits as SNC
import Data.String.CodeUnits as SCU
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Decode as Decode
import Data.Argonaut.Decode ((.:), (.:?))
import Data.Argonaut.Encode as Encode
import Data.Argonaut.Encode ((:=), (~>))

import Effect (Effect)
import Effect.Random.Extra (randomWeighted, randomUniqElements)

newtype Letter = Letter
  { word :: Word
  , asset :: Assets.Asset
  , frequency :: Number
  , sound :: Maybe Sounds.Key
  }

newtype Word = Word SN.NonEmptyString
derive instance wordEq :: Eq Word
derive instance wordNewtype :: Newtype Word _

instance letterEq :: Eq Letter where
  eq (Letter a) (Letter b) = a.word == b.word

instance decodeJsonLetter :: Decode.DecodeJson Letter where
  decodeJson json = do
    x <- Decode.decodeJson json
    word <- x .: "word"
    asset <- x .: "asset"
    frequency <- x .: "frequency"
    sound <- x .: "sound"
    pure $ Letter { word, asset, frequency, sound }

instance encodeJsonLetter :: Encode.EncodeJson Letter where
  encodeJson (Letter x) =
    "word" := x.word 
    ~> "asset" := x.asset 
    ~> "frequency" := x.frequency
    ~> "sound" := x.sound
    ~> Argonaut.jsonEmptyObject

instance decodeJsonWord :: Decode.DecodeJson Word where
  decodeJson json = 
    case Decode.decodeJson json of
      Right x ->
        case SN.fromString x of
          Just v -> Right $ wrap v
          Nothing -> Left "Word can't be empty"
      Left err -> Left err

instance encodeJsonWord :: Encode.EncodeJson Word where
  encodeJson = Encode.encodeJson <<< SN.toString <<< unwrap

character :: Letter -> String
character (Letter letter) = SCU.singleton (SNC.uncons $ unwrap letter.word).head

word :: Letter -> String
word (Letter letter) = S.toUpper $ SN.toString $ unwrap letter.word

asset :: Letter -> String
asset (Letter letter) = Assets.for letter.asset

sound :: Letter -> Maybe Sounds.Key
sound (Letter letter) = letter.sound

adjustFrequency :: Number -> Letter -> Letter
adjustFrequency delta (Letter a') =
  Letter a' { frequency = clamp 1.0 5.0 $ a'.frequency + delta }

random :: NonEmptyArray Letter -> Effect { correct :: Letter, letters :: NonEmptyArray Letter }
random x' = do
  letters <- M.fromMaybe (AN.singleton a)
    <$> randomUniqElements 3 (withFrequency <$> x')
  correct <- randomWeighted (withFrequency <$> letters)
  pure { correct, letters }

withFrequency :: Letter -> Tuple Number Letter
withFrequency x'@(Letter {frequency}) = Tuple frequency x'

find :: String -> NonEmptyArray Letter -> Maybe Letter
find str = F.find ((_ == S.toUpper str) <<< character)

sameLetter :: Letter -> Letter -> Boolean
sameLetter a' b' = character a' == character b'

all :: NonEmptyArray Letter
all = AN.cons' 
    a $
  [ b, c, d, e, f
  , g, h, i, j, k
  , l, m, n, o, p
  , q, r, s, t, u
  , v, w, x, y, z
  ]

a :: Letter
a = mkLetter (SN.nes (SProxy :: SProxy "Aff")) Assets.monkey Nothing

b :: Letter
b = mkLetter (SN.nes (SProxy :: SProxy "Bär")) Assets.bear Nothing

c :: Letter
c = mkLetter (SN.nes (SProxy :: SProxy "Clown")) Assets.clown Nothing

d :: Letter
d = mkLetter (SN.nes (SProxy :: SProxy "Delfin")) Assets.dolphin Nothing

e :: Letter
e = mkLetter (SN.nes (SProxy :: SProxy "Elch")) Assets.elk Nothing

f :: Letter
f = mkLetter (SN.nes (SProxy :: SProxy "Fuchs")) Assets.fox Nothing

g :: Letter
g = mkLetter (SN.nes (SProxy :: SProxy "Giraffe")) Assets.giraffe Nothing

h :: Letter
h = mkLetter (SN.nes (SProxy :: SProxy "Hund")) Assets.dog Nothing

i :: Letter
i = mkLetter (SN.nes (SProxy :: SProxy "Igel")) Assets.hedgehog Nothing

j :: Letter
j = mkLetter (SN.nes (SProxy :: SProxy "Jäger")) Assets.hunter Nothing

k :: Letter
k = mkLetter (SN.nes (SProxy :: SProxy "Karate")) Assets.karate Nothing

l :: Letter
l = mkLetter (SN.nes (SProxy :: SProxy "Lache")) Assets.laugh Nothing

m :: Letter
m = mkLetter (SN.nes (SProxy :: SProxy "Mama")) Assets.mama Nothing

n :: Letter
n = mkLetter (SN.nes (SProxy :: SProxy "Nase")) Assets.nose Nothing

o :: Letter
o = mkLetter (SN.nes (SProxy :: SProxy "Ohr")) Assets.ear Nothing

p :: Letter
p = mkLetter (SN.nes (SProxy :: SProxy "Papa")) Assets.papa Nothing

q :: Letter
q = mkLetter (SN.nes (SProxy :: SProxy "Quack")) Assets.quack $ Just Sounds.Quack

r :: Letter
r = mkLetter (SN.nes (SProxy :: SProxy "Rakete")) Assets.rocket Nothing

s :: Letter
s = mkLetter (SN.nes (SProxy :: SProxy "Stern")) Assets.star Nothing

t :: Letter
t = mkLetter (SN.nes (SProxy :: SProxy "Tanze")) Assets.dancing Nothing

u :: Letter
u = mkLetter (SN.nes (SProxy :: SProxy "Uhu")) Assets.uhu Nothing

v :: Letter
v = mkLetter (SN.nes (SProxy :: SProxy "Velo")) Assets.bicycle Nothing

w :: Letter
w = mkLetter (SN.nes (SProxy :: SProxy "Winter")) Assets.winter Nothing

x :: Letter
x = mkLetter (SN.nes (SProxy :: SProxy "Xylophone")) Assets.xylophone $ Just Sounds.Xylophone

y :: Letter
y = mkLetter (SN.nes (SProxy :: SProxy "Yak")) Assets.yak Nothing

z :: Letter
z = mkLetter (SN.nes (SProxy :: SProxy "Zug")) Assets.train Nothing

mkLetter :: SN.NonEmptyString -> Assets.Asset -> Maybe Sounds.Key -> Letter
mkLetter word' asset' sound' = Letter
  { word: wrap word' , asset: asset' , frequency: 1.0, sound: sound' }
