module Letter 
  ( Letter
  , all
  , character
  , word
  , asset
  , sameCharacter
  , adjustFrequency
  , random
  )
  where

import Prelude

import Assets as Assets

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Maybe as M
import Data.String as S
import Data.String.NonEmpty as SN
import Data.String.NonEmpty.CodeUnits as SNC
import Data.String.CodeUnits as SCU
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)

import Effect (Effect)
import Effect.Random.Extra (randomWeighted, randomUniqElements)

data Letter = Letter
  { word :: SN.NonEmptyString
  , asset :: Assets.Asset
  , frequency :: Number
  }

derive instance letterEq :: Eq Letter

character :: Letter -> String
character (Letter {word}) = SCU.singleton (SNC.uncons word).head

word :: Letter -> String
word (Letter {word}) = SN.toString word

asset :: Letter -> String
asset (Letter {asset}) = Assets.for asset

sameCharacter :: Letter -> Letter -> Boolean
sameCharacter (Letter {word: a}) (Letter {word: b}) = a == b

adjustFrequency :: Number -> Letter -> Letter
adjustFrequency delta (Letter a) =
  Letter a { frequency = clamp 1.0 5.0 $ a.frequency + delta }

random :: NonEmptyArray Letter -> Effect { correct :: Letter, letters :: NonEmptyArray Letter }
random x = do
  letters <- M.fromMaybe (AN.singleton a)
    <$> randomUniqElements 3 (withFrequency <$> x)
  correct <- randomWeighted (withFrequency <$> letters)
  pure { correct, letters }

withFrequency :: Letter -> Tuple Number Letter
withFrequency x@(Letter {frequency}) = Tuple frequency x

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
a = mkLetter (SN.nes (SProxy :: SProxy "Aff")) Assets.aff

b :: Letter
b = mkLetter (SN.nes (SProxy :: SProxy "Bär")) Assets.aff

c :: Letter
c = mkLetter (SN.nes (SProxy :: SProxy "Clown")) Assets.aff

d :: Letter
d = mkLetter (SN.nes (SProxy :: SProxy "Dame")) Assets.aff

e :: Letter
e = mkLetter (SN.nes (SProxy :: SProxy "Elch")) Assets.aff

f :: Letter
f = mkLetter (SN.nes (SProxy :: SProxy "Fuchs")) Assets.aff

g :: Letter
g = mkLetter (SN.nes (SProxy :: SProxy "Giraffe")) Assets.aff

h :: Letter
h = mkLetter (SN.nes (SProxy :: SProxy "Hund")) Assets.aff

i :: Letter
i = mkLetter (SN.nes (SProxy :: SProxy "Igel")) Assets.aff

j :: Letter
j = mkLetter (SN.nes (SProxy :: SProxy "Jäger")) Assets.aff

k :: Letter
k = mkLetter (SN.nes (SProxy :: SProxy "Karate")) Assets.aff

l :: Letter
l = mkLetter (SN.nes (SProxy :: SProxy "Lache")) Assets.aff

m :: Letter
m = mkLetter (SN.nes (SProxy :: SProxy "Mama")) Assets.aff

n :: Letter
n = mkLetter (SN.nes (SProxy :: SProxy "Nase")) Assets.aff

o :: Letter
o = mkLetter (SN.nes (SProxy :: SProxy "Ohr")) Assets.aff

p :: Letter
p = mkLetter (SN.nes (SProxy :: SProxy "Papa")) Assets.aff

q :: Letter
q = mkLetter (SN.nes (SProxy :: SProxy "Quack")) Assets.aff

r :: Letter
r = mkLetter (SN.nes (SProxy :: SProxy "Raggete")) Assets.aff

s :: Letter
s = mkLetter (SN.nes (SProxy :: SProxy "Stern")) Assets.aff

t :: Letter
t = mkLetter (SN.nes (SProxy :: SProxy "Tanze")) Assets.aff

u :: Letter
u = mkLetter (SN.nes (SProxy :: SProxy "Uhu")) Assets.aff

v :: Letter
v = mkLetter (SN.nes (SProxy :: SProxy "Velo")) Assets.aff

w :: Letter
w = mkLetter (SN.nes (SProxy :: SProxy "Winter")) Assets.aff

x :: Letter
x = mkLetter (SN.nes (SProxy :: SProxy "Xylophone")) Assets.aff

y :: Letter
y = mkLetter (SN.nes (SProxy :: SProxy "Yak")) Assets.aff

z :: Letter
z = mkLetter (SN.nes (SProxy :: SProxy "Zug")) Assets.aff

mkLetter :: SN.NonEmptyString -> Assets.Asset -> Letter
mkLetter word asset = Letter
  { word , asset , frequency: 1.0 }
