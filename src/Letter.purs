module Letter 
  ( Letter
  , all
  , character
  , word
  , asset
  , adjustFrequency
  , random
  , find
  , sameLetter
  )
  where

import Prelude

import Assets as Assets

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as AN
import Data.Foldable as F
import Data.Maybe as M
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.NonEmpty as SN
import Data.String.NonEmpty.CodeUnits as SNC
import Data.String.CodeUnits as SCU
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))

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

find :: String -> NonEmptyArray Letter -> Maybe Letter
find str = F.find ((_ == S.toUpper str) <<< character)

sameLetter :: Letter -> Letter -> Boolean
sameLetter a b = character a == character b

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
a = mkLetter (SN.nes (SProxy :: SProxy "Aff")) Assets.monkey

b :: Letter
b = mkLetter (SN.nes (SProxy :: SProxy "Bär")) Assets.bear

c :: Letter
c = mkLetter (SN.nes (SProxy :: SProxy "Clown")) Assets.clown

d :: Letter
d = mkLetter (SN.nes (SProxy :: SProxy "Dame")) Assets.lady

e :: Letter
e = mkLetter (SN.nes (SProxy :: SProxy "Elch")) Assets.elk

f :: Letter
f = mkLetter (SN.nes (SProxy :: SProxy "Fuchs")) Assets.fox

g :: Letter
g = mkLetter (SN.nes (SProxy :: SProxy "Giraffe")) Assets.giraffe

h :: Letter
h = mkLetter (SN.nes (SProxy :: SProxy "Hund")) Assets.dog

i :: Letter
i = mkLetter (SN.nes (SProxy :: SProxy "Igel")) Assets.hedgehog

j :: Letter
j = mkLetter (SN.nes (SProxy :: SProxy "Jäger")) Assets.hunter

k :: Letter
k = mkLetter (SN.nes (SProxy :: SProxy "Karate")) Assets.karate

l :: Letter
l = mkLetter (SN.nes (SProxy :: SProxy "Lache")) Assets.laugh

m :: Letter
m = mkLetter (SN.nes (SProxy :: SProxy "Mama")) Assets.mama

n :: Letter
n = mkLetter (SN.nes (SProxy :: SProxy "Nase")) Assets.nose

o :: Letter
o = mkLetter (SN.nes (SProxy :: SProxy "Ohr")) Assets.ear

p :: Letter
p = mkLetter (SN.nes (SProxy :: SProxy "Papa")) Assets.papa

q :: Letter
q = mkLetter (SN.nes (SProxy :: SProxy "Quack")) Assets.quack

r :: Letter
r = mkLetter (SN.nes (SProxy :: SProxy "Raggete")) Assets.rocket

s :: Letter
s = mkLetter (SN.nes (SProxy :: SProxy "Stern")) Assets.star

t :: Letter
t = mkLetter (SN.nes (SProxy :: SProxy "Tanze")) Assets.dancing

u :: Letter
u = mkLetter (SN.nes (SProxy :: SProxy "Uhu")) Assets.uhu

v :: Letter
v = mkLetter (SN.nes (SProxy :: SProxy "Velo")) Assets.bicycle

w :: Letter
w = mkLetter (SN.nes (SProxy :: SProxy "Winter")) Assets.winter

x :: Letter
x = mkLetter (SN.nes (SProxy :: SProxy "Xylophone")) Assets.xylophone

y :: Letter
y = mkLetter (SN.nes (SProxy :: SProxy "Yak")) Assets.yak

z :: Letter
z = mkLetter (SN.nes (SProxy :: SProxy "Zug")) Assets.train

mkLetter :: SN.NonEmptyString -> Assets.Asset -> Letter
mkLetter word asset = Letter
  { word , asset , frequency: 1.0 }
