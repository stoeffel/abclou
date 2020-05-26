module Assets 
  ( for
  , Asset
  , tada
  , nope
  , correct
  , bear
  , bicycle
  , clown
  , dancing
  , dog
  , ear
  , elk
  , fox
  , giraffe
  , hedgehog
  , hunter
  , karate
  , dolphin
  , laugh
  , mama
  , monkey
  , nose
  , papa
  , quack
  , quackSound
  , rocket
  , star
  , train
  , uhu
  , winter
  , xylophone
  , xylophoneSound
  , yak
  ) where

import Prelude
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Decode as Decode
import Data.Argonaut.Encode as Encode
import Data.Newtype

foreign import forAsset :: String -> String

newtype Asset = Asset String

derive instance assetEq :: Eq Asset
derive instance assetOrd :: Ord Asset
derive instance assetNewtype :: Newtype Asset _

instance decodeJsonAsset :: Decode.DecodeJson Asset where
  decodeJson = map wrap <<< Decode.decodeJson

instance encodeJsonAsset :: Encode.EncodeJson Asset where
  encodeJson = Encode.encodeJson <<< unwrap

for :: Asset -> String
for (Asset name) = forAsset name

tada :: Asset
tada = Asset "tada"

nope :: Asset
nope = Asset "nope"

correct :: Asset
correct = Asset "correct"


bear :: Asset
bear = Asset "Bear"

bicycle :: Asset
bicycle = Asset "Bicycle"

clown :: Asset
clown = Asset "Clown"

dancing :: Asset
dancing = Asset "Dancing"

dog :: Asset
dog = Asset "Dog"

ear :: Asset
ear = Asset "Ear"

elk :: Asset
elk = Asset "Elk"

fox :: Asset
fox = Asset "Fox"

giraffe :: Asset
giraffe = Asset "Giraffe"

hedgehog :: Asset
hedgehog = Asset "Hedgehog"

hunter :: Asset
hunter = Asset "Hunter"

karate :: Asset
karate = Asset "Karate"

dolphin :: Asset
dolphin = Asset "Dolphin"

laugh :: Asset
laugh = Asset "Laugh"

mama :: Asset
mama = Asset "Mama"

monkey :: Asset
monkey = Asset "Monkey"

nose :: Asset
nose = Asset "Nose"

papa :: Asset
papa = Asset "Papa"

quack :: Asset
quack = Asset "Quack"

quackSound :: Asset
quackSound = Asset "Quack-sound"

rocket :: Asset
rocket = Asset "Rocket"

train :: Asset
train = Asset "Train"

uhu :: Asset
uhu = Asset "Uhu"

winter :: Asset
winter = Asset "Winter"

xylophone :: Asset
xylophone = Asset "Xylophone"

xylophoneSound :: Asset
xylophoneSound = Asset "Xylophone-sound"

yak :: Asset
yak = Asset "Yak"

star :: Asset
star = Asset "Star"
