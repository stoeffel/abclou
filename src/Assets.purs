module Assets 
  ( for
  , Asset
  , aff
  ) where

import Prelude

foreign import forAsset :: String -> String

data Asset = Asset String

derive instance assetEq :: Eq Asset
derive instance assetOrd :: Ord Asset

for :: Asset -> String
for (Asset name) = forAsset name

aff :: Asset
aff = Asset "aff"
