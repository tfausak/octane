module Octane.Parser.Garage where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Octane.Data as Data


type Body = Text.Text
type Decal = Text.Text
type Wheels = Text.Text
type RocketTrail = Text.Text
type Antenna = Text.Text
type Topper = Text.Text
type Finish = Text.Text


getDefaultItem :: String -> Int -> Text.Text
getDefaultItem itemType itemId =
    ["Unknown", itemType, show itemId] & unwords & Text.pack


getItem :: Bimap.Bimap Int Text.Text -> String -> Int -> Text.Text
getItem items itemType itemId = items
    & Bimap.lookup itemId
    & Maybe.fromMaybe (getDefaultItem itemType itemId)


getBody :: Int -> Body
getBody = getItem Data.bodies "body"


getDecal :: Int -> Decal
getDecal = getItem Data.decals "decal"


getWheels :: Int -> Wheels
getWheels = getItem Data.wheels "wheels"


getRocketTrail :: Int -> RocketTrail
getRocketTrail = getItem Data.rocketTrails "rocket trail"


getAntenna :: Int -> Antenna
getAntenna = getItem Data.antennas "antenna"


getTopper :: Int -> Topper
getTopper = getItem Data.toppers "topper"


getFinish :: Int -> Maybe Finish
getFinish finishId = Bimap.lookup finishId Data.finishes
