module Octane.Parser.Garage where

import qualified Data.Bimap as Bimap
import qualified Data.Text as Text
import qualified Octane.Data as Data


type Body = Text.Text
type Decal = Text.Text
type Wheels = Text.Text
type RocketTrail = Text.Text
type Antenna = Text.Text
type Topper = Text.Text
type Finish = Text.Text


getBody :: Int -> Maybe Body
getBody bodyId = Bimap.lookup bodyId Data.bodies


getDecal :: Int -> Maybe Decal
getDecal decalId = Bimap.lookup decalId Data.decals


getWheels :: Int -> Maybe Wheels
getWheels wheelsId = Bimap.lookup wheelsId Data.wheels


getRocketTrail :: Int -> Maybe RocketTrail
getRocketTrail rocketTrailId = Bimap.lookup rocketTrailId Data.rocketTrails


getAntenna :: Int -> Maybe Antenna
getAntenna antennaId = Bimap.lookup antennaId Data.antennas


getTopper :: Int -> Maybe Topper
getTopper topperId = Bimap.lookup topperId Data.toppers


getFinish :: Int -> Maybe Finish
getFinish finishId = Bimap.lookup finishId Data.finishes
