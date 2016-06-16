module Octane.Parser.Garage where

import qualified Data.Bimap as Bimap
import qualified Data.Text as Text
import qualified Octane.Data as Data
import qualified Octane.Type as Type


type Body = Text.Text
type Decal = Text.Text
type Wheels = Text.Text
type RocketTrail = Text.Text
type Antenna = Text.Text
type Topper = Text.Text
type Finish = Text.Text


getBody :: Type.Int32 -> Maybe Body
getBody bodyId = Bimap.lookup (Type.fromInt32 bodyId) Data.bodies


getDecal :: Type.Int32 -> Maybe Decal
getDecal decalId = Bimap.lookup (Type.fromInt32 decalId) Data.decals


getWheels :: Type.Int32 -> Maybe Wheels
getWheels wheelsId = Bimap.lookup (Type.fromInt32 wheelsId) Data.wheels


getRocketTrail :: Type.Int32 -> Maybe RocketTrail
getRocketTrail rocketTrailId = Bimap.lookup (Type.fromInt32 rocketTrailId) Data.rocketTrails


getAntenna :: Type.Int32 -> Maybe Antenna
getAntenna antennaId = Bimap.lookup (Type.fromInt32 antennaId) Data.antennas


getTopper :: Type.Int32 -> Maybe Topper
getTopper topperId = Bimap.lookup (Type.fromInt32 topperId) Data.toppers


getFinish :: Type.Int32 -> Maybe Finish
getFinish finishId = Bimap.lookup (Type.fromInt32 finishId) Data.finishes
