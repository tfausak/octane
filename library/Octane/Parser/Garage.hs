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


getBody :: Type.Word32 -> Maybe Body
getBody bodyId = Bimap.lookup (Type.fromWord32 bodyId) Data.bodies


getDecal :: Type.Word32 -> Maybe Decal
getDecal decalId = Bimap.lookup (Type.fromWord32 decalId) Data.decals


getWheels :: Type.Word32 -> Maybe Wheels
getWheels wheelsId = Bimap.lookup (Type.fromWord32 wheelsId) Data.wheels


getRocketTrail :: Type.Word32 -> Maybe RocketTrail
getRocketTrail rocketTrailId = Bimap.lookup (Type.fromWord32 rocketTrailId) Data.rocketTrails


getAntenna :: Type.Word32 -> Maybe Antenna
getAntenna antennaId = Bimap.lookup (Type.fromWord32 antennaId) Data.antennas


getTopper :: Type.Word32 -> Maybe Topper
getTopper topperId = Bimap.lookup (Type.fromWord32 topperId) Data.toppers


getFinish :: Type.Word32 -> Maybe Finish
getFinish finishId = Bimap.lookup (Type.fromWord32 finishId) Data.finishes
