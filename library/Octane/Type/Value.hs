{-# LANGUAGE StrictData #-}

module Octane.Type.Value
  ( Value(..)
  , module Octane.Type.Value.BooleanValue
  , module Octane.Type.Value.ByteValue
  , module Octane.Type.Value.CamSettingsValue
  , module Octane.Type.Value.ClubColorsValue
  , module Octane.Type.Value.DemolishValue
  , module Octane.Type.Value.EnumValue
  , module Octane.Type.Value.ExplosionValue
  , module Octane.Type.Value.FlaggedIntValue
  , module Octane.Type.Value.FloatValue
  , module Octane.Type.Value.GameModeValue
  , module Octane.Type.Value.IntValue
  , module Octane.Type.Value.LoadoutOnlineValue
  , module Octane.Type.Value.LoadoutsOnlineValue
  , module Octane.Type.Value.LoadoutsValue
  , module Octane.Type.Value.LoadoutValue
  , module Octane.Type.Value.LocationValue
  , module Octane.Type.Value.MusicStingerValue
  , module Octane.Type.Value.PickupValue
  , module Octane.Type.Value.PrivateMatchSettingsValue
  , module Octane.Type.Value.QWordValue
  , module Octane.Type.Value.ReservationValue
  , module Octane.Type.Value.RigidBodyStateValue
  , module Octane.Type.Value.StringValue
  , module Octane.Type.Value.TeamPaintValue
  , module Octane.Type.Value.UniqueIdValue
  , module Octane.Type.Value.WeldedInfoValue
  ) where

import Octane.Type.Value.BooleanValue
import Octane.Type.Value.ByteValue
import Octane.Type.Value.CamSettingsValue
import Octane.Type.Value.ClubColorsValue
import Octane.Type.Value.DemolishValue
import Octane.Type.Value.EnumValue
import Octane.Type.Value.ExplosionValue
import Octane.Type.Value.FlaggedIntValue
import Octane.Type.Value.FloatValue
import Octane.Type.Value.GameModeValue
import Octane.Type.Value.IntValue
import Octane.Type.Value.LoadoutOnlineValue
import Octane.Type.Value.LoadoutsOnlineValue
import Octane.Type.Value.LoadoutsValue
import Octane.Type.Value.LoadoutValue
import Octane.Type.Value.LocationValue
import Octane.Type.Value.MusicStingerValue
import Octane.Type.Value.PickupValue
import Octane.Type.Value.PrivateMatchSettingsValue
import Octane.Type.Value.QWordValue
import Octane.Type.Value.ReservationValue
import Octane.Type.Value.RigidBodyStateValue
import Octane.Type.Value.StringValue
import Octane.Type.Value.TeamPaintValue
import Octane.Type.Value.UniqueIdValue
import Octane.Type.Value.WeldedInfoValue

import qualified Data.Aeson as Aeson

-- | A replicated property's value.
data Value
  = ValueBoolean BooleanValue
  | ValueByte ByteValue
  | ValueCamSettings CamSettingsValue
  | ValueClubColors ClubColorsValue
  | ValueDemolish DemolishValue
  | ValueEnum EnumValue
  | ValueExplosion ExplosionValue
  | ValueFlaggedInt FlaggedIntValue
  | ValueFloat FloatValue
  | ValueGameMode GameModeValue
  | ValueInt IntValue
  | ValueLoadout LoadoutValue
  | ValueLoadoutOnline LoadoutOnlineValue
  | ValueLoadouts LoadoutsValue
  | ValueLoadoutsOnline LoadoutsOnlineValue
  | ValueLocation LocationValue
  | ValueMusicStinger MusicStingerValue
  | ValuePickup PickupValue
  | ValuePrivateMatchSettings PrivateMatchSettingsValue
  | ValueQWord QWordValue
  | ValueReservation ReservationValue
  | ValueRigidBodyState RigidBodyStateValue
  | ValueString StringValue
  | ValueTeamPaint TeamPaintValue
  | ValueUniqueId UniqueIdValue
  | ValueWeldedInfo WeldedInfoValue
  deriving (Eq, Show)

instance Aeson.ToJSON Value where
  toJSON value =
    case value of
      ValueBoolean x -> Aeson.toJSON x
      ValueByte x -> Aeson.toJSON x
      ValueCamSettings x -> Aeson.toJSON x
      ValueClubColors x -> Aeson.toJSON x
      ValueDemolish x -> Aeson.toJSON x
      ValueEnum x -> Aeson.toJSON x
      ValueExplosion x -> Aeson.toJSON x
      ValueFlaggedInt x -> Aeson.toJSON x
      ValueFloat x -> Aeson.toJSON x
      ValueGameMode x -> Aeson.toJSON x
      ValueInt x -> Aeson.toJSON x
      ValueLoadout x -> Aeson.toJSON x
      ValueLoadoutOnline x -> Aeson.toJSON x
      ValueLoadouts x -> Aeson.toJSON x
      ValueLoadoutsOnline x -> Aeson.toJSON x
      ValueLocation x -> Aeson.toJSON x
      ValueMusicStinger x -> Aeson.toJSON x
      ValuePickup x -> Aeson.toJSON x
      ValuePrivateMatchSettings x -> Aeson.toJSON x
      ValueQWord x -> Aeson.toJSON x
      ValueReservation x -> Aeson.toJSON x
      ValueRigidBodyState x -> Aeson.toJSON x
      ValueString x -> Aeson.toJSON x
      ValueTeamPaint x -> Aeson.toJSON x
      ValueUniqueId x -> Aeson.toJSON x
      ValueWeldedInfo x -> Aeson.toJSON x
