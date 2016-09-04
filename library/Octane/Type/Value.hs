{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value
  ( Value(..)
  , module Octane.Type.Value.BooleanValue
  , module Octane.Type.Value.ByteValue
  , module Octane.Type.Value.CamSettingsValue
  , module Octane.Type.Value.DemolishValue
  , module Octane.Type.Value.EnumValue
  , module Octane.Type.Value.ExplosionValue
  , module Octane.Type.Value.FlaggedIntValue
  , module Octane.Type.Value.FloatValue
  , module Octane.Type.Value.GameModeValue
  , module Octane.Type.Value.IntValue
  , module Octane.Type.Value.LoadoutValue
  , module Octane.Type.Value.LoadoutOnlineValue
  , module Octane.Type.Value.LocationValue
  , module Octane.Type.Value.MusicStingerValue
  , module Octane.Type.Value.PickupValue
  , module Octane.Type.Value.PrivateMatchSettingsValue
  , module Octane.Type.Value.QWordValue
  , module Octane.Type.Value.RelativeRotationValue
  , module Octane.Type.Value.ReservationValue
  , module Octane.Type.Value.RigidBodyStateValue
  , module Octane.Type.Value.StringValue
  , module Octane.Type.Value.TeamPaintValue
  , UniqueIdValue(..)
  ) where

import Data.Aeson ((.=))
import Octane.Type.Value.BooleanValue
import Octane.Type.Value.ByteValue
import Octane.Type.Value.CamSettingsValue
import Octane.Type.Value.DemolishValue
import Octane.Type.Value.EnumValue
import Octane.Type.Value.ExplosionValue
import Octane.Type.Value.FlaggedIntValue
import Octane.Type.Value.FloatValue
import Octane.Type.Value.GameModeValue
import Octane.Type.Value.IntValue
import Octane.Type.Value.LoadoutValue
import Octane.Type.Value.LoadoutOnlineValue
import Octane.Type.Value.LocationValue
import Octane.Type.Value.MusicStingerValue
import Octane.Type.Value.PickupValue
import Octane.Type.Value.PrivateMatchSettingsValue
import Octane.Type.Value.QWordValue
import Octane.Type.Value.RelativeRotationValue
import Octane.Type.Value.ReservationValue
import Octane.Type.Value.RigidBodyStateValue
import Octane.Type.Value.StringValue
import Octane.Type.Value.TeamPaintValue

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Word8 as Word8

data UniqueIdValue = UniqueIdValue
  { uniqueIdValueSystemId :: Word8.Word8
  , uniqueIdValueRemoteId :: RemoteId.RemoteId
  , uniqueIdValueLocalId :: Maybe Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData UniqueIdValue

instance Aeson.ToJSON UniqueIdValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("UniqueId" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "System" .=
            case #systemId x of
              0 -> "Local"
              1 -> "Steam"
              2 -> "PlayStation"
              4 -> "Xbox"
              y -> "Unknown system " ++ show y
          , "Remote" .= #remoteId x
          , "Local" .= #localId x
          ]
      ]

-- | A replicated property's value.
data Value
  = ValueBoolean BooleanValue
  | ValueByte ByteValue
  | ValueCamSettings CamSettingsValue
  | ValueDemolish DemolishValue
  | ValueEnum EnumValue
  | ValueExplosion ExplosionValue
  | ValueFlaggedInt FlaggedIntValue
  | ValueFloat FloatValue
  | ValueGameMode GameModeValue
  | ValueInt IntValue
  | ValueLoadout LoadoutValue
  | ValueLoadoutOnline LoadoutOnlineValue
  | ValueLocation LocationValue
  | ValueMusicStinger MusicStingerValue
  | ValuePickup PickupValue
  | ValuePrivateMatchSettings PrivateMatchSettingsValue
  | ValueQWord QWordValue
  | ValueRelativeRotation RelativeRotationValue
  | ValueReservation ReservationValue
  | ValueRigidBodyState RigidBodyStateValue
  | ValueString StringValue
  | ValueTeamPaint TeamPaintValue
  | ValueUniqueId UniqueIdValue
  deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecords
    Default.def
    [ ''UniqueIdValue
    ])

instance DeepSeq.NFData Value

instance Aeson.ToJSON Value where
  toJSON value =
    case value of
      ValueBoolean x -> Aeson.toJSON x
      ValueByte x -> Aeson.toJSON x
      ValueCamSettings x -> Aeson.toJSON x
      ValueDemolish x -> Aeson.toJSON x
      ValueEnum x -> Aeson.toJSON x
      ValueExplosion x -> Aeson.toJSON x
      ValueFlaggedInt x -> Aeson.toJSON x
      ValueFloat x -> Aeson.toJSON x
      ValueGameMode x -> Aeson.toJSON x
      ValueInt x -> Aeson.toJSON x
      ValueLoadout x -> Aeson.toJSON x
      ValueLoadoutOnline x -> Aeson.toJSON x
      ValueLocation x -> Aeson.toJSON x
      ValueMusicStinger x -> Aeson.toJSON x
      ValuePickup x -> Aeson.toJSON x
      ValuePrivateMatchSettings x -> Aeson.toJSON x
      ValueQWord x -> Aeson.toJSON x
      ValueRelativeRotation x -> Aeson.toJSON x
      ValueReservation x -> Aeson.toJSON x
      ValueRigidBodyState x -> Aeson.toJSON x
      ValueString x -> Aeson.toJSON x
      ValueTeamPaint x -> Aeson.toJSON x
      ValueUniqueId x -> Aeson.toJSON x
