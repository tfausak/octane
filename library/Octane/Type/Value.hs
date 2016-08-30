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
  , BooleanValue(..)
  , ByteValue(..)
  , CamSettingsValue(..)
  , DemolishValue(..)
  , EnumValue(..)
  , ExplosionValue(..)
  , FlaggedIntValue(..)
  , FloatValue(..)
  , GameModeValue(..)
  , IntValue(..)
  , LoadoutValue(..)
  , LoadoutOnlineValue(..)
  , LocationValue(..)
  , MusicStingerValue(..)
  , PickupValue(..)
  , PrivateMatchSettingsValue(..)
  , QWordValue(..)
  , RelativeRotationValue(..)
  , ReservationValue(..)
  , RigidBodyStateValue(..)
  , StringValue(..)
  , TeamPaintValue(..)
  , UniqueIdValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Data as Data
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word16 as Word16
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Type.Word8 as Word8

newtype BooleanValue = BooleanValue
  { booleanValueUnpack :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData BooleanValue

newtype ByteValue = ByteValue
  { byteValueUnpack :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ByteValue

data CamSettingsValue = CamSettingsValue
  { camSettingsValueFov :: Float32.Float32
  , camSettingsValueHeight :: Float32.Float32
  , camSettingsValueAngle :: Float32.Float32
  , camSettingsValueDistance :: Float32.Float32
  , camSettingsValueStiffness :: Float32.Float32
  , camSettingsValueSwivelSpeed :: Float32.Float32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData CamSettingsValue

data DemolishValue = DemolishValue
  { demolishValueAttackerFlag :: Boolean.Boolean
  , demolishValueAttackerActorId :: Word32.Word32
  , demolishValueVictimFlag :: Boolean.Boolean
  , demolishValueVictimActorId :: Word32.Word32
  , demolishValueAttackerVelocity :: Vector.Vector Int
  , demolishValueVictimVelocity :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData DemolishValue

data EnumValue = EnumValue
  { enumValueValue :: Word16.Word16
  , enumValueFlag :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData EnumValue

data ExplosionValue = ExplosionValue
  { explosionValueActorless :: Boolean.Boolean
  , explosionValueActorId :: Maybe Int32.Int32
  , explosionValuePosition :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ExplosionValue

data FlaggedIntValue = FlaggedIntValue
  { flaggedIntValueFlag :: Boolean.Boolean
  , flaggedIntValueInt :: Int32.Int32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData FlaggedIntValue

newtype FloatValue = FloatValue
  { floatValueUnpack :: Float32.Float32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData FloatValue

newtype GameModeValue = GameModeValue
  { gameModeValueUnpack :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData GameModeValue

newtype IntValue = IntValue
  { intValueUnpack :: Int32.Int32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData IntValue

data LoadoutValue = LoadoutValue
  { loadoutValueVersion :: Word8.Word8
  , loadoutValueBody :: Word32.Word32
  , loadoutValueDecal :: Word32.Word32
  , loadoutValueWheels :: Word32.Word32
  , loadoutValueRocketTrail :: Word32.Word32
  , loadoutValueAntenna :: Word32.Word32
  , loadoutValueTopper :: Word32.Word32
  , loadoutValueUnknown1 :: Word32.Word32
  , loadoutValueUnknown2 :: Maybe Word32.Word32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LoadoutValue

newtype LoadoutOnlineValue = LoadoutOnlineValue
  { loadoutOnlineValueUnpack :: [[(Word32.Word32, CompressedWord.CompressedWord)]]
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LoadoutOnlineValue

newtype LocationValue = LocationValue
  { locationValueUnpack :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LocationValue

data MusicStingerValue = MusicStingerValue
  { musicStingerValueFlag :: Boolean.Boolean
  , musicStingerValueCue :: Word32.Word32
  , musicStingerValueTrigger :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData MusicStingerValue

data PickupValue = PickupValue
  { pickupValueHasInstigator :: Boolean.Boolean
  , pickupValueInstigatorId :: Maybe Word32.Word32
  , pickupValuePickedUp :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData PickupValue

data PrivateMatchSettingsValue = PrivateMatchSettingsValue
  { privateMatchSettingsValueMutators :: Text.Text
  , privateMatchSettingsValueJoinableBy :: Word32.Word32
  , privateMatchSettingsValueMaxPlayers :: Word32.Word32
  , privateMatchSettingsValueGameName :: Text.Text
  , privateMatchSettingsValuePassword :: Text.Text
  , privateMatchSettingsValueFlag :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData PrivateMatchSettingsValue

newtype QWordValue = QWordValue
  { qWordValueUnpack :: Word64.Word64
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData QWordValue

newtype RelativeRotationValue = RelativeRotationValue
  { relativeRotationValueUnpack :: Vector.Vector Float
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RelativeRotationValue

data ReservationValue = ReservationValue
  { reservationValueNumber :: CompressedWord.CompressedWord
  , reservationValueSystemId :: Word8.Word8
  , reservationValueRemoteId :: RemoteId.RemoteId
  , reservationValueLocalId :: Maybe Word8.Word8
  , reservationValuePlayerName :: Maybe Text.Text
  , reservationValueUnknown1 :: Boolean.Boolean
  , reservationValueUnknown2 :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ReservationValue

data RigidBodyStateValue = RigidBodyStateValue
  { rigidBodyStateValueSleeping :: Boolean.Boolean
  , rigidBodyStateValuePosition :: Vector.Vector Int
  , rigidBodyStateValueRotation :: Vector.Vector Float
  , rigidBodyStateValueLinearVelocity :: Maybe (Vector.Vector Int)
  , rigidBodyStateValueAngularVelocity :: Maybe (Vector.Vector Int)
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RigidBodyStateValue

newtype StringValue = StringValue
  { stringValueUnpack :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData StringValue

data TeamPaintValue = TeamPaintValue
  { teamPaintValueTeam :: Word8.Word8
  , teamPaintValuePrimaryColor :: Word8.Word8
  , teamPaintValueAccentColor :: Word8.Word8
  , teamPaintValuePrimaryFinish :: Word32.Word32
  , teamPaintValueAccentFinish :: Word32.Word32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData TeamPaintValue

data UniqueIdValue = UniqueIdValue
  { uniqueIdValueSystemId :: Word8.Word8
  , uniqueIdValueRemoteId :: RemoteId.RemoteId
  , uniqueIdValueLocalId :: Maybe Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData UniqueIdValue

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
    [ ''BooleanValue
    , ''ByteValue
    , ''CamSettingsValue
    , ''DemolishValue
    , ''EnumValue
    , ''ExplosionValue
    , ''FlaggedIntValue
    , ''FloatValue
    , ''GameModeValue
    , ''IntValue
    , ''LoadoutValue
    , ''LoadoutOnlineValue
    , ''LocationValue
    , ''MusicStingerValue
    , ''PickupValue
    , ''PrivateMatchSettingsValue
    , ''QWordValue
    , ''RelativeRotationValue
    , ''ReservationValue
    , ''RigidBodyStateValue
    , ''StringValue
    , ''TeamPaintValue
    , ''UniqueIdValue
    ])

instance DeepSeq.NFData Value

instance Aeson.ToJSON Value where
  toJSON value =
    Aeson.object ["Type" .= typeName value, "Value" .= jsonValue value]

typeName :: Value -> StrictText.Text
typeName value =
  case value of
    ValueBoolean _ -> "Boolean"
    ValueByte _ -> "Byte"
    ValueCamSettings _ -> "CameraSettings"
    ValueDemolish _ -> "Demolition"
    ValueEnum _ -> "Enum"
    ValueExplosion _ -> "Explosion"
    ValueFlaggedInt _ -> "FlaggedInt"
    ValueFloat _ -> "Float"
    ValueGameMode _ -> "GameMode"
    ValueInt _ -> "Int"
    ValueLoadout _ -> "Loadout"
    ValueLoadoutOnline _ -> "OnlineLoadout"
    ValueLocation _ -> "Position"
    ValueMusicStinger _ -> "MusicStinger"
    ValuePickup _ -> "Pickup"
    ValuePrivateMatchSettings _ -> "PrivateMatchSettings"
    ValueQWord _ -> "QWord"
    ValueRelativeRotation _ -> "RelativeRotation"
    ValueReservation _ -> "Reservation"
    ValueRigidBodyState _ -> "RigidBodyState"
    ValueString _ -> "String"
    ValueTeamPaint _ -> "Paint"
    ValueUniqueId _ -> "UniqueId"

jsonValue :: Value -> Aeson.Value
jsonValue value =
  case value of
    ValueBoolean x -> Aeson.toJSON (#unpack x)
    ValueByte x -> Aeson.toJSON (#unpack x)
    ValueCamSettings x ->
      Aeson.object
        [ "FOV" .= #fov x
        , "Height" .= #height x
        , "Angle" .= #angle x
        , "Distance" .= #distance x
        , "Stiffness" .= #stiffness x
        , "SwivelSpeed" .= #swivelSpeed x
        ]
    ValueDemolish x ->
      Aeson.object
        [ "AttackerFlag" .= #attackerFlag x
        , "AttackerActorId" .= #attackerActorId x
        , "VictimFlag" .= #victimFlag x
        , "VictimActorId" .= #victimActorId x
        , "AttackerVelocity" .= #attackerVelocity x
        , "VictimVelocity" .= #victimVelocity x
        ]
    ValueEnum x -> Aeson.object ["Value" .= #value x, "Flag" .= #flag x]
    ValueExplosion x ->
      Aeson.object
        [ "Actorless" .= #actorless x
        , "ActorId" .= #actorId x
        , "Position" .= #position x
        ]
    ValueFlaggedInt x -> Aeson.object ["Flag" .= #flag x, "Int" .= #int x]
    ValueFloat x -> Aeson.toJSON (#unpack x)
    ValueGameMode x ->
      Aeson.object ["Id" .= #unpack x, "Name" .= getGameMode (#unpack x)]
    ValueInt x -> Aeson.toJSON (#unpack x)
    ValueLoadout x ->
      Aeson.object
        [ "Version" .= #version x
        , "Body" .=
          Aeson.object ["Id" .= #body x, "Name" .= getProduct (#body x)]
        , "Decal" .=
          Aeson.object ["Id" .= #decal x, "Name" .= getProduct (#decal x)]
        , "Wheels" .=
          Aeson.object ["Id" .= #wheels x, "Name" .= getProduct (#wheels x)]
        , "RocketTrail" .=
          Aeson.object
            ["Id" .= #rocketTrail x, "Name" .= getProduct (#rocketTrail x)]
        , "Antenna" .=
          Aeson.object ["Id" .= #antenna x, "Name" .= getProduct (#antenna x)]
        , "Topper" .=
          Aeson.object ["Id" .= #topper x, "Name" .= getProduct (#topper x)]
        , "Unknown1" .= #unknown1 x
        , "Unknown2" .= #unknown2 x
        ]
    ValueLoadoutOnline x -> Aeson.toJSON (#unpack x)
    ValueLocation x -> Aeson.toJSON (#unpack x)
    ValueMusicStinger x ->
      Aeson.object ["Flag" .= #flag x, "Cue" .= #cue x, "Trigger" .= #trigger x]
    ValuePickup x ->
      Aeson.object
        [ "HasInstigator" .= #hasInstigator x
        , "InstigatorId" .= #instigatorId x
        , "PickedUp" .= #pickedUp x
        ]
    ValuePrivateMatchSettings x ->
      Aeson.object
        [ "Mutators" .= #mutators x
        , "JoinableBy" .= #joinableBy x
        , "MaxPlayers" .= #maxPlayers x
        , "Name" .= #gameName x
        , "Password" .= #password x
        , "Unknown" .= #flag x
        ]
    ValueQWord x -> Aeson.toJSON (#unpack x)
    ValueRelativeRotation x -> Aeson.toJSON (#unpack x)
    ValueReservation x ->
      Aeson.object
        [ "Number" .= #number x
        , "SystemId" .= #systemId x
        , "RemoteId" .= #remoteId x
        , "LocalId" .= #localId x
        , "Name" .= #playerName x
        , "Unknown1" .= #unknown1 x
        , "Unknown2" .= #unknown2 x
        ]
    ValueRigidBodyState x ->
      Aeson.object
        [ "Sleeping" .= #sleeping x
        , "Position" .= #position x
        , "Rotation" .= #rotation x
        , "LinearVelocity" .= #linearVelocity x
        , "AngularVelocity" .= #angularVelocity x
        ]
    ValueString x -> Aeson.toJSON (#unpack x)
    ValueTeamPaint x ->
      Aeson.object
        [ "Team" .= #team x
        , "PrimaryColor" .= #primaryColor x
        , "AccentColor" .= #accentColor x
        , "PrimaryFinish" .=
          Aeson.object
            ["Id" .= #primaryFinish x, "Name" .= getProduct (#primaryFinish x)]
        , "AccentFinish" .=
          Aeson.object
            ["Id" .= #accentFinish x, "Name" .= getProduct (#accentFinish x)]
        ]
    ValueUniqueId x ->
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

getGameMode :: Word8.Word8 -> Maybe StrictText.Text
getGameMode x = Bimap.lookup (Word8.fromWord8 x) Data.gameModes

getProduct :: Word32.Word32 -> Maybe StrictText.Text
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products
