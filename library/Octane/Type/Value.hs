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
import Octane.Type.Value.BooleanValue
import Octane.Type.Value.ByteValue
import Octane.Type.Value.CamSettingsValue
import Octane.Type.Value.DemolishValue

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

getGameMode :: Word8.Word8 -> Maybe StrictText.Text
getGameMode x = Bimap.lookup (Word8.fromWord8 x) Data.gameModes

getProduct :: Word32.Word32 -> Maybe StrictText.Text
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products

data EnumValue = EnumValue
  { enumValueValue :: Word16.Word16
  , enumValueFlag :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData EnumValue

instance Aeson.ToJSON EnumValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Enum" :: StrictText.Text)
      , "Value" .= Aeson.object ["Value" .= #value x, "Flag" .= #flag x]
      ]

data ExplosionValue = ExplosionValue
  { explosionValueActorless :: Boolean.Boolean
  , explosionValueActorId :: Maybe Int32.Int32
  , explosionValuePosition :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ExplosionValue

instance Aeson.ToJSON ExplosionValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Explosion" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Actorless" .= #actorless x
          , "ActorId" .= #actorId x
          , "Position" .= #position x
          ]
      ]

data FlaggedIntValue = FlaggedIntValue
  { flaggedIntValueFlag :: Boolean.Boolean
  , flaggedIntValueInt :: Int32.Int32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData FlaggedIntValue

instance Aeson.ToJSON FlaggedIntValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("FlaggedInt" :: StrictText.Text)
      , "Value" .= Aeson.object ["Flag" .= #flag x, "Int" .= #int x]
      ]

newtype FloatValue = FloatValue
  { floatValueUnpack :: Float32.Float32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData FloatValue

instance Aeson.ToJSON FloatValue where
  toJSON x =
    Aeson.object ["Type" .= ("Float" :: StrictText.Text), "Value" .= #unpack x]

newtype GameModeValue = GameModeValue
  { gameModeValueUnpack :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData GameModeValue

instance Aeson.ToJSON GameModeValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("GameMode" :: StrictText.Text)
      , "Value" .=
        Aeson.object ["Id" .= #unpack x, "Name" .= getGameMode (#unpack x)]
      ]

newtype IntValue = IntValue
  { intValueUnpack :: Int32.Int32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData IntValue

instance Aeson.ToJSON IntValue where
  toJSON x =
    Aeson.object ["Type" .= ("Int" :: StrictText.Text), "Value" .= #unpack x]

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

instance Aeson.ToJSON LoadoutValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Loadout" :: StrictText.Text)
      , "Value" .=
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
      ]

newtype LoadoutOnlineValue = LoadoutOnlineValue
  { loadoutOnlineValueUnpack :: [[(Word32.Word32, CompressedWord.CompressedWord)]]
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LoadoutOnlineValue

instance Aeson.ToJSON LoadoutOnlineValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("OnlineLoadout" :: StrictText.Text), "Value" .= #unpack x]

newtype LocationValue = LocationValue
  { locationValueUnpack :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LocationValue

instance Aeson.ToJSON LocationValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("Position" :: StrictText.Text), "Value" .= #unpack x]

data MusicStingerValue = MusicStingerValue
  { musicStingerValueFlag :: Boolean.Boolean
  , musicStingerValueCue :: Word32.Word32
  , musicStingerValueTrigger :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData MusicStingerValue

instance Aeson.ToJSON MusicStingerValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("MusicStinger" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          ["Flag" .= #flag x, "Cue" .= #cue x, "Trigger" .= #trigger x]
      ]

data PickupValue = PickupValue
  { pickupValueHasInstigator :: Boolean.Boolean
  , pickupValueInstigatorId :: Maybe Word32.Word32
  , pickupValuePickedUp :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData PickupValue

instance Aeson.ToJSON PickupValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Pickup" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "HasInstigator" .= #hasInstigator x
          , "InstigatorId" .= #instigatorId x
          , "PickedUp" .= #pickedUp x
          ]
      ]

data PrivateMatchSettingsValue = PrivateMatchSettingsValue
  { privateMatchSettingsValueMutators :: Text.Text
  , privateMatchSettingsValueJoinableBy :: Word32.Word32
  , privateMatchSettingsValueMaxPlayers :: Word32.Word32
  , privateMatchSettingsValueGameName :: Text.Text
  , privateMatchSettingsValuePassword :: Text.Text
  , privateMatchSettingsValueFlag :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData PrivateMatchSettingsValue

instance Aeson.ToJSON PrivateMatchSettingsValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("PrivateMatchSettings" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Mutators" .= #mutators x
          , "JoinableBy" .= #joinableBy x
          , "MaxPlayers" .= #maxPlayers x
          , "Name" .= #gameName x
          , "Password" .= #password x
          , "Unknown" .= #flag x
          ]
      ]

newtype QWordValue = QWordValue
  { qWordValueUnpack :: Word64.Word64
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData QWordValue

instance Aeson.ToJSON QWordValue where
  toJSON x =
    Aeson.object ["Type" .= ("QWord" :: StrictText.Text), "Value" .= #unpack x]

newtype RelativeRotationValue = RelativeRotationValue
  { relativeRotationValueUnpack :: Vector.Vector Float
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RelativeRotationValue

instance Aeson.ToJSON RelativeRotationValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("RelativeRotation" :: StrictText.Text), "Value" .= #unpack x]

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

instance Aeson.ToJSON ReservationValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Reservation" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Number" .= #number x
          , "SystemId" .= #systemId x
          , "RemoteId" .= #remoteId x
          , "LocalId" .= #localId x
          , "Name" .= #playerName x
          , "Unknown1" .= #unknown1 x
          , "Unknown2" .= #unknown2 x
          ]
      ]

data RigidBodyStateValue = RigidBodyStateValue
  { rigidBodyStateValueSleeping :: Boolean.Boolean
  , rigidBodyStateValuePosition :: Vector.Vector Int
  , rigidBodyStateValueRotation :: Vector.Vector Float
  , rigidBodyStateValueLinearVelocity :: Maybe (Vector.Vector Int)
  , rigidBodyStateValueAngularVelocity :: Maybe (Vector.Vector Int)
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RigidBodyStateValue

instance Aeson.ToJSON RigidBodyStateValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("RigidBodyState" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Sleeping" .= #sleeping x
          , "Position" .= #position x
          , "Rotation" .= #rotation x
          , "LinearVelocity" .= #linearVelocity x
          , "AngularVelocity" .= #angularVelocity x
          ]
      ]

newtype StringValue = StringValue
  { stringValueUnpack :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData StringValue

instance Aeson.ToJSON StringValue where
  toJSON x =
    Aeson.object ["Type" .= ("String" :: StrictText.Text), "Value" .= #unpack x]

data TeamPaintValue = TeamPaintValue
  { teamPaintValueTeam :: Word8.Word8
  , teamPaintValuePrimaryColor :: Word8.Word8
  , teamPaintValueAccentColor :: Word8.Word8
  , teamPaintValuePrimaryFinish :: Word32.Word32
  , teamPaintValueAccentFinish :: Word32.Word32
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData TeamPaintValue

instance Aeson.ToJSON TeamPaintValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Paint" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Team" .= #team x
          , "PrimaryColor" .= #primaryColor x
          , "AccentColor" .= #accentColor x
          , "PrimaryFinish" .=
            Aeson.object
              [ "Id" .= #primaryFinish x
              , "Name" .= getProduct (#primaryFinish x)
              ]
          , "AccentFinish" .=
            Aeson.object
              ["Id" .= #accentFinish x, "Name" .= getProduct (#accentFinish x)]
          ]
      ]

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
    [ ''EnumValue
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
