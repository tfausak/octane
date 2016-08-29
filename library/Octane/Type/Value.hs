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

instance DeepSeq.NFData BooleanValue where


newtype ByteValue = ByteValue
    { byteValueUnpack :: Word8.Word8
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ByteValue where


data CamSettingsValue = CamSettingsValue
    { camSettingsValueFov :: Float32.Float32
    , camSettingsValueHeight :: Float32.Float32
    , camSettingsValueAngle :: Float32.Float32
    , camSettingsValueDistance :: Float32.Float32
    , camSettingsValueStiffness :: Float32.Float32
    , camSettingsValueSwivelSpeed :: Float32.Float32
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData CamSettingsValue where


data DemolishValue = DemolishValue
    { demolishValueAttackerFlag :: Boolean.Boolean
    , demolishValueAttackerActorId :: Word32.Word32
    , demolishValueVictimFlag :: Boolean.Boolean
    , demolishValueVictimActorId :: Word32.Word32
    , demolishValueAttackerVelocity :: Vector.Vector Int
    , demolishValueVictimVelocity :: Vector.Vector Int
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData DemolishValue where


-- TODO: What do these fields represent?
data EnumValue = EnumValue
    { enumValueX :: Word16.Word16
    , enumValueY :: Boolean.Boolean
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData EnumValue where


-- TODO: What do these fields represent?
data ExplosionValue = ExplosionValue
    { explosionValueX :: Boolean.Boolean -- presence of next field?
    , explosionValueY :: Maybe Int32.Int32 -- actor id?
    , explosionValueZ :: Vector.Vector Int -- position?
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ExplosionValue where


data FlaggedIntValue = FlaggedIntValue
    { flaggedIntValueFlag :: Boolean.Boolean
    , flaggedIntValueInt :: Int32.Int32
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData FlaggedIntValue where


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

instance DeepSeq.NFData LoadoutValue where


newtype LoadoutOnlineValue = LoadoutOnlineValue
    { loadoutOnlineValueUnpack :: [[(Word32.Word32, CompressedWord.CompressedWord)]]
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LoadoutOnlineValue where


newtype LocationValue = LocationValue
    { locationValueUnpack :: Vector.Vector Int
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LocationValue where


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
    | VMusicStinger
        Boolean.Boolean
        Word32.Word32
        Word8.Word8
    | VPickup
        Boolean.Boolean
        (Maybe Word32.Word32)
        Boolean.Boolean
    | VPrivateMatchSettings
        Text.Text
        Word32.Word32
        Word32.Word32
        Text.Text
        Text.Text
        Boolean.Boolean
    | VQWord
        Word64.Word64
    | VRelativeRotation
        (Vector.Vector Float)
    | VReservation
        CompressedWord.CompressedWord
        Word8.Word8
        RemoteId.RemoteId
        (Maybe Word8.Word8)
        (Maybe Text.Text)
        Boolean.Boolean
        Boolean.Boolean
    | VRigidBodyState
        Boolean.Boolean
        (Vector.Vector Int)
        (Vector.Vector Float)
        (Maybe (Vector.Vector Int))
        (Maybe (Vector.Vector Int))
    | VString
        Text.Text
    | VTeamPaint
        Word8.Word8
        Word8.Word8
        Word8.Word8
        Word32.Word32
        Word32.Word32
    | VUniqueId
        Word8.Word8
        RemoteId.RemoteId
        (Maybe Word8.Word8)
    deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecords Default.def
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
    ])

instance DeepSeq.NFData Value where

instance Aeson.ToJSON Value where
    toJSON value = Aeson.object
        [ "Type" .= typeName value
        , "Value" .= jsonValue value
        ]


typeName :: Value -> StrictText.Text
typeName value = case value of
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
    VMusicStinger _ _ _ -> "MusicStinger"
    VPickup _ _ _ -> "Pickup"
    VPrivateMatchSettings _ _ _ _ _ _ -> "PrivateMatchSettings"
    VQWord _ -> "QWord"
    VRelativeRotation _ -> "RelativeRotation"
    VReservation _ _ _ _ _ _ _ -> "Reservation"
    VRigidBodyState _ _ _ _ _ -> "RigidBodyState"
    VString _ -> "String"
    VTeamPaint _ _ _ _ _ -> "Paint"
    VUniqueId _ _ _ -> "UniqueId"


jsonValue :: Value -> Aeson.Value
jsonValue value = case value of
    ValueBoolean x -> Aeson.toJSON (#unpack x)
    ValueByte x -> Aeson.toJSON (#unpack x)
    ValueCamSettings x -> Aeson.object
        [ "FOV" .= #fov x
        , "Height" .= #height x
        , "Angle" .= #angle x
        , "Distance" .= #distance x
        , "Stiffness" .= #stiffness x
        , "SwivelSpeed" .= #swivelSpeed x
        ]
    ValueDemolish x -> Aeson.object
        [ "AttackerFlag" .= #attackerFlag x
        , "AttackerActorId" .= #attackerActorId x
        , "VictimFlag" .= #victimFlag x
        , "VictimActorId" .= #victimActorId x
        , "AttackerVelocity" .= #attackerVelocity x
        , "VictimVelocity" .= #victimVelocity x
        ]
    ValueEnum x -> Aeson.toJSON
        ( #x x
        , #y x
        )
    ValueExplosion x -> Aeson.toJSON
        ( #x x
        , #y x
        , #z x
        )
    ValueFlaggedInt x -> Aeson.object
        [ "Flag" .= #flag x
        , "Int" .= #int x
        ]
    ValueFloat x -> Aeson.toJSON (#unpack x)
    ValueGameMode x -> Aeson.object
        [ "Id" .= #unpack x
        , "Name" .= getGameMode (#unpack x)
        ]
    ValueInt x -> Aeson.toJSON (#unpack x)
    ValueLoadout x -> Aeson.object
        [ "Version" .= #version x
        , "Body" .= Aeson.object
            [ "Id" .= #body x
            , "Name" .= getProduct (#body x)
            ]
        , "Decal" .= Aeson.object
            [ "Id" .= #decal x
            , "Name" .= getProduct (#decal x)
            ]
        , "Wheels" .= Aeson.object
            [ "Id" .= #wheels x
            , "Name" .= getProduct (#wheels x)
            ]
        , "RocketTrail" .= Aeson.object
            [ "Id" .= #rocketTrail x
            , "Name" .= getProduct (#rocketTrail x)
            ]
        , "Antenna" .= Aeson.object
            [ "Id" .= #antenna x
            , "Name" .= getProduct (#antenna x)
            ]
        , "Topper" .= Aeson.object
            [ "Id" .= #topper x
            , "Name" .= getProduct (#topper x)
            ]
        , "Unknown1" .= #unknown1 x
        , "Unknown2" .= #unknown2 x
        ]
    ValueLoadoutOnline x -> Aeson.toJSON (#unpack x)
    ValueLocation x -> Aeson.toJSON (#unpack x)
    VMusicStinger a b c -> Aeson.toJSON (a, b, c)
    VPickup a b c -> Aeson.toJSON (a, b, c)
    VPrivateMatchSettings mutators joinableBy maxPlayers name password x -> Aeson.object
        [ "Mutators" .= mutators
        , "JoinableBy" .= joinableBy
        , "MaxPlayers" .= maxPlayers
        , "Name" .= name
        , "Password" .= password
        , "Unknown" .= x
        ]
    VQWord x -> Aeson.toJSON x
    VRelativeRotation x -> Aeson.toJSON x
    VReservation num systemId remoteId localId name x y -> Aeson.object
        [ "Number" .= num
        , "SystemId" .= systemId
        , "RemoteId" .= remoteId
        , "LocalId" .= localId
        , "Name" .= name
        , "Unknown1" .= x
        , "Unknown2" .= y
        ]
    VRigidBodyState sleeping position rotation linear angular -> Aeson.object
        [ "Sleeping" .= sleeping
        , "Position" .= position
        , "Rotation" .= rotation
        , "LinearVelocity" .= linear
        , "AngularVelocity" .= angular
        ]
    VString x -> Aeson.toJSON x
    VTeamPaint team color1 color2 finish1 finish2 -> Aeson.object
        [ "Team" .= team
        , "PrimaryColor" .= color1
        , "AccentColor" .= color2
        , "PrimaryFinish" .= Aeson.object
            [ "Id" .= finish1
            , "Name" .= getProduct finish1
            ]
        , "AccentFinish" .= Aeson.object
            [ "Id" .= finish2
            , "Name" .= getProduct finish2
            ]
        ]
    VUniqueId systemId remoteId localId -> Aeson.object
        [ "System" .= case systemId of
            0 -> "Local"
            1 -> "Steam"
            2 -> "PlayStation"
            4 -> "Xbox"
            _ -> "Unknown system " ++ show systemId
        , "Remote" .= remoteId
        , "Local" .= localId
        ]


getGameMode :: Word8.Word8 -> Maybe StrictText.Text
getGameMode x = Bimap.lookup (Word8.fromWord8 x) Data.gameModes


getProduct :: Word32.Word32 -> Maybe StrictText.Text
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products
