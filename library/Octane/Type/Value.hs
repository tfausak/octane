{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Value (Value(..)) where

import Data.Function ((&))
import Data.Monoid ((<>))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
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


-- TODO: Split these into individual data types like RemoteId.
-- | A replicated property's value.
data Value
    = VBoolean
        Boolean.Boolean
    | VByte
        Word8.Word8
    | VCamSettings
        Float32.Float32
        Float32.Float32
        Float32.Float32
        Float32.Float32
        Float32.Float32
        Float32.Float32
    | VDemolish
        Boolean.Boolean
        Word32.Word32
        Boolean.Boolean
        Word32.Word32
        (Vector.Vector Int)
        (Vector.Vector Int)
    | VEnum
        Word16.Word16
        Boolean.Boolean
    | VExplosion
        Boolean.Boolean
        (Maybe Int32.Int32)
        (Vector.Vector Int)
    | VFlaggedInt
        Boolean.Boolean
        Int32.Int32
    | VFloat
        Float32.Float32
    | VGameMode
        Word8.Word8
    | VInt
        Int32.Int32
    | VLoadout
        Word8.Word8
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        (Maybe Word32.Word32)
    | VLoadoutOnline
        [[(Word32.Word32, CompressedWord.CompressedWord)]]
    | VLocation
        (Vector.Vector Int)
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

instance DeepSeq.NFData Value where

instance Aeson.ToJSON Value where
    toJSON value = case value of
        VBoolean x -> Aeson.toJSON x
        VByte x -> Aeson.toJSON x
        VCamSettings fov height angle distance stiffness swivelSpeed -> Aeson.object
            [ ("FOV", Aeson.toJSON fov)
            , ("Height", Aeson.toJSON height)
            , ("Angle", Aeson.toJSON angle)
            , ("Distance", Aeson.toJSON distance)
            , ("Stiffness", Aeson.toJSON stiffness)
            , ("SwivelSpeed", Aeson.toJSON swivelSpeed)
            ]
        VDemolish a b c d e f -> Aeson.toJSON (a, b, c, d, e, f)
        VEnum x y -> Aeson.toJSON (x, y)
        VExplosion a b c -> Aeson.toJSON (a, b, c)
        VFlaggedInt x y -> Aeson.toJSON (x, y)
        VFloat x -> Aeson.toJSON x
        VGameMode gameMode -> Aeson.object
            [ ("Id", Aeson.toJSON gameMode)
            , ("Name", gameMode & getGameMode & Aeson.toJSON)
            ]
        VInt x -> Aeson.toJSON x
        VLoadout version body decal wheels rocketTrail antenna topper x y -> Aeson.object
            [ ("Version", Aeson.toJSON version)
            , ("Body", Aeson.object
                [ ("Id", Aeson.toJSON body)
                , ("Name", body & getProduct & Aeson.toJSON)
                ])
            , ("Decal", Aeson.object
                [ ("Id", Aeson.toJSON decal)
                , ("Name", decal & getProduct & Aeson.toJSON)
                ])
            , ("Wheels", Aeson.object
                [ ("Id", Aeson.toJSON wheels)
                , ("Name", wheels & getProduct & Aeson.toJSON)
                ])
            , ("RocketTrail", Aeson.object
                [ ("Id", Aeson.toJSON rocketTrail)
                , ("Name", rocketTrail & getProduct & Aeson.toJSON)
                ])
            , ("Antenna", Aeson.object
                [ ("Id", Aeson.toJSON antenna)
                , ("Name", antenna & getProduct & Aeson.toJSON)
                ])
            , ("Topper", Aeson.object
                [ ("Id", Aeson.toJSON topper)
                , ("Name", topper & getProduct & Aeson.toJSON)
                ])
            , ("Unknown1", Aeson.toJSON x)
            , ("Unknown2", Aeson.toJSON y)
            ]
        VLoadoutOnline a -> Aeson.toJSON a
        VLocation x -> Aeson.toJSON x
        VMusicStinger a b c -> Aeson.toJSON (a, b, c)
        VPickup a b c -> Aeson.toJSON (a, b, c)
        VPrivateMatchSettings mutators joinableBy maxPlayers name password x -> Aeson.object
            [ ("Mutators", Aeson.toJSON mutators)
            , ("JoinableBy", Aeson.toJSON joinableBy)
            , ("MaxPlayers", Aeson.toJSON maxPlayers)
            , ("Name", Aeson.toJSON name)
            , ("Password", Aeson.toJSON password)
            , ("Unknown", Aeson.toJSON x)
            ]
        VQWord x -> Aeson.toJSON x
        VRelativeRotation x -> Aeson.toJSON x
        VReservation num systemId remoteId localId name x y -> Aeson.object
            [ ("Number", Aeson.toJSON num)
            , ("SystemId", Aeson.toJSON systemId)
            , ("RemoteId", Aeson.toJSON remoteId)
            , ("LocalId", Aeson.toJSON localId)
            , ("Name", Aeson.toJSON name)
            , ("Unknown1", Aeson.toJSON x)
            , ("Unknown2", Aeson.toJSON y)
            ]
        VRigidBodyState sleeping position rotation linear angular -> Aeson.object
            [ ("Sleeping", Aeson.toJSON sleeping)
            , ("Position", Aeson.toJSON position)
            , ("Rotation", Aeson.toJSON rotation)
            , ("LinearVelocity", Aeson.toJSON linear)
            , ("AngularVelocity", Aeson.toJSON angular)
            ]
        VString x -> Aeson.toJSON x
        VTeamPaint team color1 color2 finish1 finish2 -> Aeson.object
            [ ("Team", Aeson.toJSON team)
            , ("PrimaryColor", Aeson.toJSON color1)
            , ("AccentColor", Aeson.toJSON color2)
            , ("PrimaryFinish", Aeson.object
                [ ("Id", Aeson.toJSON finish1)
                , ("Name", finish1 & getProduct & Aeson.toJSON)
                ])
            , ("AccentFinish", Aeson.object
                [ ("Id", Aeson.toJSON finish2)
                , ("Name", finish2 & getProduct & Aeson.toJSON)
                ])
            ]
        VUniqueId systemId remoteId localId -> Aeson.object
            [ ("System", case systemId of
                0 -> "Local"
                1 -> "Steam"
                2 -> "PlayStation"
                4 -> "Xbox"
                _ -> Aeson.String ("Unknown system " <> StrictText.pack (show systemId)))
            , ("Remote", Aeson.toJSON remoteId)
            , ("Local", Aeson.toJSON localId)
            ]


getGameMode :: Word8.Word8 -> Maybe StrictText.Text
getGameMode x = Bimap.lookup (Word8.fromWord8 x) Data.gameModes


getProduct :: Word32.Word32 -> Maybe StrictText.Text
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products
