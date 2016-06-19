{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Frame (Frame(..)) where

import Data.Aeson ((.=))
import Data.Function ((&))
import Data.Monoid ((<>))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Data as Data
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Value as Value
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word8 as Word8


-- | A frame in the network stream. This holds all the interesting game data.
data Frame = Frame
    { number :: Word
    -- ^ This frame's number in the network stream. Starts at 0.
    , isKeyFrame :: Bool
    -- ^ Is this frame a key frame?
    , time :: Float32.Float32
    -- ^ The since the start of the match that this frame occurred.
    , delta :: Float32.Float32
    -- ^ The time between the last frame and this one.
    , replications :: [Replication.Replication]
    -- ^ A list of all the replications in this frame.
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Frame where

instance Aeson.ToJSON Frame where
    toJSON frame = Aeson.object
        [ "Number" .= number frame
        , "IsKeyFrame" .= isKeyFrame frame
        , "Time" .= time frame
        , "Delta" .= delta frame
        , "Spawned" .= (frame & replications & getSpawned)
        , "Updated" .= (frame & replications & getUpdated)
        , "Destroyed" .= (frame & replications & getDestroyed)
        ]


newtype Spawned = Spawned [Replication.Replication]

instance Aeson.ToJSON Spawned where
    toJSON (Spawned xs) = xs
        & map (\ x -> do
            let k = x & Replication.actorId & show & StrictText.pack
            let v = Aeson.object
                    [ "Name" .= Replication.objectName x
                    , "Class" .= Replication.className x
                    , "Position" .= (x & Replication.initialization & fmap Initialization.location)
                    , "Rotation" .= (x & Replication.initialization & fmap Initialization.rotation)
                    ]
            (k, v))
        & Map.fromList
        & Aeson.toJSON

getSpawned :: [Replication.Replication] -> Spawned
getSpawned xs = xs
    & filter (\ x -> x
        & Replication.state
        & (== State.SOpening))
    & Spawned


newtype Updated = Updated [Replication.Replication]

instance Aeson.ToJSON Updated where
    toJSON (Updated xs) = xs
        & map (\ x -> do
            let k = x
                    & Replication.actorId
                    & show
                    & StrictText.pack
            let v = x
                    & Replication.properties
                    & Map.map (\ value -> Aeson.object
                        [ "Type" .= getType value
                        , "Value" .= getValue value
                        ])
            (k, v))
        & Map.fromList
        & Aeson.toJSON


getUpdated :: [Replication.Replication] -> Updated
getUpdated xs = xs
    & filter (\ x -> x
        & Replication.state
        & (== State.SExisting))
    & filter (\ x -> x
        & Replication.properties
        & null
        & not)
    & Updated


newtype Destroyed = Destroyed [Replication.Replication]

instance Aeson.ToJSON Destroyed where
    toJSON (Destroyed xs) = xs
        & map Replication.actorId
        & Aeson.toJSON

getDestroyed :: [Replication.Replication] -> Destroyed
getDestroyed xs = xs
    & filter (\ x -> x
        & Replication.state
        & (== State.SClosing))
    & Destroyed


getType :: Value.Value -> StrictText.Text
getType value = case value of
    Value.VBoolean _ -> "Boolean"
    Value.VByte _ -> "Byte"
    Value.VCamSettings _ _ _ _ _ _ -> "CameraSettings"
    Value.VDemolish _ _ _ _ _ _ -> "Demolition"
    Value.VEnum _ _ -> "Enum"
    Value.VExplosion _ _ _ -> "Explosion"
    Value.VFlaggedInt _ _ -> "FlaggedInt"
    Value.VFloat _ -> "Float"
    Value.VGameMode _ -> "GameMode"
    Value.VInt _ -> "Int"
    Value.VLoadout _ _ _ _ _ _ _ _ _ -> "Loadout"
    Value.VLoadoutOnline _ _ _ _ -> "OnlineLoadout"
    Value.VLocation _ -> "Position"
    Value.VMusicStinger _ _ _ -> "MusicStinger"
    Value.VPickup _ _ _ -> "Pickup"
    Value.VPrivateMatchSettings _ _ _ _ _ _ -> "PrivateMatchSettings"
    Value.VQWord _ -> "QWord"
    Value.VRelativeRotation _ -> "RelativeRotation"
    Value.VReservation _ _ _ _ _ _ _ -> "Reservation"
    Value.VRigidBodyState _ _ _ _ _ -> "RigidBodyState"
    Value.VString _ -> "String"
    Value.VTeamPaint _ _ _ _ _ -> "Paint"
    Value.VUniqueId _ _ _ -> "UniqueId"


getValue :: Value.Value -> Aeson.Value
getValue value = case value of
    Value.VBoolean x -> Aeson.toJSON x
    Value.VByte x -> Aeson.toJSON x
    Value.VCamSettings fov height angle distance stiffness swivelSpeed -> Aeson.object
        [ ("FOV", Aeson.toJSON fov)
        , ("Height", Aeson.toJSON height)
        , ("Angle", Aeson.toJSON angle)
        , ("Distance", Aeson.toJSON distance)
        , ("Stiffness", Aeson.toJSON stiffness)
        , ("SwivelSpeed", Aeson.toJSON swivelSpeed)
        ]
    Value.VDemolish a b c d e f -> Aeson.toJSON (a, b, c, d, e, f)
    Value.VEnum x y -> Aeson.toJSON (x, y)
    Value.VExplosion a b c -> Aeson.toJSON (a, b, c)
    Value.VFlaggedInt x y -> Aeson.toJSON (x, y)
    Value.VFloat x -> Aeson.toJSON x
    Value.VGameMode gameMode -> Aeson.object
        [ ("Id", Aeson.toJSON gameMode)
        , ("Name", gameMode & getGameMode & Aeson.toJSON)
        ]
    Value.VInt x -> Aeson.toJSON x
    Value.VLoadout version body decal wheels rocketTrail antenna topper x y -> Aeson.object
        [ ("Version", Aeson.toJSON version)
        , ("Body", Aeson.object
            [ ("Id", Aeson.toJSON body)
            , ("Name", body & getBody & Aeson.toJSON)
            ])
        , ("Decal", Aeson.object
            [ ("Id", Aeson.toJSON decal)
            , ("Name", decal & getDecal & Aeson.toJSON)
            ])
        , ("Wheels", Aeson.object
            [ ("Id", Aeson.toJSON wheels)
            , ("Name", wheels & getWheels & Aeson.toJSON)
            ])
        , ("RocketTrail", Aeson.object
            [ ("Id", Aeson.toJSON rocketTrail)
            , ("Name", rocketTrail & getRocketTrail & Aeson.toJSON)
            ])
        , ("Antenna", Aeson.object
            [ ("Id", Aeson.toJSON antenna)
            , ("Name", antenna & getAntenna & Aeson.toJSON)
            ])
        , ("Topper", Aeson.object
            [ ("Id", Aeson.toJSON topper)
            , ("Name", topper & getTopper & Aeson.toJSON)
            ])
        , ("Unknown1", Aeson.toJSON x)
        , ("Unknown2", Aeson.toJSON y)
        ]
    Value.VLoadoutOnline a b c d -> Aeson.toJSON (a, b, c, d)
    Value.VLocation x -> Aeson.toJSON x
    Value.VMusicStinger a b c -> Aeson.toJSON (a, b, c)
    Value.VPickup a b c -> Aeson.toJSON (a, b, c)
    Value.VPrivateMatchSettings mutators joinableBy maxPlayers name password x -> Aeson.object
        [ ("Mutators", Aeson.toJSON mutators)
        , ("JoinableBy", Aeson.toJSON joinableBy)
        , ("MaxPlayers", Aeson.toJSON maxPlayers)
        , ("Name", Aeson.toJSON name)
        , ("Password", Aeson.toJSON password)
        , ("Unknown", Aeson.toJSON x)
        ]
    Value.VQWord x -> Aeson.toJSON x
    Value.VRelativeRotation x -> Aeson.toJSON x
    Value.VReservation num systemId remoteId localId name x y -> Aeson.object
        [ ("Number", Aeson.toJSON num)
        , ("SystemId", Aeson.toJSON systemId)
        , ("RemoteId", Aeson.toJSON remoteId)
        , ("LocalId", Aeson.toJSON localId)
        , ("Name", Aeson.toJSON name)
        , ("Unknown1", Aeson.toJSON x)
        , ("Unknown2", Aeson.toJSON y)
        ]
    Value.VRigidBodyState sleeping position rotation linear angular -> Aeson.object
        [ ("Sleeping", Aeson.toJSON sleeping)
        , ("Position", Aeson.toJSON position)
        , ("Rotation", Aeson.toJSON rotation)
        , ("LinearVelocity", Aeson.toJSON linear)
        , ("AngularVelocity", Aeson.toJSON angular)
        ]
    Value.VString x -> Aeson.toJSON x
    Value.VTeamPaint team color1 color2 finish1 finish2 -> Aeson.object
        [ ("Team", Aeson.toJSON team)
        , ("PrimaryColor", Aeson.toJSON color1)
        , ("AccentColor", Aeson.toJSON color2)
        , ("PrimaryFinish", Aeson.object
            [ ("Id", Aeson.toJSON finish1)
            , ("Name", finish1 & getFinish & Aeson.toJSON)
            ])
        , ("AccentFinish", Aeson.object
            [ ("Id", Aeson.toJSON finish2)
            , ("Name", finish2 & getFinish & Aeson.toJSON)
            ])
        ]
    Value.VUniqueId systemId remoteId localId -> Aeson.object
        [ ("System", case systemId of
            0 -> "Local"
            1 -> "Steam"
            2 -> "PlayStation"
            4 -> "Xbox"
            _ -> Aeson.String ("Unknown system " <> StrictText.pack (show systemId)))
        , ("Remote", case remoteId of
            RemoteId.SplitscreenId x -> Aeson.toJSON x
            RemoteId.SteamId x -> Aeson.toJSON x
            RemoteId.PlayStationId x -> Aeson.toJSON x
            RemoteId.XboxId x -> Aeson.toJSON x)
        , ("Local", Aeson.toJSON localId)
        ]


getGameMode :: Word8.Word8 -> Maybe StrictText.Text
getGameMode x = Bimap.lookup (Word8.fromWord8 x) Data.gameModes


getBody :: Word32.Word32 -> Maybe StrictText.Text
getBody x = Bimap.lookup (Word32.fromWord32 x) Data.bodies


getDecal :: Word32.Word32 -> Maybe StrictText.Text
getDecal x = Bimap.lookup (Word32.fromWord32 x) Data.decals


getWheels :: Word32.Word32 -> Maybe StrictText.Text
getWheels x = Bimap.lookup (Word32.fromWord32 x) Data.wheels


getRocketTrail :: Word32.Word32 -> Maybe StrictText.Text
getRocketTrail x = Bimap.lookup (Word32.fromWord32 x) Data.rocketTrails


getAntenna :: Word32.Word32 -> Maybe StrictText.Text
getAntenna x = Bimap.lookup (Word32.fromWord32 x) Data.antennas


getTopper :: Word32.Word32 -> Maybe StrictText.Text
getTopper x = Bimap.lookup (Word32.fromWord32 x) Data.toppers


getFinish :: Word32.Word32 -> Maybe StrictText.Text
getFinish x = Bimap.lookup (Word32.fromWord32 x) Data.finishes
