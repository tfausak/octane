module Octane.Type.Frame (Frame(..)) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Text as StrictText
import qualified Octane.Data as Data
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Value as Value
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word8 as Word8


-- | A frame in the network stream. This holds all the interesting game data.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it
-- requires out-of-band information (the class property map) to decode.
data Frame = Frame
    { frameNumber :: Word
    -- ^ This frame's number in the network stream. Starts at 0.
    , frameIsKeyFrame :: Bool
    -- ^ Is this frame a key frame?
    , frameTime :: Float32.Float32
    -- ^ The since the start of the match that this frame occurred.
    , frameDelta :: Float32.Float32
    -- ^ The time between the last frame and this one.
    , frameReplications :: [Replication.Replication]
    -- ^ A list of all the replications in this frame.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''Frame)

instance NFData Frame where

instance ToJSON Frame where
    toJSON frame = Aeson.object
        [ "Number" .= #number frame
        , "IsKeyFrame" .= #isKeyFrame frame
        , "Time" .= #time frame
        , "Delta" .= #delta frame
        , "Spawned" .= (frame & #replications & getSpawned)
        , "Updated" .= (frame & #replications & getUpdated)
        , "Destroyed" .= (frame & #replications & getDestroyed)
        ]


newtype Spawned = Spawned [Replication.Replication]

instance ToJSON Spawned where
    toJSON (Spawned xs) = xs
        & map (\ x -> do
            let k = x & #actorId & #value & show & StrictText.pack
            let v = Aeson.object
                    [ "Name" .= #objectName x
                    , "Class" .= #className x
                    , "Position" .= (x & #initialization & fmap #location)
                    , "Rotation" .= (x & #initialization & fmap #rotation)
                    ]
            (k, v))
        & Map.fromList
        & toJSON

getSpawned :: [Replication.Replication] -> Spawned
getSpawned xs = xs
    & filter (\ x -> x
        & #state
        & (== State.SOpening))
    & Spawned


newtype Updated = Updated [Replication.Replication]

instance ToJSON Updated where
    toJSON (Updated xs) = xs
        & map (\ x -> do
            let k = x
                    & #actorId
                    & #value
                    & show
                    & StrictText.pack
            let v = x
                    & #properties
                    & Map.map (\ value -> Aeson.object
                        [ "Type" .= getType value
                        , "Value" .= getValue value
                        ])
            (k, v))
        & Map.fromList
        & toJSON


getUpdated :: [Replication.Replication] -> Updated
getUpdated xs = xs
    & filter (\ x -> x
        & #state
        & (== State.SExisting))
    & filter (\ x -> x
        & #properties
        & null
        & not)
    & Updated


newtype Destroyed = Destroyed [Replication.Replication]

instance ToJSON Destroyed where
    toJSON (Destroyed xs) = xs
        & map #actorId
        & map #value
        & toJSON

getDestroyed :: [Replication.Replication] -> Destroyed
getDestroyed xs = xs
    & filter (\ x -> x
        & #state
        & (== State.SClosing))
    & Destroyed


getType :: Value.Value -> StrictText
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
    Value.VLoadoutOnline _ -> "OnlineLoadout"
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
    Value.VBoolean x -> toJSON x
    Value.VByte x -> toJSON x
    Value.VCamSettings fov height angle distance stiffness swivelSpeed -> Aeson.object
        [ ("FOV", toJSON fov)
        , ("Height", toJSON height)
        , ("Angle", toJSON angle)
        , ("Distance", toJSON distance)
        , ("Stiffness", toJSON stiffness)
        , ("SwivelSpeed", toJSON swivelSpeed)
        ]
    Value.VDemolish a b c d e f -> toJSON (a, b, c, d, e, f)
    Value.VEnum x y -> toJSON (x, y)
    Value.VExplosion a b c -> toJSON (a, b, c)
    Value.VFlaggedInt x y -> toJSON (x, y)
    Value.VFloat x -> toJSON x
    Value.VGameMode gameMode -> Aeson.object
        [ ("Id", toJSON gameMode)
        , ("Name", gameMode & getGameMode & toJSON)
        ]
    Value.VInt x -> toJSON x
    Value.VLoadout version body decal wheels rocketTrail antenna topper x y -> Aeson.object
        [ ("Version", toJSON version)
        , ("Body", Aeson.object
            [ ("Id", toJSON body)
            , ("Name", body & getProduct & toJSON)
            ])
        , ("Decal", Aeson.object
            [ ("Id", toJSON decal)
            , ("Name", decal & getProduct & toJSON)
            ])
        , ("Wheels", Aeson.object
            [ ("Id", toJSON wheels)
            , ("Name", wheels & getProduct & toJSON)
            ])
        , ("RocketTrail", Aeson.object
            [ ("Id", toJSON rocketTrail)
            , ("Name", rocketTrail & getProduct & toJSON)
            ])
        , ("Antenna", Aeson.object
            [ ("Id", toJSON antenna)
            , ("Name", antenna & getProduct & toJSON)
            ])
        , ("Topper", Aeson.object
            [ ("Id", toJSON topper)
            , ("Name", topper & getProduct & toJSON)
            ])
        , ("Unknown1", toJSON x)
        , ("Unknown2", toJSON y)
        ]
    Value.VLoadoutOnline a -> toJSON a
    Value.VLocation x -> toJSON x
    Value.VMusicStinger a b c -> toJSON (a, b, c)
    Value.VPickup a b c -> toJSON (a, b, c)
    Value.VPrivateMatchSettings mutators joinableBy maxPlayers name password x -> Aeson.object
        [ ("Mutators", toJSON mutators)
        , ("JoinableBy", toJSON joinableBy)
        , ("MaxPlayers", toJSON maxPlayers)
        , ("Name", toJSON name)
        , ("Password", toJSON password)
        , ("Unknown", toJSON x)
        ]
    Value.VQWord x -> toJSON x
    Value.VRelativeRotation x -> toJSON x
    Value.VReservation num systemId remoteId localId name x y -> Aeson.object
        [ ("Number", toJSON num)
        , ("SystemId", toJSON systemId)
        , ("RemoteId", toJSON remoteId)
        , ("LocalId", toJSON localId)
        , ("Name", toJSON name)
        , ("Unknown1", toJSON x)
        , ("Unknown2", toJSON y)
        ]
    Value.VRigidBodyState sleeping position rotation linear angular -> Aeson.object
        [ ("Sleeping", toJSON sleeping)
        , ("Position", toJSON position)
        , ("Rotation", toJSON rotation)
        , ("LinearVelocity", toJSON linear)
        , ("AngularVelocity", toJSON angular)
        ]
    Value.VString x -> toJSON x
    Value.VTeamPaint team color1 color2 finish1 finish2 -> Aeson.object
        [ ("Team", toJSON team)
        , ("PrimaryColor", toJSON color1)
        , ("AccentColor", toJSON color2)
        , ("PrimaryFinish", Aeson.object
            [ ("Id", toJSON finish1)
            , ("Name", finish1 & getProduct & toJSON)
            ])
        , ("AccentFinish", Aeson.object
            [ ("Id", toJSON finish2)
            , ("Name", finish2 & getProduct & toJSON)
            ])
        ]
    Value.VUniqueId systemId remoteId localId -> Aeson.object
        [ ("System", case systemId of
            0 -> "Local"
            1 -> "Steam"
            2 -> "PlayStation"
            4 -> "Xbox"
            _ -> Aeson.String ("Unknown system " <> StrictText.pack (show systemId)))
        , ("Remote", toJSON remoteId)
        , ("Local", toJSON localId)
        ]


getGameMode :: Word8.Word8 -> Maybe StrictText
getGameMode x = Bimap.lookup (Word8.fromWord8 x) Data.gameModes


getProduct :: Word32.Word32 -> Maybe StrictText
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products
