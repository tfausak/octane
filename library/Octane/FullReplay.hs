{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.FullReplay
    ( FullReplay
    , parseReplay
    , parseReplayFile
    , unsafeParseReplay
    , unsafeParseReplayFile
    ) where

import Data.Aeson ((.=))
import Data.Function ((&))
import Data.Monoid ((<>))
import Prelude ((==), (/=), (&&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Parser as Parser
import qualified Octane.Type as Type
import qualified Prelude


newtype FullReplay = FullReplay
    { unpackFullReplay :: (Type.Replay, [Parser.Frame])
    } deriving (Generics.Generic, Prelude.Show)

instance DeepSeq.NFData FullReplay

instance Aeson.ToJSON FullReplay where
    toJSON fullReplay = do
        Aeson.object
            [ "Version" .= getVersion fullReplay
            , "Metadata" .= getMetadata fullReplay
            , "Levels" .= getLevels fullReplay
            , "Messages" .= getMessages fullReplay
            , "TickMarks" .= getTickMarks fullReplay
            , "Frames" .= getFrames fullReplay
            ]


newFullReplay :: Type.Replay -> [Parser.Frame] -> FullReplay
newFullReplay replay frames = FullReplay (replay, frames)


getVersion :: FullReplay -> Prelude.String
getVersion fullReplay =
    [ fullReplay
        & unpackFullReplay
        & Prelude.fst
        & Type.replayVersion1
        & Type.unpackWord32LE
        & Prelude.fromIntegral
    , fullReplay
        & unpackFullReplay
        & Prelude.fst
        & Type.replayVersion2
        & Type.unpackWord32LE
        & Prelude.fromIntegral
    ] & Version.makeVersion & Version.showVersion


getMetadata :: FullReplay -> Map.Map Text.Text Aeson.Value
getMetadata fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayProperties
    & Type.unpackDictionary
    & Map.mapKeys Type.unpackPCString
    & Map.map Aeson.toJSON


getLevels :: FullReplay -> [Text.Text]
getLevels fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayLevels
    & Type.unpackList
    & Prelude.map Type.unpackPCString


getMessages :: FullReplay -> Map.Map Text.Text Type.PCString
getMessages fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayMessages
    & Type.unpackList
    & Prelude.map (\ message ->
        ( message
            & Type.messageFrame
            & Type.unpackWord32LE
            & Prelude.show
            & Text.pack
        , message & Type.messageContent
        ))
    & Map.fromList


getTickMarks :: FullReplay -> Map.Map Text.Text Type.PCString
getTickMarks fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayMarks
    & Type.unpackList
    & Prelude.map (\ mark ->
        ( mark
            & Type.markFrame
            & Type.unpackWord32LE
            & Prelude.show
            & Text.pack
        , mark & Type.markLabel
        ))
    & Map.fromList


getFrames :: FullReplay -> [Map.Map Text.Text Aeson.Value]
getFrames fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.snd
    & Foldable.foldl'
        (\ (state, frames) frame -> let
            newState = updateState frame state
            minimalFrame = getDelta state frame
            in (newState, minimalFrame : frames))
        (initialState, [])
    & Prelude.snd
    & Prelude.reverse
    & Prelude.map (\ frame -> Map.fromList
        [ ("IsKeyFrame", frame & Parser.frameIsKeyFrame & Aeson.toJSON)
        , ("Number", frame & Parser.frameNumber & Aeson.toJSON)
        , ("Time", frame & Parser.frameTime & Aeson.toJSON)
        , ("Delta", frame & Parser.frameDelta & Aeson.toJSON)
        , ("Spawned", getSpawnedActors frame)
        , ("Updated", getUpdatedActors frame)
        , ("Destroyed", getDestroyedActors frame)
        ])


-- { actor id => (alive?, { property name => property value } ) }
type State = IntMap.IntMap (Prelude.Bool, Map.Map Text.Text Parser.PropValue)


initialState :: State
initialState = IntMap.empty


updateState :: Parser.Frame -> State -> State
updateState frame state1 = let
    spawned = frame
        & Parser.frameReplications
        & Prelude.filter (\ replication -> replication
            & Parser.replicationState
            & (== Parser.RSOpening))
        & Prelude.map Parser.replicationActorId
    state2 = spawned
        & Prelude.foldr
            (IntMap.alter (\ maybeValue -> Prelude.Just (case maybeValue of
                Prelude.Nothing -> (Prelude.True, Map.empty)
                Prelude.Just (_, properties) -> (Prelude.True, properties))))
            state1

    destroyed = frame
        & Parser.frameReplications
        & Prelude.filter (\ replication -> replication
            & Parser.replicationState
            & (== Parser.RSClosing))
        & Prelude.map Parser.replicationActorId
    state3 = destroyed
        & Prelude.foldr
            (IntMap.alter (\ maybeValue -> Prelude.Just (case maybeValue of
                Prelude.Nothing -> (Prelude.False, Map.empty)
                Prelude.Just (_, properties) -> (Prelude.False, properties))))
            state2

    updated = frame
        & Parser.frameReplications
        & Prelude.filter (\ replication -> replication
            & Parser.replicationState
            & (== Parser.RSExisting))
    state4 = updated
        & Prelude.foldr
            (\ replication -> IntMap.alter
                (\ maybeValue -> Prelude.Just (case maybeValue of
                    Prelude.Nothing ->
                        (Prelude.True, Parser.replicationProperties replication)
                    Prelude.Just (alive, properties) ->
                        ( alive
                        , Map.union
                            (Parser.replicationProperties replication)
                            properties
                        )))
                (Parser.replicationActorId replication))
            state3

    in state4


getDelta :: State -> Parser.Frame -> Parser.Frame
getDelta state frame = let
    newReplications = frame
        & Parser.frameReplications
        -- Remove replications that aren't actually new.
        & reject (\ replication -> let
            isOpening = Parser.replicationState replication == Parser.RSOpening
            actorId = Parser.replicationActorId replication
            currentState = IntMap.lookup actorId state
            isAlive = Prelude.fmap Prelude.fst currentState
            wasAlreadyAlive = isAlive == Prelude.Just Prelude.True
            in isOpening && wasAlreadyAlive)
        -- Remove properties that haven't changed.
        & Prelude.map (\ replication ->
            if Parser.replicationState replication == Parser.RSExisting
            then let
                actorId = Parser.replicationActorId replication
                currentState = IntMap.findWithDefault
                    (Prelude.True, Map.empty) actorId state
                currentProperties = Prelude.snd currentState
                newProperties = Parser.replicationProperties replication
                changes = newProperties
                    & Map.filterWithKey (\ name newValue -> let
                        oldValue = Map.lookup name currentProperties
                        in Prelude.Just newValue /= oldValue)
                in replication { Parser.replicationProperties = changes }
            else replication)
    in frame { Parser.frameReplications = newReplications }


reject :: (a -> Prelude.Bool) -> [a] -> [a]
reject p xs = Prelude.filter (\ x -> Prelude.not (p x)) xs


getSpawnedActors :: Parser.Frame -> Aeson.Value
getSpawnedActors frame = frame
    & Parser.frameReplications
    & Prelude.filter (\ replication -> replication
        & Parser.replicationState
        & (== Parser.RSOpening))
    & Prelude.map (\ replication ->
        ( replication
            & Parser.replicationActorId
            & Prelude.show
            & Text.pack
        , Aeson.object
            [ ("Name", replication
                & Parser.replicationObjectName
                & Aeson.toJSON)
            , ("Class", replication
                & Parser.replicationClassName
                & Aeson.toJSON)
            , ("Position", replication
                & Parser.replicationInitialization
                & Prelude.fmap Parser.classInitLocation
                & Monad.join
                & Aeson.toJSON)
            , ("Rotation", replication
                & Parser.replicationInitialization
                & Prelude.fmap Parser.classInitRotation
                & Monad.join
                & Aeson.toJSON)
            ]
        ))
    & Aeson.object


getUpdatedActors :: Parser.Frame -> Aeson.Value
getUpdatedActors frame = frame
    & Parser.frameReplications
    & Prelude.filter (\ replication -> replication
        & Parser.replicationState
        & (== Parser.RSExisting))
    & Prelude.map (\ replication ->
        ( replication
            & Parser.replicationActorId
            & Prelude.show
            & Text.pack
        , replication
            & Parser.replicationProperties
            & Map.map (\ property -> Aeson.object
                [ ("Type", getPropertyName property)
                , ("Value", getPropertyValue property)
                ])
        ))
    & reject (\ (_, properties) -> Map.null properties)
    & Prelude.map (\ (actorId, properties) ->
        (actorId, Aeson.toJSON properties))
    & Aeson.object


getPropertyName :: Parser.PropValue -> Aeson.Value
getPropertyName property = case property of
    Parser.PBoolean _ -> "Boolean"
    Parser.PByte _ -> "Byte"
    Parser.PCamSettings _ _ _ _ _ _ -> "CameraSettings"
    Parser.PDemolish _ _ _ _ _ _ -> "Demolition"
    Parser.PEnum _ _ -> "Enum"
    Parser.PExplosion _ _ _ -> "Explosion"
    Parser.PFlaggedInt _ _ -> "FlaggedInt"
    Parser.PFloat _ -> "Float"
    Parser.PGameMode _ -> "GameMode"
    Parser.PInt _ -> "Int"
    Parser.PLoadout _ _ _ _ _ _ _ _ _ -> "Loadout"
    Parser.PLoadoutOnline _ _ _ _ -> "OnlineLoadout"
    Parser.PLocation _ -> "Position"
    Parser.PMusicStinger _ _ _ -> "MusicStinger"
    Parser.PPickup _ _ _ -> "Pickup"
    Parser.PPrivateMatchSettings _ _ _ _ _ _ -> "PrivateMatchSettings"
    Parser.PQWord _ _ -> "QWord"
    Parser.PRelativeRotation _ -> "RelativeRotation"
    Parser.PReservation _ _ _ _ _ _ _ -> "Reservation"
    Parser.PRigidBodyState _ _ _ _ _ -> "RigidBodyState"
    Parser.PString _ -> "String"
    Parser.PTeamPaint _ _ _ _ _ -> "Paint"
    Parser.PUniqueId _ _ _ -> "UniqueId"


getPropertyValue :: Parser.PropValue -> Aeson.Value
getPropertyValue property = case property of
    Parser.PBoolean x -> Aeson.toJSON x
    Parser.PByte x -> Aeson.toJSON x
    Parser.PCamSettings fov height angle distance stiffness swivelSpeed -> Aeson.object
        [ ("FOV", Aeson.toJSON fov)
        , ("Height", Aeson.toJSON height)
        , ("Angle", Aeson.toJSON angle)
        , ("Distance", Aeson.toJSON distance)
        , ("Stiffness", Aeson.toJSON stiffness)
        , ("SwivelSpeed", Aeson.toJSON swivelSpeed)
        ]
    Parser.PDemolish a b c d e f -> Aeson.toJSON (a, b, c, d, e, f)
    Parser.PEnum x y -> Aeson.toJSON (x, y)
    Parser.PExplosion a b c -> Aeson.toJSON (a, b, c)
    Parser.PFlaggedInt x y -> Aeson.toJSON (x, y)
    Parser.PFloat x -> Aeson.toJSON x
    Parser.PGameMode x -> case x of
        1 -> "Hockey"
        2 -> "Hoops"
        _ -> Aeson.String ("Unknown game mode " <> Text.pack (Prelude.show x))
    Parser.PInt x -> Aeson.toJSON x
    Parser.PLoadout version body decal wheels rocketTrail antenna topper x y -> Aeson.object
        [ ("Version", Aeson.toJSON version)
        , ("Body", Aeson.toJSON body)
        , ("Decal", Aeson.toJSON decal)
        , ("Wheels", Aeson.toJSON wheels)
        , ("RocketTrail", Aeson.toJSON rocketTrail)
        , ("Antenna", Aeson.toJSON antenna)
        , ("Topper", Aeson.toJSON topper)
        , ("Unknown1", Aeson.toJSON x)
        , ("Unknown2", Aeson.toJSON y)
        ]
    Parser.PLoadoutOnline a b c d -> Aeson.toJSON (a, b, c, d)
    Parser.PLocation x -> Aeson.toJSON x
    Parser.PMusicStinger a b c -> Aeson.toJSON (a, b, c)
    Parser.PPickup a b c -> Aeson.toJSON (a, b, c)
    Parser.PPrivateMatchSettings mutators joinableBy maxPlayers name password x -> Aeson.object
        [ ("Mutators", Aeson.toJSON mutators)
        , ("JoinableBy", Aeson.toJSON joinableBy)
        , ("MaxPlayers", Aeson.toJSON maxPlayers)
        , ("Name", Aeson.toJSON name)
        , ("Password", Aeson.toJSON password)
        , ("Unknown", Aeson.toJSON x)
        ]
    Parser.PQWord a b -> Aeson.toJSON (a, b)
    Parser.PRelativeRotation x -> Aeson.toJSON x
    Parser.PReservation number systemId remoteId localId name x y -> Aeson.object
        [ ("Number", Aeson.toJSON number)
        , ("SystemId", Aeson.toJSON systemId)
        , ("RemoteId", Aeson.toJSON remoteId)
        , ("LocalId", Aeson.toJSON localId)
        , ("Name", Aeson.toJSON name)
        , ("Unknown1", Aeson.toJSON x)
        , ("Unknown2", Aeson.toJSON y)
        ]
    Parser.PRigidBodyState sleeping position rotation linear angular -> Aeson.object
        [ ("Sleeping", Aeson.toJSON sleeping)
        , ("Position", Aeson.toJSON position)
        , ("Rotation", Aeson.toJSON rotation)
        , ("LinearVelocity", Aeson.toJSON linear)
        , ("AngularVelocity", Aeson.toJSON angular)
        ]
    Parser.PString x -> Aeson.toJSON x
    Parser.PTeamPaint team color1 color2 finish1 finish2 -> Aeson.object
        [ ("Team", Aeson.toJSON team)
        , ("PrimaryColor", Aeson.toJSON color1)
        , ("AccentColor", Aeson.toJSON color2)
        , ("PrimaryFinish", Aeson.toJSON finish1)
        , ("AccentFinish", Aeson.toJSON finish2)
        ]
    Parser.PUniqueId systemId remoteId localId -> Aeson.object
        [ ("System", case systemId of
            0 -> "Local"
            1 -> "Steam"
            2 -> "PlayStation"
            4 -> "Xbox"
            _ -> Aeson.String ("Unknown system " <> Text.pack (Prelude.show systemId)))
        , ("Remote", case remoteId of
            Parser.SplitscreenId x -> Aeson.toJSON x
            Parser.SteamId x -> Aeson.toJSON x
            Parser.PlayStationId x -> Aeson.toJSON x
            Parser.XboxId x -> Aeson.toJSON x)
        , ("Local", Aeson.toJSON localId)
        ]

getDestroyedActors :: Parser.Frame -> Aeson.Value
getDestroyedActors frame = frame
    & Parser.frameReplications
    & Prelude.filter (\ replication -> replication
        & Parser.replicationState
        & (== Parser.RSClosing))
    & Prelude.map Parser.replicationActorId
    & Aeson.toJSON


parseReplay :: ByteString.ByteString -> Prelude.Either Text.Text FullReplay
parseReplay bytes = do
    case Binary.decodeOrFail bytes of
        Prelude.Left (_, _, message) -> do
            Prelude.Left (Text.pack message)
        Prelude.Right (_, _, replay) -> do
            let frames = Parser.parseFrames replay
            Prelude.Right (newFullReplay replay frames)


parseReplayFile :: Prelude.FilePath -> Prelude.IO (Prelude.Either Text.Text FullReplay)
parseReplayFile file = do
    result <- Binary.decodeFileOrFail file
    case result of
        Prelude.Left (_, message) -> do
            Prelude.pure (Prelude.Left (Text.pack message))
        Prelude.Right replay -> do
            let frames = Parser.parseFrames replay
            Prelude.pure (Prelude.Right (newFullReplay replay frames))


unsafeParseReplay :: ByteString.ByteString -> FullReplay
unsafeParseReplay bytes = do
    let replay = Binary.decode bytes
    let frames = Parser.parseFrames replay
    newFullReplay replay frames


unsafeParseReplayFile :: Prelude.FilePath -> Prelude.IO FullReplay
unsafeParseReplayFile file = do
    replay <- Binary.decodeFile file
    let frames = Parser.parseFrames replay
    Prelude.pure (newFullReplay replay frames)
