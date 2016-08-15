{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

module Octane.Utility.Parser (parseStream) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Data as Data
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Property as Property
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.ReplayWithoutFrames as ReplayWithoutFrames
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Value as Value
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word16 as Word16
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Type.Word8 as Word8
import qualified Octane.Utility.ClassPropertyMap as CPM
import qualified Text.Printf as Printf


-- | Parses the network stream and returns a list of frames.
parseStream :: ReplayWithoutFrames.ReplayWithoutFrames -> [Frame.Frame]
parseStream replay = let
    numFrames = replay
        & #properties
        & #unpack
        & Map.lookup ("NumFrames" & StrictText.pack & Text.Text)
        & (\ property -> case property of
            Just (Property.IntProperty _ x) -> x & #unpack & fromIntegral
            _ -> 0)
    get = replay & extractContext & getFrames 0 numFrames & Bits.runBitGet
    stream = replay & #stream & Stream.unpack
    (_context, frames) = Binary.runGet get stream
    in frames


getFrames :: Word -> Int -> Context -> Bits.BitGet (Context, [Frame.Frame])
getFrames number numFrames context = do
    if fromIntegral number >= numFrames
    then pure (context, [])
    else do
        isEmpty <- Bits.isEmpty
        if isEmpty
        then pure (context, [])
        else do
            maybeFrame <- getMaybeFrame context number
            case maybeFrame of
                Nothing -> pure (context, [])
                Just (newContext, frame) -> do
                    (newerContext, frames) <- getFrames (number + 1) numFrames newContext
                    pure (newerContext, (frame : frames))


getMaybeFrame :: Context -> Word -> Bits.BitGet (Maybe (Context, Frame.Frame))
getMaybeFrame context number = do
    time <- getFloat32
    delta <- getFloat32
    if time == 0 && delta == 0
    then pure Nothing
    else if time < 0.001 || delta < 0.001
    then fail ("parsing previous frame probably failed. time: " ++ show time ++ ", delta: " ++ show delta)
    else do
        (newContext, frame) <- getFrame context number time delta
        pure (Just (newContext, frame))


getFrame :: Context -> Word -> Float32.Float32 -> Float32.Float32 -> Bits.BitGet (Context, Frame.Frame)
getFrame context number time delta = do
    (newContext, replications) <- getReplications context
    let frame =
            Frame.Frame
            { Frame.frameNumber = number
            , Frame.frameIsKeyFrame = context & contextKeyFrames & Set.member number
            , Frame.frameTime = time
            , Frame.frameDelta = delta
            , Frame.frameReplications = replications
            }
    (newContext, frame) & DeepSeq.force & pure


getReplications :: Context -> Bits.BitGet (Context, [Replication.Replication])
getReplications context = do
    maybeReplication <- getMaybeReplication context
    case maybeReplication of
        Nothing -> pure (context, [])
        Just (newContext, replication) -> do
            (newerContext, replications) <- getReplications newContext
            pure (newerContext, replication : replications)


getMaybeReplication :: Context -> Bits.BitGet (Maybe (Context, Replication.Replication))
getMaybeReplication context = do
    hasReplication <- getBool
    if #unpack hasReplication
        then do
            (newContext,replication) <- getReplication context
            pure (Just (newContext, replication))
        else pure Nothing


getReplication :: Context -> Bits.BitGet (Context, Replication.Replication)
getReplication context = do
    actorId <- BinaryBit.getBits maxActorId
    isOpen <- getBool
    let go =
            if #unpack isOpen
                then getOpenReplication
                else getClosedReplication
    go context actorId


getOpenReplication :: Context
                   -> CompressedWord.CompressedWord
                   -> Bits.BitGet (Context, Replication.Replication)
getOpenReplication context actorId = do
    isNew <- getBool
    let go =
            if #unpack isNew
                then getNewReplication
                else getExistingReplication
    go context actorId


getNewReplication :: Context
                  -> CompressedWord.CompressedWord
                  -> Bits.BitGet (Context, Replication.Replication)
getNewReplication context actorId = do
    unknownFlag <- getBool
    if #unpack unknownFlag
        then fail "the unknown flag in a new replication is true! what does it mean?"
        else pure ()
    objectId <- getInt32
    objectName <- case context & contextObjectMap & IntMap.lookup (Int32.fromInt32 objectId) of
        Nothing -> fail ("could not find object name for id " ++ show objectId)
        Just x -> pure x
    (classId, className) <- CPM.getClass
        (contextObjectMap context)
        Data.classes
        (contextClassMap context)
        (Int32.fromInt32 objectId)
    classInit <- Initialization.getInitialization className
    let thing = Thing
            { thingFlag = unknownFlag
            , thingObjectId = objectId
            , thingObjectName = objectName
            , thingClassId = classId
            , thingClassName = className
            , thingInitialization = classInit
            }
    let things = contextThings context
    let newThings = IntMap.insert (CompressedWord.fromCompressedWord actorId) thing things
    let newContext = context { contextThings = newThings }
    pure
        ( newContext
        , Replication.Replication
          { Replication.actorId = actorId
          , Replication.objectName = objectName
          , Replication.className = className
          , Replication.state = State.SOpening
          , Replication.initialization = Just classInit
          , Replication.properties = Map.empty
          })


getExistingReplication :: Context
                       -> CompressedWord.CompressedWord
                       -> Bits.BitGet (Context, Replication.Replication)
getExistingReplication context actorId = do
    thing <- case context & contextThings & IntMap.lookup (CompressedWord.fromCompressedWord actorId) of
        Nothing -> fail ("could not find thing for existing actor " ++ show actorId)
        Just x -> pure x
    props <- getProps context thing
    pure (context, Replication.Replication
        { Replication.actorId = actorId
        , Replication.objectName = thingObjectName thing
        , Replication.className = thingClassName thing
        , Replication.state = State.SExisting
        , Replication.initialization = Nothing
        , Replication.properties = props
        })


getClosedReplication :: Context
                     -> CompressedWord.CompressedWord
                     -> Bits.BitGet (Context, Replication.Replication)
getClosedReplication context actorId = do
    thing <- case context & contextThings & IntMap.lookup (CompressedWord.fromCompressedWord actorId) of
        Nothing -> fail ("could not find thing for closed actor " ++ show actorId)
        Just x -> pure x
    let newThings = context & contextThings & IntMap.delete (CompressedWord.fromCompressedWord actorId)
    let newContext = context { contextThings = newThings }
    pure
        ( newContext
        , Replication.Replication
          { Replication.actorId = actorId
          , Replication.objectName = thingObjectName thing
          , Replication.className = thingClassName thing
          , Replication.state = State.SClosing
          , Replication.initialization = Nothing
          , Replication.properties = Map.empty
          })


getProps :: Context -> Thing -> Bits.BitGet (Map.Map StrictText.Text Value.Value)
getProps context thing = do
    maybeProp <- getMaybeProp context thing
    case maybeProp of
        Nothing -> pure Map.empty
        Just (k, v) -> do
            let m = Map.singleton k v
            props <- getProps context thing
            pure (Map.union m props)


getMaybeProp :: Context -> Thing -> Bits.BitGet (Maybe (StrictText.Text, Value.Value))
getMaybeProp context thing = do
    hasProp <- getBool
    if #unpack hasProp
    then do
        prop <- getProp context thing
        pure (Just prop)
    else pure Nothing


getProp :: Context -> Thing -> Bits.BitGet (StrictText.Text, Value.Value)
getProp context thing = do
    let classId = thing & thingClassId
    props <- case context & contextClassPropertyMap & IntMap.lookup classId of
        Nothing -> fail ("could not find property map for class id " ++ show classId)
        Just x -> pure x
    let maxId = props & IntMap.keys & (0 :) & maximum
    pid <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits maxId)
    name <- case props & IntMap.lookup pid of
        Nothing -> fail ("could not find property name for property id " ++ show pid)
        Just x -> pure x
    value <- getPropValue context name
    pure (name, value)


getPropValue :: Context -> StrictText.Text -> Bits.BitGet Value.Value
getPropValue context name = case Map.lookup name Data.properties of
    Just property -> case StrictText.unpack property of
        "boolean" -> getBooleanProperty
        "byte" -> getByteProperty
        "cam_settings" -> getCamSettingsProperty
        "demolish" -> getDemolishProperty
        "enum" -> getEnumProperty
        "explosion" -> getExplosionProperty
        "flagged_int" -> getFlaggedIntProperty
        "float" -> getFloatProperty
        "game_mode" -> getGameModeProperty context
        "int" -> getIntProperty
        "loadout_online" -> getLoadoutOnlineProperty
        "loadout" -> getLoadoutProperty
        "location" -> getLocationProperty
        "music_stinger" -> getMusicStingerProperty
        "party_leader" -> getPartyLeaderProperty
        "pickup" -> getPickupProperty
        "private_match_settings" -> getPrivateMatchSettingsProperty
        "qword" -> getQWordProperty
        "relative_rotation" -> getRelativeRotationProperty
        "reservation" -> getReservationProperty context
        "rigid_body_state" -> getRigidBodyStateProperty
        "string" -> getStringProperty
        "team_paint" -> getTeamPaintProperty
        "unique_id" -> getUniqueIdProperty
        _ -> fail ("Don't know how to read property type " ++ show property ++ " for " ++ show name)
    Nothing -> do
        fail ("Don't know how to read property " ++ show name)


getBooleanProperty :: Bits.BitGet Value.Value
getBooleanProperty = do
    bool <- getBool
    pure (Value.VBoolean bool)


getByteProperty :: Bits.BitGet Value.Value
getByteProperty = do
    word <- getWord8
    pure (Value.VByte word)


getCamSettingsProperty :: Bits.BitGet Value.Value
getCamSettingsProperty = do
    fov <- getFloat32
    height <- getFloat32
    angle <- getFloat32
    distance <- getFloat32
    stiffness <- getFloat32
    swivelSpeed <- getFloat32
    pure (Value.VCamSettings fov height angle distance stiffness swivelSpeed)


getDemolishProperty :: Bits.BitGet Value.Value
getDemolishProperty = do
    atkFlag <- getBool
    atk <- getWord32
    vicFlag <- getBool
    vic <- getWord32
    vec1 <- Vector.getIntVector
    vec2 <- Vector.getIntVector
    pure (Value.VDemolish atkFlag atk vicFlag vic vec1 vec2)


getEnumProperty :: Bits.BitGet Value.Value
getEnumProperty = do
    x <- Bits.getWord16be 10
    y <- if x == 1023
        then getBool
        else fail ("unexpected enum value " ++ show x)
    pure (Value.VEnum (Word16.toWord16 x) y)


getExplosionProperty :: Bits.BitGet Value.Value
getExplosionProperty = do
    noGoal <- getBool
    a <- if #unpack noGoal
        then pure Nothing
        else fmap Just getInt32
    b <- Vector.getIntVector
    pure (Value.VExplosion noGoal a b)


getFlaggedIntProperty :: Bits.BitGet Value.Value
getFlaggedIntProperty = do
    flag <- getBool
    int <- getInt32
    pure (Value.VFlaggedInt flag int)


getFloatProperty :: Bits.BitGet Value.Value
getFloatProperty = do
    float <- getFloat32
    pure (Value.VFloat float)


getGameModeProperty :: Context -> Bits.BitGet Value.Value
getGameModeProperty context = do
    let numBits = if atLeastNeoTokyo context then 8 else 2
    x <- Bits.getWord8 numBits
    pure (Value.VGameMode (Word8.toWord8 x))


getIntProperty :: Bits.BitGet Value.Value
getIntProperty = do
    int <- getInt32
    pure (Value.VInt int)


getLoadoutOnlineProperty :: Bits.BitGet Value.Value
getLoadoutOnlineProperty = do
    size <- fmap Word8.fromWord8 getWord8
    values <- Monad.replicateM size (do
        innerSize <- fmap Word8.fromWord8 getWord8
        Monad.replicateM innerSize (do
            x <- getWord32
            y <- BinaryBit.getBits 27
            pure (x, y)))
    pure (Value.VLoadoutOnline values)


getLoadoutProperty :: Bits.BitGet Value.Value
getLoadoutProperty = do
    version <- getWord8
    body <- getWord32
    decal <- getWord32
    wheels <- getWord32
    rocketTrail <- getWord32
    antenna <- getWord32
    topper <- getWord32
    g <- getWord32
    h <- if version > 10 then fmap Just getWord32 else pure Nothing
    pure (Value.VLoadout version body decal wheels rocketTrail antenna topper g h)


getLocationProperty :: Bits.BitGet Value.Value
getLocationProperty = do
    vector <- Vector.getIntVector
    pure (Value.VLocation vector)


getMusicStingerProperty :: Bits.BitGet Value.Value
getMusicStingerProperty = do
    flag <- getBool
    cue <- getWord32
    trigger <- getWord8
    pure (Value.VMusicStinger flag cue trigger)


getPickupProperty :: Bits.BitGet Value.Value
getPickupProperty = do
    instigator <- getBool
    instigatorId <- if #unpack instigator
        then fmap Just getWord32
        else pure Nothing
    pickedUp <- getBool
    pure (Value.VPickup instigator instigatorId pickedUp)


getPrivateMatchSettingsProperty :: Bits.BitGet Value.Value
getPrivateMatchSettingsProperty = do
    mutators <- getText
    joinableBy <- getWord32
    maxPlayers <- getWord32
    gameName <- getText
    password <- getText
    flag <- getBool
    pure (Value.VPrivateMatchSettings mutators joinableBy maxPlayers gameName password flag)


getQWordProperty :: Bits.BitGet Value.Value
getQWordProperty = do
    qword <- getWord64
    pure (Value.VQWord qword)


getRelativeRotationProperty :: Bits.BitGet Value.Value
getRelativeRotationProperty = do
    vector <- Vector.getFloatVector
    pure (Value.VRelativeRotation vector)


getReservationProperty :: Context -> Bits.BitGet Value.Value
getReservationProperty context = do
    -- I think this is the connection order. The first player to connect
    -- gets number 0, and it goes up from there. The maximum is 7, which
    -- would be a full 4x4 game.
    number <- BinaryBit.getBits maxConnectionNumber
    (systemId, remoteId, localId) <- getUniqueId
    playerName <- if systemId == 0 then pure Nothing else fmap Just getText
    -- No idea what these two flags are. Might be for bots?
    a <- getBool
    b <- getBool

    -- The Neo Tokyo update added 6 bits to the reservation property that are
    -- always (as far as I can tell) 0.
    Monad.when (atLeastNeoTokyo context) (do
        x <- Bits.getWord8 6
        Monad.when (x /= 0b000000) (do
            fail (Printf.printf "Read 6 reservation bits and they weren't all 0! 0b%06b" x)))

    pure (Value.VReservation number systemId remoteId localId playerName a b)


getRigidBodyStateProperty :: Bits.BitGet Value.Value
getRigidBodyStateProperty = do
    flag <- getBool
    position <- Vector.getIntVector
    rotation <- Vector.getFloatVector
    x <- if #unpack flag
        then pure Nothing
        else fmap Just Vector.getIntVector
    y <- if #unpack flag
        then pure Nothing
        else fmap Just Vector.getIntVector
    pure (Value.VRigidBodyState flag position rotation x y)


getStringProperty :: Bits.BitGet Value.Value
getStringProperty = do
    string <- getText
    pure (Value.VString string)


getTeamPaintProperty :: Bits.BitGet Value.Value
getTeamPaintProperty = do
    team <- getWord8
    primaryColor <- getWord8
    accentColor <- getWord8
    primaryFinish <- getWord32
    accentFinish <- getWord32
    pure (Value.VTeamPaint team primaryColor accentColor primaryFinish accentFinish)


getUniqueIdProperty :: Bits.BitGet Value.Value
getUniqueIdProperty = do
    (systemId, remoteId, localId) <- getUniqueId
    pure (Value.VUniqueId systemId remoteId localId)


-- | Even though this is just a unique ID property, it must be handled
-- specially because it sometimes doesn't have the remote or local IDs.
getPartyLeaderProperty :: Bits.BitGet Value.Value
getPartyLeaderProperty = do
    systemId <- getWord8
    (remoteId, localId) <- if systemId == 0
        then pure (RemoteId.RemoteSplitscreenId (RemoteId.SplitscreenId Nothing), Nothing)
        else do
            remoteId <- getRemoteId systemId
            localId <- fmap Just getWord8
            pure (remoteId, localId)
    pure (Value.VUniqueId systemId remoteId localId)


getUniqueId :: Bits.BitGet (Word8.Word8, RemoteId.RemoteId, Maybe Word8.Word8)
getUniqueId = do
    systemId <- getWord8
    remoteId <- getRemoteId systemId
    localId <- fmap Just getWord8
    pure (systemId, remoteId, localId)


getRemoteId :: Word8.Word8 -> Bits.BitGet RemoteId.RemoteId
getRemoteId systemId = case systemId of
    0 -> do
        splitscreenId <- BinaryBit.getBits 0
        pure (RemoteId.RemoteSplitscreenId splitscreenId)
    1 -> do
        steamId <- BinaryBit.getBits 0
        pure (RemoteId.RemoteSteamId steamId)
    2 -> do
        playStationId <- BinaryBit.getBits 0
        pure (RemoteId.RemotePlayStationId playStationId)
    4 -> do
        xboxId <- BinaryBit.getBits 0
        pure (RemoteId.RemoteXboxId xboxId)
    _ -> fail ("unknown system id " ++ show systemId)


-- Data types


data Thing = Thing
    { thingFlag :: Boolean.Boolean
    , thingObjectId :: Int32.Int32
    , thingObjectName :: StrictText.Text
    , thingClassId :: Int
    , thingClassName :: StrictText.Text
    , thingInitialization :: Initialization.Initialization
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Thing


-- { class stream id => { property stream id => name } }
type ClassPropertyMap = IntMap.IntMap (IntMap.IntMap StrictText.Text)


-- { stream id => object name }
type ObjectMap = IntMap.IntMap StrictText.Text


-- { class name => class id }
type ClassMap = Map.Map StrictText.Text Int


data Context = Context
    { contextObjectMap :: ObjectMap
    , contextClassPropertyMap :: ClassPropertyMap
    , contextThings :: (IntMap.IntMap Thing)
    , contextClassMap :: ClassMap
    , contextKeyFrames :: (Set.Set Word)
    , contextVersion :: Version.Version
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Context


extractContext :: ReplayWithoutFrames.ReplayWithoutFrames -> Context
extractContext replay = Context
    { contextObjectMap = CPM.getPropertyMap replay
    , contextClassPropertyMap = CPM.getClassPropertyMap replay
    , contextThings = IntMap.empty
    , contextClassMap = CPM.getActorMap replay
    , contextKeyFrames = replay
        & #keyFrames
        & #unpack
        & map #frame
        & map Word32.fromWord32
        & Set.fromList
    , contextVersion =
        [ replay & #version1
        , replay & #version2
        ] & map Word32.fromWord32 & Version.makeVersion
    }


-- Helpers


-- Some values are parsed differently depending on the version of the game that
-- saved them. So far, all differences happened with the Neo Tokyo patch. This
-- function takes a context and returns true if the replay was saved by a game
-- running at least the Neo Tokyo version.
atLeastNeoTokyo :: Context -> Bool
atLeastNeoTokyo context = contextVersion context >= neoTokyoVersion


-- Constants


neoTokyoVersion :: Version.Version
neoTokyoVersion = Version.makeVersion [868, 12]


maxActorId :: Int
maxActorId = 1024


maxConnectionNumber :: Int
maxConnectionNumber = 7


-- Type-restricted helpers.


getBool :: Bits.BitGet Boolean.Boolean
getBool = BinaryBit.getBits 0


getFloat32 :: Bits.BitGet Float32.Float32
getFloat32 = BinaryBit.getBits 0


getInt32 :: Bits.BitGet Int32.Int32
getInt32 = BinaryBit.getBits 0


getText :: Bits.BitGet Text.Text
getText = BinaryBit.getBits 0


getWord8 :: Bits.BitGet Word8.Word8
getWord8 = BinaryBit.getBits 0


getWord32 :: Bits.BitGet Word32.Word32
getWord32 = BinaryBit.getBits 0


getWord64 :: Bits.BitGet Word64.Word64
getWord64 = BinaryBit.getBits 0
