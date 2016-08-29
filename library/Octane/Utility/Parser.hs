{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Utility.Parser (parseStream) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Default.Class as Default
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.OverloadedRecords.TH as OverloadedRecords
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
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Value as Value
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word16 as Word16
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Type.Word8 as Word8
import qualified Octane.Utility.ClassPropertyMap as CPM
import qualified Text.Printf as Printf


-- Data types


-- { class stream id => { property stream id => name } }
type ClassPropertyMap = IntMap.IntMap (IntMap.IntMap StrictText.Text)


-- { stream id => object name }
type ObjectMap = IntMap.IntMap StrictText.Text


-- { class name => class id }
type ClassMap = Map.Map StrictText.Text Int


data Thing = Thing
    { thingFlag :: Boolean.Boolean
    , thingObjectId :: Int32.Int32
    , thingObjectName :: StrictText.Text
    , thingClassId :: Int
    , thingClassName :: StrictText.Text
    , thingInitialization :: Initialization.Initialization
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Thing)

instance DeepSeq.NFData Thing


data Context = Context
    { contextObjectMap :: ObjectMap
    , contextClassPropertyMap :: ClassPropertyMap
    , contextThings :: IntMap.IntMap Thing
    , contextClassMap :: ClassMap
    , contextKeyFrames :: Set.Set Word
    , contextVersion :: Version.Version
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Context)

instance DeepSeq.NFData Context


extractContext :: ReplayWithoutFrames.ReplayWithoutFrames -> Context
extractContext replay = do
    let keyFrames = replay
            & #keyFrames
            & #unpack
            & map #frame
            & map Word32.fromWord32
            & Set.fromList
    let version =
            [ replay & #version1
            , replay & #version2
            ] & map Word32.fromWord32 & Version.makeVersion
    Context
        (CPM.getPropertyMap replay)
        (CPM.getClassPropertyMap replay)
        IntMap.empty
        (CPM.getActorMap replay)
        keyFrames
        version


-- | Parses the network stream and returns a list of frames.
parseStream :: ReplayWithoutFrames.ReplayWithoutFrames -> [Frame.Frame]
parseStream replay = let
    numFrames = replay
        & #properties
        & #unpack
        & Map.lookup ("NumFrames" & StrictText.pack & Text.Text)
        & (\ property -> case property of
            Just (Property.PropertyInt int) -> int & #content & Int32.fromInt32
            _ -> 0)
    get = replay & extractContext & getFrames 0 numFrames & BinaryBit.runBitGet
    stream = replay & #stream & #unpack
    (_context, frames) = Binary.runGet get stream
    in frames


getFrames :: Word -> Int -> Context -> BinaryBit.BitGet (Context, [Frame.Frame])
getFrames number numFrames context = do
    if fromIntegral number >= numFrames
    then pure (context, [])
    else do
        isEmpty <- BinaryBit.isEmpty
        if isEmpty
        then pure (context, [])
        else do
            maybeFrame <- getMaybeFrame context number
            case maybeFrame of
                Nothing -> pure (context, [])
                Just (newContext, frame) -> do
                    (newerContext, frames) <- getFrames (number + 1) numFrames newContext
                    pure (newerContext, (frame : frames))


getMaybeFrame :: Context -> Word -> BinaryBit.BitGet (Maybe (Context, Frame.Frame))
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


getFrame :: Context -> Word -> Float32.Float32 -> Float32.Float32 -> BinaryBit.BitGet (Context, Frame.Frame)
getFrame context number time delta = do
    (newContext, replications) <- getReplications context
    let frame = Frame.Frame
            number
            (context & #keyFrames & Set.member number)
            time
            delta
            replications
    (newContext, frame) & DeepSeq.force & pure


getReplications :: Context -> BinaryBit.BitGet (Context, [Replication.Replication])
getReplications context = do
    maybeReplication <- getMaybeReplication context
    case maybeReplication of
        Nothing -> pure (context, [])
        Just (newContext, replication) -> do
            (newerContext, replications) <- getReplications newContext
            pure (newerContext, replication : replications)


getMaybeReplication :: Context -> BinaryBit.BitGet (Maybe (Context, Replication.Replication))
getMaybeReplication context = do
    hasReplication <- getBool
    if #unpack hasReplication
        then do
            (newContext,replication) <- getReplication context
            pure (Just (newContext, replication))
        else pure Nothing


getReplication :: Context -> BinaryBit.BitGet (Context, Replication.Replication)
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
                   -> BinaryBit.BitGet (Context, Replication.Replication)
getOpenReplication context actorId = do
    isNew <- getBool
    let go =
            if #unpack isNew
                then getNewReplication
                else getExistingReplication
    go context actorId


getNewReplication :: Context
                  -> CompressedWord.CompressedWord
                  -> BinaryBit.BitGet (Context, Replication.Replication)
getNewReplication context actorId = do
    unknownFlag <- getBool
    if #unpack unknownFlag
        then fail "the unknown flag in a new replication is true! what does it mean?"
        else pure ()
    objectId <- getInt32
    objectName <- case context & #objectMap & IntMap.lookup (Int32.fromInt32 objectId) of
        Nothing -> fail ("could not find object name for id " ++ show objectId)
        Just x -> pure x
    (classId, className) <- CPM.getClass
        (#objectMap context)
        Data.classes
        (#classMap context)
        (Int32.fromInt32 objectId)
    classInit <- Initialization.getInitialization className
    let thing = Thing
            unknownFlag
            objectId
            objectName
            classId
            className
            classInit
    let things = #things context
    let newThings = IntMap.insert (CompressedWord.fromCompressedWord actorId) thing things
    let newContext = context { contextThings = newThings }
    pure (newContext, Replication.Replication
        actorId
        objectName
        className
        State.SOpening
        (Just classInit)
        Map.empty)


getExistingReplication :: Context
                       -> CompressedWord.CompressedWord
                       -> BinaryBit.BitGet (Context, Replication.Replication)
getExistingReplication context actorId = do
    thing <- case context & #things & IntMap.lookup (CompressedWord.fromCompressedWord actorId) of
        Nothing -> fail ("could not find thing for existing actor " ++ show actorId)
        Just x -> pure x
    props <- getProps context thing
    pure (context, Replication.Replication
        actorId
        (#objectName thing)
        (#className thing)
        State.SExisting
        Nothing
        props)


getClosedReplication :: Context
                     -> CompressedWord.CompressedWord
                     -> BinaryBit.BitGet (Context, Replication.Replication)
getClosedReplication context actorId = do
    thing <- case context & #things & IntMap.lookup (CompressedWord.fromCompressedWord actorId) of
        Nothing -> fail ("could not find thing for closed actor " ++ show actorId)
        Just x -> pure x
    let newThings = context & #things & IntMap.delete (CompressedWord.fromCompressedWord actorId)
    let newContext = context { contextThings = newThings }
    pure (newContext, Replication.Replication
          actorId
          (#objectName thing)
          (#className thing)
          State.SClosing
          Nothing
          Map.empty)


getProps :: Context -> Thing -> BinaryBit.BitGet (Map.Map StrictText.Text Value.Value)
getProps context thing = do
    maybeProp <- getMaybeProp context thing
    case maybeProp of
        Nothing -> pure Map.empty
        Just (k, v) -> do
            let m = Map.singleton k v
            props <- getProps context thing
            pure (Map.union m props)


getMaybeProp :: Context -> Thing -> BinaryBit.BitGet (Maybe (StrictText.Text, Value.Value))
getMaybeProp context thing = do
    hasProp <- getBool
    if #unpack hasProp
    then do
        prop <- getProp context thing
        pure (Just prop)
    else pure Nothing


getProp :: Context -> Thing -> BinaryBit.BitGet (StrictText.Text, Value.Value)
getProp context thing = do
    let classId = #classId thing
    props <- case context & #classPropertyMap & IntMap.lookup classId of
        Nothing -> fail ("could not find property map for class id " ++ show classId)
        Just x -> pure x
    let maxId = props & IntMap.keys & (0 :) & maximum
    pid <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits maxId)
    name <- case props & IntMap.lookup pid of
        Nothing -> fail ("could not find property name for property id " ++ show pid)
        Just x -> pure x
    value <- getPropValue context name
    pure (name, value)


getPropValue :: Context -> StrictText.Text -> BinaryBit.BitGet Value.Value
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


getBooleanProperty :: BinaryBit.BitGet Value.Value
getBooleanProperty = do
    bool <- getBool
    pure (Value.ValueBoolean (Value.BooleanValue bool))


getByteProperty :: BinaryBit.BitGet Value.Value
getByteProperty = do
    word <- getWord8
    pure (Value.ValueByte (Value.ByteValue word))


getCamSettingsProperty :: BinaryBit.BitGet Value.Value
getCamSettingsProperty = do
    fov <- getFloat32
    height <- getFloat32
    angle <- getFloat32
    distance <- getFloat32
    stiffness <- getFloat32
    swivelSpeed <- getFloat32
    pure (Value.ValueCamSettings (Value.CamSettingsValue fov height angle distance stiffness swivelSpeed))


getDemolishProperty :: BinaryBit.BitGet Value.Value
getDemolishProperty = do
    attackerFlag <- getBool
    attackerActorId <- getWord32
    victimFlag <- getBool
    victimActorId <- getWord32
    attackerVelocity <- Vector.getIntVector
    victimVelocity <- Vector.getIntVector
    pure (Value.ValueDemolish (Value.DemolishValue attackerFlag attackerActorId victimFlag victimActorId attackerVelocity victimVelocity))


getEnumProperty :: BinaryBit.BitGet Value.Value
getEnumProperty = do
    x <- BinaryBit.getWord16be 10
    y <- if x == 1023
        then getBool
        else fail ("unexpected enum value " ++ show x)
    pure (Value.ValueEnum (Value.EnumValue (Word16.toWord16 x) y))


getExplosionProperty :: BinaryBit.BitGet Value.Value
getExplosionProperty = do
    x <- getBool
    y <- if #unpack x
        then pure Nothing
        else fmap Just getInt32
    z <- Vector.getIntVector
    pure (Value.ValueExplosion (Value.ExplosionValue x y z))


getFlaggedIntProperty :: BinaryBit.BitGet Value.Value
getFlaggedIntProperty = do
    flag <- getBool
    int <- getInt32
    pure (Value.ValueFlaggedInt (Value.FlaggedIntValue flag int))


getFloatProperty :: BinaryBit.BitGet Value.Value
getFloatProperty = do
    float <- getFloat32
    pure (Value.ValueFloat (Value.FloatValue float))


getGameModeProperty :: Context -> BinaryBit.BitGet Value.Value
getGameModeProperty context = do
    let numBits = if atLeastNeoTokyo context then 8 else 2
    x <- BinaryBit.getWord8 numBits
    pure (Value.ValueGameMode (Value.GameModeValue (Word8.toWord8 x)))


getIntProperty :: BinaryBit.BitGet Value.Value
getIntProperty = do
    int <- getInt32
    pure (Value.ValueInt (Value.IntValue int))


getLoadoutOnlineProperty :: BinaryBit.BitGet Value.Value
getLoadoutOnlineProperty = do
    size <- fmap Word8.fromWord8 getWord8
    values <- Monad.replicateM size (do
        innerSize <- fmap Word8.fromWord8 getWord8
        Monad.replicateM innerSize (do
            x <- getWord32
            y <- BinaryBit.getBits 27
            pure (x, y)))
    pure (Value.ValueLoadoutOnline (Value.LoadoutOnlineValue values))


getLoadoutProperty :: BinaryBit.BitGet Value.Value
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
    pure (Value.ValueLoadout (Value.LoadoutValue version body decal wheels rocketTrail antenna topper g h))


getLocationProperty :: BinaryBit.BitGet Value.Value
getLocationProperty = do
    vector <- Vector.getIntVector
    pure (Value.ValueLocation (Value.LocationValue vector))


getMusicStingerProperty :: BinaryBit.BitGet Value.Value
getMusicStingerProperty = do
    flag <- getBool
    cue <- getWord32
    trigger <- getWord8
    pure (Value.ValueMusicStinger (Value.MusicStingerValue flag cue trigger))


getPickupProperty :: BinaryBit.BitGet Value.Value
getPickupProperty = do
    instigator <- getBool
    instigatorId <- if #unpack instigator
        then fmap Just getWord32
        else pure Nothing
    pickedUp <- getBool
    pure (Value.ValuePickup (Value.PickupValue instigator instigatorId pickedUp))


getPrivateMatchSettingsProperty :: BinaryBit.BitGet Value.Value
getPrivateMatchSettingsProperty = do
    mutators <- getText
    joinableBy <- getWord32
    maxPlayers <- getWord32
    gameName <- getText
    password <- getText
    flag <- getBool
    pure (Value.ValuePrivateMatchSettings (Value.PrivateMatchSettingsValue mutators joinableBy maxPlayers gameName password flag))


getQWordProperty :: BinaryBit.BitGet Value.Value
getQWordProperty = do
    qword <- getWord64
    pure (Value.ValueQWord (Value.QWordValue qword))


getRelativeRotationProperty :: BinaryBit.BitGet Value.Value
getRelativeRotationProperty = do
    vector <- Vector.getFloatVector
    pure (Value.ValueRelativeRotation (Value.RelativeRotationValue vector))


getReservationProperty :: Context -> BinaryBit.BitGet Value.Value
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
        x <- BinaryBit.getWord8 6
        Monad.when (x /= 0b000000) (do
            fail (Printf.printf "Read 6 reservation bits and they weren't all 0! 0b%06b" x)))

    pure (Value.ValueReservation (Value.ReservationValue number systemId remoteId localId playerName a b))


getRigidBodyStateProperty :: BinaryBit.BitGet Value.Value
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


getStringProperty :: BinaryBit.BitGet Value.Value
getStringProperty = do
    string <- getText
    pure (Value.VString string)


getTeamPaintProperty :: BinaryBit.BitGet Value.Value
getTeamPaintProperty = do
    team <- getWord8
    primaryColor <- getWord8
    accentColor <- getWord8
    primaryFinish <- getWord32
    accentFinish <- getWord32
    pure (Value.VTeamPaint team primaryColor accentColor primaryFinish accentFinish)


getUniqueIdProperty :: BinaryBit.BitGet Value.Value
getUniqueIdProperty = do
    (systemId, remoteId, localId) <- getUniqueId
    pure (Value.VUniqueId systemId remoteId localId)


-- | Even though this is just a unique ID property, it must be handled
-- specially because it sometimes doesn't have the remote or local IDs.
getPartyLeaderProperty :: BinaryBit.BitGet Value.Value
getPartyLeaderProperty = do
    systemId <- getWord8
    (remoteId, localId) <- if systemId == 0
        then pure (RemoteId.RemoteSplitscreenId (RemoteId.SplitscreenId Nothing), Nothing)
        else do
            remoteId <- getRemoteId systemId
            localId <- fmap Just getWord8
            pure (remoteId, localId)
    pure (Value.VUniqueId systemId remoteId localId)


getUniqueId :: BinaryBit.BitGet (Word8.Word8, RemoteId.RemoteId, Maybe Word8.Word8)
getUniqueId = do
    systemId <- getWord8
    remoteId <- getRemoteId systemId
    localId <- fmap Just getWord8
    pure (systemId, remoteId, localId)


getRemoteId :: Word8.Word8 -> BinaryBit.BitGet RemoteId.RemoteId
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


-- Helpers


-- Some values are parsed differently depending on the version of the game that
-- saved them. So far, all differences happened with the Neo Tokyo patch. This
-- function takes a context and returns true if the replay was saved by a game
-- running at least the Neo Tokyo version.
atLeastNeoTokyo :: Context -> Bool
atLeastNeoTokyo context = #version context >= neoTokyoVersion


-- Constants


neoTokyoVersion :: Version.Version
neoTokyoVersion = Version.makeVersion [868, 12]


maxActorId :: Int
maxActorId = 1024


maxConnectionNumber :: Int
maxConnectionNumber = 7


-- Type-restricted helpers.


getBool :: BinaryBit.BitGet Boolean.Boolean
getBool = BinaryBit.getBits 0


getFloat32 :: BinaryBit.BitGet Float32.Float32
getFloat32 = BinaryBit.getBits 0


getInt32 :: BinaryBit.BitGet Int32.Int32
getInt32 = BinaryBit.getBits 0


getText :: BinaryBit.BitGet Text.Text
getText = BinaryBit.getBits 0


getWord8 :: BinaryBit.BitGet Word8.Word8
getWord8 = BinaryBit.getBits 0


getWord32 :: BinaryBit.BitGet Word32.Word32
getWord32 = BinaryBit.getBits 0


getWord64 :: BinaryBit.BitGet Word64.Word64
getWord64 = BinaryBit.getBits 0
