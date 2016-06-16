{-# LANGUAGE DeriveGeneric #-}

module Octane.Parser where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Int as Int
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Octane.Data as Data
import qualified Octane.Json as Json
import qualified Octane.Parser.ClassPropertyMap as CPM
import qualified Octane.Type as Type
import qualified Octane.Utility as Utility
import qualified Text.Printf as Printf

parseFrames :: Type.Replay -> [Frame]
parseFrames replay = let
    numFrames = replay
        & Type.replayProperties
        & Type.unpackDictionary
        & Map.lookup ("NumFrames" & Text.pack & Type.PCString)
        & (\ property -> case property of
            Just (Type.IntProperty _ x) -> x & Type.unpackInt32 & fromIntegral
            _ -> 0)
    get = replay & extractContext & getFrames 0 numFrames & Bits.runBitGet
    stream = replay & Type.replayStream & Type.unpackStream & BSL.fromStrict
    (_context, frames) = Binary.runGet get stream
    in frames

getFrames :: Int -> Int -> Context -> Bits.BitGet (Context, [Frame])
getFrames number numFrames context = do
    if number >= numFrames
    then return (context, [])
    else do
        isEmpty <- Bits.isEmpty
        if isEmpty
        then return (context, [])
        else do
            maybeFrame <- getMaybeFrame context number
            case maybeFrame of
                Nothing -> return (context, [])
                Just (newContext, frame) -> do
                    (newerContext, frames) <- getFrames (number + 1) numFrames newContext
                    return (newerContext, (frame : frames))

getMaybeFrame :: Context -> Int -> Bits.BitGet (Maybe (Context, Frame))
getMaybeFrame context number = do
    time <- getFloat32
    delta <- getFloat32
    if time == 0 && delta == 0
    then return Nothing
    else if time < 0.001 || delta < 0.001
    then error ("parsing previous frame probably failed. time: " ++ show time ++ ", delta: " ++ show delta)
    else do
        (newContext, frame) <- getFrame context number time delta
        return (Just (newContext, frame))

getFrame :: Context -> Int -> Time -> Delta -> Bits.BitGet (Context, Frame)
getFrame context number time delta = do
    (newContext, replications) <- getReplications context
    let frame =
            Frame
            { frameNumber = number
            , frameIsKeyFrame = context & contextKeyFrames & Set.member number
            , frameTime = time
            , frameDelta = delta
            , frameReplications = replications
            }
    (newContext, frame) & DeepSeq.force & return

getReplications :: Context -> Bits.BitGet (Context, [Replication])
getReplications context = do
    maybeReplication <- getMaybeReplication context
    case maybeReplication of
        Nothing -> return (context, [])
        Just (newContext, replication) -> do
            (newerContext, replications) <- getReplications newContext
            return (newerContext, replication : replications)

getMaybeReplication :: Context -> Bits.BitGet (Maybe (Context, Replication))
getMaybeReplication context = do
    hasReplication <- getBool
    if Type.unpackBoolean hasReplication
        then do
            (newContext,replication) <- getReplication context
            return (Just (newContext, replication))
        else return Nothing

getReplication :: Context -> Bits.BitGet (Context, Replication)
getReplication context = do
    actorId <- getActorId
    isOpen <- getBool
    let go =
            if Type.unpackBoolean isOpen
                then getOpenReplication
                else getClosedReplication
    go context actorId

getOpenReplication :: Context
                   -> ActorId
                   -> Bits.BitGet (Context, Replication)
getOpenReplication context actorId = do
    isNew <- getBool
    let go =
            if Type.unpackBoolean isNew
                then getNewReplication
                else getExistingReplication
    go context actorId

getNewReplication :: Context
                  -> ActorId
                  -> Bits.BitGet (Context, Replication)
getNewReplication context actorId = do
    unknownFlag <- getBool
    if Type.unpackBoolean unknownFlag
        then error "the unknown flag in a new replication is true! what does it mean?"
        else return ()
    objectId <- getInt32
    let objectName = case context & contextObjectMap & IntMap.lookup objectId of
            Nothing -> error ("could not find object name for id " ++ show objectId)
            Just x -> x
    let (classId,className) = case CPM.getClass (contextObjectMap context) Data.objectToClass (contextClassMap context) objectId of
            Nothing -> error ("could not find class for object id " ++ show objectId)
            Just x -> x
    classInit <- getClassInit className
    let thing = Thing
            { thingFlag = unknownFlag
            , thingObjectId = objectId
            , thingObjectName = objectName
            , thingClassId = classId
            , thingClassName = className
            , thingClassInit = classInit
            }
    let things = contextThings context
    let newThings = IntMap.insert actorId thing things
    let newContext = context { contextThings = newThings }
    return
        ( newContext
        , Replication
          { replicationActorId = actorId
          , replicationObjectName = objectName
          , replicationClassName = className
          , replicationState = RSOpening
          , replicationInitialization = Just classInit
          , replicationProperties = Map.empty
          })

getExistingReplication :: Context
                       -> ActorId
                       -> Bits.BitGet (Context, Replication)
getExistingReplication context actorId = do
    let thing = case context & contextThings & IntMap.lookup actorId of
            Nothing -> error ("could not find thing for actor id " ++ show actorId)
            Just x -> x
    props <- getProps context thing
    return (context, Replication
        { replicationActorId = actorId
        , replicationObjectName = thingObjectName thing
        , replicationClassName = thingClassName thing
        , replicationState = RSExisting
        , replicationInitialization = Nothing
        , replicationProperties = props
        })

getClosedReplication :: Context
                     -> ActorId
                     -> Bits.BitGet (Context, Replication)
getClosedReplication context actorId = do
    let thing = case context & contextThings & IntMap.lookup actorId of
            Nothing -> error ("could not find thing for actor id " ++ show actorId)
            Just x -> x
    let newThings = context & contextThings & IntMap.delete actorId
    let newContext = context { contextThings = newThings }
    return
        ( newContext
        , Replication
          { replicationActorId = actorId
          , replicationObjectName = thingObjectName thing
          , replicationClassName = thingClassName thing
          , replicationState = RSClosing
          , replicationInitialization = Nothing
          , replicationProperties = Map.empty
          })

getProps :: Context -> Thing -> Bits.BitGet (Map.Map Text.Text PropValue)
getProps context thing = do
    maybeProp <- getMaybeProp context thing
    case maybeProp of
        Nothing -> return Map.empty
        Just prop -> do
            let k = propName prop
            let v = propValue prop
            let m = Map.singleton k v
            props <- getProps context thing
            return (Map.union m props)

getMaybeProp :: Context -> Thing -> Bits.BitGet (Maybe Prop)
getMaybeProp context thing = do
    hasProp <- getBool
    if Type.unpackBoolean hasProp
    then do
        prop <- getProp context thing
        return (Just prop)
    else return Nothing

getProp :: Context -> Thing -> Bits.BitGet Prop
getProp context thing = do
    let classId = thing & thingClassId
    let props = case context & contextClassPropertyMap & IntMap.lookup classId of
            Nothing -> error ("could not find property map for class id " ++ show classId)
            Just x -> x
    let maxId = props & IntMap.keys & (0 :) & maximum
    pid <- getInt maxId
    let name = case props & IntMap.lookup pid of
            Nothing -> error ("could not find property name for property id " ++ show pid)
            Just x -> x
    value <- getPropValue name
    return (Prop { propName = name, propValue = value })

--

getPropValue :: Text.Text -> Bits.BitGet PropValue
getPropValue name = case Map.lookup name propertyNameToGet of
    Nothing -> error ("don't know how to read property " ++ show name)
    Just get -> get

propertyNameToGet :: Map.Map Text.Text (Bits.BitGet PropValue)
propertyNameToGet =
    [ (Data.booleanProperties, getBooleanProperty)
    , (Data.byteProperties, getByteProperty)
    , (Data.camSettingsProperties, getCamSettingsProperty)
    , (Data.demolishProperties, getDemolishProperty)
    , (Data.enumProperties, getEnumProperty)
    , (Data.explosionProperties, getExplosionProperty)
    , (Data.flaggedIntProperties, getFlaggedIntProperty)
    , (Data.floatProperties, getFloatProperty)
    , (Data.gameModeProperties, getGameModeProperty)
    , (Data.intProperties, getIntProperty)
    , (Data.loadoutOnlineProperties, getLoadoutOnlineProperty)
    , (Data.loadoutProperties, getLoadoutProperty)
    , (Data.locationProperties, getLocationProperty)
    , (Data.musicStingerProperties, getMusicStingerProperty)
    , (Data.pickupProperties, getPickupProperty)
    , (Data.privateMatchSettingsProperties, getPrivateMatchSettingsProperty)
    , (Data.qWordProperties, getQWordProperty)
    , (Data.relativeRotationProperties, getRelativeRotationProperty)
    , (Data.reservationProperties, getReservationProperty)
    , (Data.rigidBodyStateProperties, getRigidBodyStateProperty)
    , (Data.stringProperties, getStringProperty)
    , (Data.teamPaintProperties, getTeamPaintProperty)
    , (Data.uniqueIdProperties, getUniqueIdProperty)
    , (Set.fromList [Text.pack "TAGame.PRI_TA:PartyLeader"], getPartyLeaderProperty)
    ]
        & concatMap (\ (ks, v) -> ks & Set.toList & map (\ k -> (k, v)))
        & Map.fromList

getBooleanProperty :: Bits.BitGet PropValue
getBooleanProperty = do
    bool <- getBool
    return (PBoolean bool)

getByteProperty :: Bits.BitGet PropValue
getByteProperty = do
    word <- getWord8
    return (PByte word)

getCamSettingsProperty :: Bits.BitGet PropValue
getCamSettingsProperty = do
    fov <- getFloat32
    height <- getFloat32
    angle <- getFloat32
    distance <- getFloat32
    stiffness <- getFloat32
    swivelSpeed <- getFloat32
    return (PCamSettings fov height angle distance stiffness swivelSpeed)

getDemolishProperty :: Bits.BitGet PropValue
getDemolishProperty = do
    atkFlag <- getBool
    atk <- getInt32
    vicFlag <- getBool
    vic <- getInt32
    vec1 <- getVector
    vec2 <- getVector
    return (PDemolish atkFlag atk vicFlag vic vec1 vec2)

getEnumProperty :: Bits.BitGet PropValue
getEnumProperty = do
    x <- Bits.getWord16be 10
    y <- if x == 1023
        then getBool
        else error ("unexpected enum value " ++ show x)
    return (PEnum x y)

getExplosionProperty :: Bits.BitGet PropValue
getExplosionProperty = do
    noGoal <- getBool
    a <- if Type.unpackBoolean noGoal
        then return Nothing
        else fmap Just getInt32
    b <- getVector
    return (PExplosion noGoal a b)

getFlaggedIntProperty :: Bits.BitGet PropValue
getFlaggedIntProperty = do
    flag <- getBool
    int <- getInt32
    return (PFlaggedInt flag (fromIntegral int))

getFloatProperty :: Bits.BitGet PropValue
getFloatProperty = do
    float <- getFloat32
    return (PFloat float)

getGameModeProperty :: Bits.BitGet PropValue
getGameModeProperty = do
    x <- Bits.getWord8 2
    return (PGameMode x)

getIntProperty :: Bits.BitGet PropValue
getIntProperty = do
    int <- getInt32
    return (PInt int)

getLoadoutOnlineProperty :: Bits.BitGet PropValue
getLoadoutOnlineProperty = do
    version <- getInt32
    x <- getInt32
    y <- getInt32
    z <- if version >= 12
        then do
            value <- getInt8
            return (Just value)
        else return Nothing
    return (PLoadoutOnline version x y z)

getLoadoutProperty :: Bits.BitGet PropValue
getLoadoutProperty = do
    version <- getInt8
    body <- getInt32
    decal <- getInt32
    wheels <- getInt32
    rocketTrail <- getInt32
    antenna <- getInt32
    topper <- getInt32
    g <- getInt32
    h <- if version > 10
        then do
            value <- getInt32
            return (Just value)
        else return Nothing
    return (PLoadout version body decal wheels rocketTrail antenna topper g h)

getLocationProperty :: Bits.BitGet PropValue
getLocationProperty = do
    vector <- getVector
    return (PLocation vector)

getMusicStingerProperty :: Bits.BitGet PropValue
getMusicStingerProperty = do
    flag <- getBool
    cue <- getInt32
    trigger <- getInt8
    return (PMusicStinger flag cue trigger)

getPickupProperty :: Bits.BitGet PropValue
getPickupProperty = do
    instigator <- getBool
    instigatorId <- if Type.unpackBoolean instigator
        then fmap Just getInt32
        else return Nothing
    pickedUp <- getBool
    return (PPickup instigator instigatorId pickedUp)

getPrivateMatchSettingsProperty :: Bits.BitGet PropValue
getPrivateMatchSettingsProperty = do
    mutators <- getString
    joinableBy <- getInt32
    maxPlayers <- getInt32
    gameName <- getString
    password <- getString
    flag <- getBool
    return (PPrivateMatchSettings mutators joinableBy maxPlayers gameName password flag)

getQWordProperty :: Bits.BitGet PropValue
getQWordProperty = do
    x <- getInt32
    y <- getInt32
    return (PQWord x y)

getRelativeRotationProperty :: Bits.BitGet PropValue
getRelativeRotationProperty = do
    vector <- getFloatVector
    return (PRelativeRotation vector)

getReservationProperty :: Bits.BitGet PropValue
getReservationProperty = do
    -- I think this is the connection order. The first player to connect
    -- gets number 0, and it goes up from there. The maximum is 7, which
    -- would be a full 4x4 game.
    number <- getInt7
    (systemId, remoteId, localId) <- getUniqueId
    playerName <- if systemId == 0 then return Nothing else do
        string <- getString
        return (Just string)
    -- No idea what these two flags are. Might be for bots?
    a <- getBool
    b <- getBool
    return (PReservation number systemId remoteId localId playerName a b)

getRigidBodyStateProperty :: Bits.BitGet PropValue
getRigidBodyStateProperty = do
    flag <- getBool
    position <- getVector
    rotation <- getFloatVector
    x <- if Type.unpackBoolean flag
        then return Nothing
        else fmap Just getVector
    y <- if Type.unpackBoolean flag
        then return Nothing
        else fmap Just getVector
    return (PRigidBodyState flag position rotation x y)

getStringProperty :: Bits.BitGet PropValue
getStringProperty = do
    string <- getString
    return (PString string)

getTeamPaintProperty :: Bits.BitGet PropValue
getTeamPaintProperty = do
    team <- getInt8
    primaryColor <- getInt8
    accentColor <- getInt8
    primaryFinish <- getInt32
    accentFinish <- getInt32
    return (PTeamPaint team primaryColor accentColor primaryFinish accentFinish)

getUniqueIdProperty :: Bits.BitGet PropValue
getUniqueIdProperty = do
    (systemId, remoteId, localId) <- getUniqueId
    return (PUniqueId systemId remoteId localId)

-- | Even though this is just a unique ID property, it must be handled
-- specially because it sometimes doesn't have the remote or local IDs.
getPartyLeaderProperty :: Bits.BitGet PropValue
getPartyLeaderProperty = do
    systemId <- getSystemId
    (remoteId, localId) <- if systemId == 0
        then return (SplitscreenId Nothing, Nothing)
        else do
            remoteId <- getRemoteId systemId
            localId <- getLocalId
            return (remoteId, localId)
    return (PUniqueId systemId remoteId localId)

--

getFloat32 :: Bits.BitGet Float
getFloat32 = do
    bytes <- Bits.getByteString 4
    bytes & byteStringToFloat & return

getString :: Bits.BitGet Text.Text
getString = do
    rawSize <- getInt32
    rawText <- if rawSize < 0
        then do
            let size = -2 * rawSize
            bytes <- Bits.getByteString size
            bytes & BS.map Utility.reverseBits & Encoding.decodeUtf16LE & return
        else do
            bytes <- Bits.getByteString rawSize
            bytes & BS.map Utility.reverseBits & Encoding.decodeLatin1 & return
    rawText & Text.dropEnd 1 & return

getUniqueId :: Bits.BitGet (SystemId, RemoteId, LocalId)
getUniqueId = do
    systemId <- getSystemId
    remoteId <- getRemoteId systemId
    localId <- getLocalId
    return (systemId, remoteId, localId)

getSystemId :: Bits.BitGet SystemId
getSystemId = do
    byte <- Bits.getWord8 8
    byte & Utility.reverseBits & return

getRemoteId :: SystemId -> Bits.BitGet RemoteId
getRemoteId systemId = case systemId of
    0 -> do
        remoteId <- Bits.getByteString 3
        if BS.all (\ byte -> byte == 0) remoteId
            then 0 & Just & SplitscreenId & return
            else error ("unexpected splitscreen id " ++ show remoteId)
    1 -> do
        bytes <- Bits.getByteString 8
        let remoteId = Binary.runGet
                Binary.getWord64le
                (bytes & BS.map Utility.reverseBits & BSL.fromStrict)
        remoteId & SteamId & return
    2 -> do
        bytes <- Bits.getByteString 32
        let remoteId = bytes
                & BS.map Utility.reverseBits
                & BS.unpack
                & concatMap (\ b -> Printf.printf "%02x" b)
                & Text.pack
        remoteId & PlayStationId & return
    4 -> do
        bytes <- Bits.getByteString 8
        let remoteId = Binary.runGet
                Binary.getWord64le
                (bytes & BS.map Utility.reverseBits & BSL.fromStrict)
        remoteId & XboxId & return
    _ -> error ("unknown system id " ++ show systemId)

getLocalId :: Bits.BitGet LocalId
getLocalId = do
    localId <- Bits.getWord8 8
    localId & Just & return

type SystemId = Word.Word8

-- This is the number associated with a splitscreen player. So the first player
-- is 0, the second is 1, and so on.
-- - 0 "Someone"
-- - 1 "Someone (1)"
type LocalId = Maybe Word.Word8

data RemoteId
    = SteamId !Word.Word64
    | PlayStationId !Text.Text
    | SplitscreenId !(Maybe Int)
    | XboxId !Word.Word64
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RemoteId
instance Aeson.ToJSON RemoteId

data Prop = Prop
    { propName :: !Text.Text
    , propValue :: !PropValue
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Prop
instance Aeson.ToJSON Prop where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Prop")

data PropValue
    = PBoolean !Type.Boolean
    | PByte !Word.Word8
    | PCamSettings !Float !Float !Float !Float !Float !Float
    | PDemolish !Type.Boolean !Int !Type.Boolean !Int !(Vector Int) !(Vector Int)
    | PEnum !Word.Word16 !Type.Boolean
    | PExplosion !Type.Boolean !(Maybe Int) !(Vector Int)
    | PFlaggedInt !Type.Boolean !Int
    | PFloat !Float
    | PGameMode !Word.Word8
    | PInt !Int
    | PLoadout !Int !Int !Int !Int !Int !Int !Int !Int !(Maybe Int)
    | PLoadoutOnline !Int !Int !Int !(Maybe Int)
    | PLocation !(Vector Int)
    | PMusicStinger !Type.Boolean !Int !Int
    | PPickup !Type.Boolean !(Maybe Int) !Type.Boolean
    | PPrivateMatchSettings !Text.Text !Int !Int !Text.Text !Text.Text !Type.Boolean
    | PQWord !Int !Int
    | PRelativeRotation !(Vector Float)
    | PReservation !Int !SystemId !RemoteId !LocalId !(Maybe Text.Text) !Type.Boolean !Type.Boolean
    | PRigidBodyState !Type.Boolean !(Vector Int) !(Vector Float) !(Maybe (Vector Int)) !(Maybe (Vector Int))
    | PString !Text.Text
    | PTeamPaint !Int !Int !Int !Int !Int
    | PUniqueId !SystemId !RemoteId !LocalId
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData PropValue
instance Aeson.ToJSON PropValue where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "PropValue")

-- | A frame in the net stream. Each frame has the time since the beginning of
-- the match, the time since the last frame, and a list of replications.
data Frame = Frame
    { frameNumber :: !Int
    , frameIsKeyFrame :: !Bool
    , frameTime :: !Float
    , frameDelta :: !Float
    , frameReplications :: ![Replication]
    } deriving (Eq,Generics.Generic,Show)

instance DeepSeq.NFData Frame
instance Aeson.ToJSON Frame where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Frame")

data ReplicationState
    = RSOpening
    | RSExisting
    | RSClosing
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ReplicationState
instance Aeson.ToJSON ReplicationState where
    toJSON rs = Aeson.toJSON (case rs of
        RSOpening -> "opening"
        RSExisting -> "existing"
        RSClosing -> "closing")

-- | Replication information about an actor in the net stream.
data Replication = Replication
    { replicationActorId :: !Int
    , replicationObjectName :: !Text.Text
    , replicationClassName :: !Text.Text
    , replicationState :: !ReplicationState
    , replicationInitialization :: !(Maybe ClassInit)
    , replicationProperties :: !(Map.Map Text.Text PropValue)
    } deriving (Eq,Generics.Generic,Show)

instance DeepSeq.NFData Replication
instance Aeson.ToJSON Replication where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Replication")

data Thing = Thing
    { thingFlag :: !Type.Boolean
    , thingObjectId :: !Int
    , thingObjectName :: !Text.Text
    , thingClassId :: !Int
    , thingClassName :: !Text.Text
    , thingClassInit :: !ClassInit
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Thing

type Time = Float

type Delta = Float

type ActorId = Int

data Vector a = Vector
    { vectorX :: !a
    , vectorY :: !a
    , vectorZ :: !a
    } deriving (Eq, Generics.Generic, Show)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Vector a)
instance (Aeson.ToJSON a) => Aeson.ToJSON (Vector a) where
    toJSON vector = Aeson.toJSON
        [ vectorX vector
        , vectorY vector
        , vectorZ vector
        ]

data ClassInit = ClassInit
    { classInitLocation :: !(Maybe (Vector Int))
    , classInitRotation :: !(Maybe (Vector Int))
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ClassInit
instance Aeson.ToJSON ClassInit where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "ClassInit")

-- { class stream id => { property stream id => name } }
type ClassPropertyMap = IntMap.IntMap (IntMap.IntMap Text.Text)

-- { stream id => object name }
type ObjectMap = IntMap.IntMap Text.Text

-- { class name => class id }
type ClassMap = Map.Map Text.Text Int

data Context = Context
    { contextObjectMap :: !ObjectMap
    , contextClassPropertyMap :: !ClassPropertyMap
    , contextThings :: !(IntMap.IntMap Thing)
    , contextClassMap :: !ClassMap
    , contextKeyFrames :: !(Set.Set Int)
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Context

extractContext :: Type.Replay -> Context
extractContext replay =
    Context
    { contextObjectMap = CPM.getPropertyMap replay
    , contextClassPropertyMap = CPM.getClassPropertyMap replay
    , contextThings = IntMap.empty
    , contextClassMap = CPM.getActorMap replay
    , contextKeyFrames = replay
        & Type.replayKeyFrames
        & Type.unpackList
        & map Type.keyFrameFrame
        & map Type.unpackInt32
        & map fromIntegral
        & Set.fromList
    }

byteStringToFloat :: BS.ByteString -> Float
byteStringToFloat bytes = Binary.runGet
    IEEE754.getFloat32le
    (bytes & BSL.fromStrict & Utility.reverseBitsInBytes)

getVector :: Bits.BitGet (Vector Int)
getVector = do
    numBits <- getNumVectorBits
    let bias = Bits.shiftL 1 (numBits + 1)
    let maxBits = numBits + 2
    let maxValue = 2 ^ maxBits
    dx <- getInt maxValue
    dy <- getInt maxValue
    dz <- getInt maxValue
    return
        Vector
        { vectorX = dx - bias
        , vectorY = dy - bias
        , vectorZ = dz - bias
        }

getVectorBytewise
    :: Bits.BitGet (Vector Int)
getVectorBytewise = do
    hasX <- getBool
    x <-
        if Type.unpackBoolean hasX
            then do
                word <- Bits.getWord8 8
                word & Utility.reverseBits & fromIntegral & return
            else return 0
    hasY <- getBool
    y <-
        if Type.unpackBoolean hasY
            then do
                word <- Bits.getWord8 8
                word & Utility.reverseBits & fromIntegral & return
            else return 0
    hasZ <- getBool
    z <-
        if Type.unpackBoolean hasZ
            then do
                word <- Bits.getWord8 8
                word & Utility.reverseBits & fromIntegral & return
            else return 0
    return
        Vector
        { vectorX = x
        , vectorY = y
        , vectorZ = z
        }

getFloatVector :: Bits.BitGet (Vector Float)
getFloatVector = do
    let maxValue = 1
    let numBits = 16
    x <- getFloat maxValue numBits
    y <- getFloat maxValue numBits
    z <- getFloat maxValue numBits
    return Vector { vectorX = x, vectorY = y, vectorZ = z }

getFloat :: Int -> Int -> Bits.BitGet Float
getFloat maxValue numBits = do
    let maxBitValue = (Bits.shiftL 1 (numBits - 1)) - 1
    let bias = Bits.shiftL 1 (numBits - 1)
    let serIntMax = Bits.shiftL 1 numBits
    delta <- getInt serIntMax
    let unscaledValue = delta - bias
    if maxValue > maxBitValue
    then do
        let invScale = fromIntegral maxValue / fromIntegral maxBitValue
        return (fromIntegral unscaledValue * invScale)
    else do
        let scale = fromIntegral maxBitValue / fromIntegral maxValue
        let invScale = 1.0 / scale
        return (fromIntegral unscaledValue * invScale)

getClassInit :: Text.Text -> Bits.BitGet ClassInit
getClassInit className = do
    location <-
        if Set.member className Data.locationClasses
            then do
                vector <- getVector
                return (Just vector)
            else return Nothing
    rotation <-
        if Set.member className Data.rotationClasses
            then do
                vector <- getVectorBytewise
                return (Just vector)
            else return Nothing
    return
        ClassInit
        { classInitLocation = location
        , classInitRotation = rotation
        }

bitSize
    :: (Integral a)
    => a -> a
bitSize x = x & fromIntegral & logBase (2 :: Double) & ceiling

-- Reads an integer bitwise. The bits of the integer are backwards, so the
-- least significant bit is first. The argument is the maximum value this
-- integer can have. Bits will be read until the next bit would be greater than
-- the maximum value, or the number of bits necessary to reach the maximum
-- value has been reached, whichever comes first.
--
-- For example, if the maximum value is 4 and "11" has been read already,
-- nothing more will be read because another "1" would put the value over the
-- maximum.
getInt
    :: Int -> Bits.BitGet Int
getInt maxValue = do
    let maxBits = bitSize maxValue
        go i value = do
            let x = Bits.shiftL 1 i
            if i < maxBits && value + x <= maxValue
                then do
                    bit <- getBool
                    let newValue =
                            if Type.unpackBoolean bit
                                then value + x
                                else value
                    go (i + 1) newValue
                else return value
    go 0 0

getInt32 :: Bits.BitGet Int
getInt32 = fmap (fromIntegral . Type.unpackInt32) (BinaryBit.getBits 32)

getInt8 :: Bits.BitGet Int
getInt8 = do
    byte <- Bits.getByteString 1
    let word = Binary.runGet
            Binary.getWord8
            (byte & BSL.fromStrict & Utility.reverseBitsInBytes)
    word & fromIntegral & (\ x -> x :: Int.Int8) & fromIntegral & return

getWord8 :: Bits.BitGet Word.Word8
getWord8 = do
    byte <- Bits.getByteString 1
    let word = Binary.runGet
            Binary.getWord8
            (byte & BSL.fromStrict & Utility.reverseBitsInBytes)
    return word

getActorId :: Bits.BitGet Int
getActorId = getInt 1024

getNumVectorBits :: Bits.BitGet Int
getNumVectorBits = getInt 19

getInt7 :: Bits.BitGet Int
getInt7 = getInt 7

getBool :: Bits.BitGet Type.Boolean
getBool = BinaryBit.getBits 1
