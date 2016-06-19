{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Octane.Utility.Parser where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as StrictBytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Data as Data
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Int8 as Int8
import qualified Octane.Type.KeyFrame as KeyFrame
import qualified Octane.Type.List as List
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
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


parseFrames :: ReplayWithoutFrames.ReplayWithoutFrames -> [Frame.Frame]
parseFrames replay = let
    numFrames = replay
        & ReplayWithoutFrames.properties
        & Dictionary.unpack
        & Map.lookup ("NumFrames" & StrictText.pack & Text.Text)
        & (\ property -> case property of
            Just (Property.IntProperty _ x) -> x & Int32.unpack & fromIntegral
            _ -> 0)
    get = replay & extractContext & getFrames 0 numFrames & Bits.runBitGet
    stream = replay & ReplayWithoutFrames.stream & Stream.unpack
    (_context, frames) = Binary.runGet get stream
    in frames

getFrames :: Word -> Int -> Context -> Bits.BitGet (Context, [Frame.Frame])
getFrames number numFrames context = do
    if fromIntegral number >= numFrames
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

getMaybeFrame :: Context -> Word -> Bits.BitGet (Maybe (Context, Frame.Frame))
getMaybeFrame context number = do
    time <- getFloat32
    delta <- getFloat32
    if time == 0 && delta == 0
    then return Nothing
    else if time < 0.001 || delta < 0.001
    then fail ("parsing previous frame probably failed. time: " ++ show time ++ ", delta: " ++ show delta)
    else do
        (newContext, frame) <- getFrame context number time delta
        return (Just (newContext, frame))

getFrame :: Context -> Word -> Float32.Float32 -> Float32.Float32 -> Bits.BitGet (Context, Frame.Frame)
getFrame context number time delta = do
    (newContext, replications) <- getReplications context
    let frame =
            Frame.Frame
            { Frame.number = number
            , Frame.isKeyFrame = context & contextKeyFrames & Set.member number
            , Frame.time = time
            , Frame.delta = delta
            , Frame.replications = replications
            }
    (newContext, frame) & DeepSeq.force & return

getReplications :: Context -> Bits.BitGet (Context, [Replication.Replication])
getReplications context = do
    maybeReplication <- getMaybeReplication context
    case maybeReplication of
        Nothing -> return (context, [])
        Just (newContext, replication) -> do
            (newerContext, replications) <- getReplications newContext
            return (newerContext, replication : replications)

getMaybeReplication :: Context -> Bits.BitGet (Maybe (Context, Replication.Replication))
getMaybeReplication context = do
    hasReplication <- getBool
    if Boolean.unpack hasReplication
        then do
            (newContext,replication) <- getReplication context
            return (Just (newContext, replication))
        else return Nothing

getReplication :: Context -> Bits.BitGet (Context, Replication.Replication)
getReplication context = do
    actorId <- getActorId
    isOpen <- getBool
    let go =
            if Boolean.unpack isOpen
                then getOpenReplication
                else getClosedReplication
    go context actorId

getOpenReplication :: Context
                   -> Int
                   -> Bits.BitGet (Context, Replication.Replication)
getOpenReplication context actorId = do
    isNew <- getBool
    let go =
            if Boolean.unpack isNew
                then getNewReplication
                else getExistingReplication
    go context actorId

getNewReplication :: Context
                  -> Int
                  -> Bits.BitGet (Context, Replication.Replication)
getNewReplication context actorId = do
    unknownFlag <- getBool
    if Boolean.unpack unknownFlag
        then fail "the unknown flag in a new replication is true! what does it mean?"
        else pure ()
    objectId <- getInt32
    objectName <- case context & contextObjectMap & IntMap.lookup (Int32.fromInt32 objectId) of
        Nothing -> fail ("could not find object name for id " ++ show objectId)
        Just x -> pure x
    (classId, className) <- case CPM.getClass (contextObjectMap context) Data.objectToClass (contextClassMap context) (Int32.fromInt32 objectId) of
        Nothing -> fail ("could not find class for object id " ++ show objectId)
        Just x -> pure x
    classInit <- getInitialization className
    let thing = Thing
            { thingFlag = unknownFlag
            , thingObjectId = objectId
            , thingObjectName = objectName
            , thingClassId = classId
            , thingClassName = className
            , thingInitialization = classInit
            }
    let things = contextThings context
    let newThings = IntMap.insert actorId thing things
    let newContext = context { contextThings = newThings }
    return
        ( newContext
        , Replication.Replication
          { Replication.actorId = fromIntegral actorId
          , Replication.objectName = objectName
          , Replication.className = className
          , Replication.state = State.SOpening
          , Replication.initialization = Just classInit
          , Replication.properties = Map.empty
          })

getExistingReplication :: Context
                       -> Int
                       -> Bits.BitGet (Context, Replication.Replication)
getExistingReplication context actorId = do
    thing <- case context & contextThings & IntMap.lookup actorId of
        Nothing -> fail ("could not find thing for actor id " ++ show actorId)
        Just x -> pure x
    props <- getProps context thing
    return (context, Replication.Replication
        { Replication.actorId = fromIntegral actorId
        , Replication.objectName = thingObjectName thing
        , Replication.className = thingClassName thing
        , Replication.state = State.SExisting
        , Replication.initialization = Nothing
        , Replication.properties = props
        })

getClosedReplication :: Context
                     -> Int
                     -> Bits.BitGet (Context, Replication.Replication)
getClosedReplication context actorId = do
    thing <- case context & contextThings & IntMap.lookup actorId of
        Nothing -> fail ("could not find thing for actor id " ++ show actorId)
        Just x -> pure x
    let newThings = context & contextThings & IntMap.delete actorId
    let newContext = context { contextThings = newThings }
    return
        ( newContext
        , Replication.Replication
          { Replication.actorId = fromIntegral actorId
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
        Nothing -> return Map.empty
        Just (k, v) -> do
            let m = Map.singleton k v
            props <- getProps context thing
            return (Map.union m props)

getMaybeProp :: Context -> Thing -> Bits.BitGet (Maybe (StrictText.Text, Value.Value))
getMaybeProp context thing = do
    hasProp <- getBool
    if Boolean.unpack hasProp
    then do
        prop <- getProp context thing
        return (Just prop)
    else return Nothing

getProp :: Context -> Thing -> Bits.BitGet (StrictText.Text, Value.Value)
getProp context thing = do
    let classId = thing & thingClassId
    props <- case context & contextClassPropertyMap & IntMap.lookup classId of
        Nothing -> fail ("could not find property map for class id " ++ show classId)
        Just x -> pure x
    let maxId = props & IntMap.keys & (0 :) & maximum
    pid <- getInt maxId
    name <- case props & IntMap.lookup pid of
        Nothing -> fail ("could not find property name for property id " ++ show pid)
        Just x -> pure x
    value <- getPropValue name
    return (name, value)

--

getPropValue :: StrictText.Text -> Bits.BitGet Value.Value
getPropValue name = case Map.lookup name propertyNameToGet of
    Nothing -> fail ("don't know how to read property " ++ show name)
    Just get -> get

propertyNameToGet :: Map.Map StrictText.Text (Bits.BitGet Value.Value)
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
    , (Set.fromList [StrictText.pack "TAGame.PRI_TA:PartyLeader"], getPartyLeaderProperty)
    ]
        & concatMap (\ (ks, v) -> ks & Set.toList & map (\ k -> (k, v)))
        & Map.fromList

getBooleanProperty :: Bits.BitGet Value.Value
getBooleanProperty = do
    bool <- getBool
    return (Value.VBoolean bool)

getByteProperty :: Bits.BitGet Value.Value
getByteProperty = do
    word <- getWord8
    return (Value.VByte word)

getCamSettingsProperty :: Bits.BitGet Value.Value
getCamSettingsProperty = do
    fov <- getFloat32
    height <- getFloat32
    angle <- getFloat32
    distance <- getFloat32
    stiffness <- getFloat32
    swivelSpeed <- getFloat32
    return (Value.VCamSettings fov height angle distance stiffness swivelSpeed)

getDemolishProperty :: Bits.BitGet Value.Value
getDemolishProperty = do
    atkFlag <- getBool
    atk <- getWord32
    vicFlag <- getBool
    vic <- getWord32
    vec1 <- getVector
    vec2 <- getVector
    return (Value.VDemolish atkFlag atk vicFlag vic vec1 vec2)

getEnumProperty :: Bits.BitGet Value.Value
getEnumProperty = do
    x <- Bits.getWord16be 10
    y <- if x == 1023
        then getBool
        else fail ("unexpected enum value " ++ show x)
    return (Value.VEnum (Word16.toWord16 x) y)

getExplosionProperty :: Bits.BitGet Value.Value
getExplosionProperty = do
    noGoal <- getBool
    a <- if Boolean.unpack noGoal
        then return Nothing
        else fmap Just getInt32
    b <- getVector
    return (Value.VExplosion noGoal a b)

getFlaggedIntProperty :: Bits.BitGet Value.Value
getFlaggedIntProperty = do
    flag <- getBool
    int <- getInt32
    return (Value.VFlaggedInt flag int)

getFloatProperty :: Bits.BitGet Value.Value
getFloatProperty = do
    float <- getFloat32
    return (Value.VFloat float)

getGameModeProperty :: Bits.BitGet Value.Value
getGameModeProperty = do
    x <- Bits.getWord8 2
    return (Value.VGameMode (Word8.toWord8 x))

getIntProperty :: Bits.BitGet Value.Value
getIntProperty = do
    int <- getInt32
    return (Value.VInt int)

getLoadoutOnlineProperty :: Bits.BitGet Value.Value
getLoadoutOnlineProperty = do
    version <- getWord32
    x <- getWord32
    y <- getWord32
    z <- if version >= 12
        then do
            value <- getWord8
            return (Just value)
        else return Nothing
    return (Value.VLoadoutOnline version x y z)

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
    h <- if version > 10
        then do
            value <- getWord32
            return (Just value)
        else return Nothing
    return (Value.VLoadout version body decal wheels rocketTrail antenna topper g h)

getLocationProperty :: Bits.BitGet Value.Value
getLocationProperty = do
    vector <- getVector
    return (Value.VLocation vector)

getMusicStingerProperty :: Bits.BitGet Value.Value
getMusicStingerProperty = do
    flag <- getBool
    cue <- getWord32
    trigger <- getWord8
    return (Value.VMusicStinger flag cue trigger)

getPickupProperty :: Bits.BitGet Value.Value
getPickupProperty = do
    instigator <- getBool
    instigatorId <- if Boolean.unpack instigator
        then fmap Just getWord32
        else return Nothing
    pickedUp <- getBool
    return (Value.VPickup instigator instigatorId pickedUp)

getPrivateMatchSettingsProperty :: Bits.BitGet Value.Value
getPrivateMatchSettingsProperty = do
    mutators <- getText
    joinableBy <- getWord32
    maxPlayers <- getWord32
    gameName <- getText
    password <- getText
    flag <- getBool
    return (Value.VPrivateMatchSettings mutators joinableBy maxPlayers gameName password flag)

getQWordProperty :: Bits.BitGet Value.Value
getQWordProperty = do
    qword <- getWord64
    return (Value.VQWord qword)

getRelativeRotationProperty :: Bits.BitGet Value.Value
getRelativeRotationProperty = do
    vector <- getFloatVector
    return (Value.VRelativeRotation vector)

getReservationProperty :: Bits.BitGet Value.Value
getReservationProperty = do
    -- I think this is the connection order. The first player to connect
    -- gets number 0, and it goes up from there. The maximum is 7, which
    -- would be a full 4x4 game.
    number <- getInt7
    (systemId, remoteId, localId) <- getUniqueId
    playerName <- if systemId == 0 then return Nothing else do
        string <- getText
        return (Just string)
    -- No idea what these two flags are. Might be for bots?
    a <- getBool
    b <- getBool
    return (Value.VReservation number systemId remoteId localId playerName a b)

getRigidBodyStateProperty :: Bits.BitGet Value.Value
getRigidBodyStateProperty = do
    flag <- getBool
    position <- getVector
    rotation <- getFloatVector
    x <- if Boolean.unpack flag
        then return Nothing
        else fmap Just getVector
    y <- if Boolean.unpack flag
        then return Nothing
        else fmap Just getVector
    return (Value.VRigidBodyState flag position rotation x y)

getStringProperty :: Bits.BitGet Value.Value
getStringProperty = do
    string <- getText
    return (Value.VString string)

getTeamPaintProperty :: Bits.BitGet Value.Value
getTeamPaintProperty = do
    team <- getWord8
    primaryColor <- getWord8
    accentColor <- getWord8
    primaryFinish <- getWord32
    accentFinish <- getWord32
    return (Value.VTeamPaint team primaryColor accentColor primaryFinish accentFinish)

getUniqueIdProperty :: Bits.BitGet Value.Value
getUniqueIdProperty = do
    (systemId, remoteId, localId) <- getUniqueId
    return (Value.VUniqueId systemId remoteId localId)

-- | Even though this is just a unique ID property, it must be handled
-- specially because it sometimes doesn't have the remote or local IDs.
getPartyLeaderProperty :: Bits.BitGet Value.Value
getPartyLeaderProperty = do
    systemId <- getSystemId
    (remoteId, localId) <- if systemId == 0
        then return (RemoteId.SplitscreenId Nothing, Nothing)
        else do
            remoteId <- getRemoteId systemId
            localId <- getLocalId
            return (remoteId, localId)
    return (Value.VUniqueId systemId remoteId localId)

--

getFloat32 :: Bits.BitGet Float32.Float32
getFloat32 = BinaryBit.getBits unimportant

getText :: Bits.BitGet Text.Text
getText = BinaryBit.getBits unimportant

getUniqueId :: Bits.BitGet (Word8.Word8, RemoteId.RemoteId, Maybe Word8.Word8)
getUniqueId = do
    systemId <- getSystemId
    remoteId <- getRemoteId systemId
    localId <- getLocalId
    return (systemId, remoteId, localId)

getSystemId :: Bits.BitGet Word8.Word8
getSystemId = getWord8

getRemoteId :: Word8.Word8 -> Bits.BitGet RemoteId.RemoteId
getRemoteId systemId = case systemId of
    0 -> do
        remoteId <- Bits.getByteString 3
        if StrictBytes.all (\ byte -> byte == 0) remoteId
            then 0 & Just & RemoteId.SplitscreenId & return
            else fail ("unexpected splitscreen id " ++ show remoteId)
    1 -> do
        bytes <- Bits.getByteString 8
        let remoteId = Binary.runGet
                Binary.getWord64le
                (bytes & LazyBytes.fromStrict & Endian.reverseBitsInBytes)
        remoteId & Word64.toWord64 & RemoteId.SteamId & return
    2 -> do
        bytes <- Bits.getByteString 32
        let remoteId = bytes
                & LazyBytes.fromStrict
                & Endian.reverseBitsInBytes
                & LazyBytes.unpack
                & concatMap (\ b -> Printf.printf "%02x" b)
                & StrictText.pack
                & Text.Text
        remoteId & RemoteId.PlayStationId & return
    4 -> do
        bytes <- Bits.getByteString 8
        let remoteId = Binary.runGet
                Binary.getWord64le
                (bytes & LazyBytes.fromStrict & Endian.reverseBitsInBytes)
        remoteId & Word64.toWord64 & RemoteId.XboxId & return
    _ -> fail ("unknown system id " ++ show systemId)

getLocalId :: Bits.BitGet (Maybe Word8.Word8)
getLocalId = fmap Just getWord8


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
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Context

extractContext :: ReplayWithoutFrames.ReplayWithoutFrames -> Context
extractContext replay =
    Context
    { contextObjectMap = CPM.getPropertyMap replay
    , contextClassPropertyMap = CPM.getClassPropertyMap replay
    , contextThings = IntMap.empty
    , contextClassMap = CPM.getActorMap replay
    , contextKeyFrames = replay
        & ReplayWithoutFrames.keyFrames
        & List.unpack
        & map KeyFrame.frame
        & map Word32.fromWord32
        & Set.fromList
    }

getVector :: Bits.BitGet (Vector.Vector Int)
getVector = do
    numBits <- getNumVectorBits
    let bias = Bits.shiftL 1 (numBits + 1)
    let maxBits = numBits + 2
    let maxValue = 2 ^ maxBits
    dx <- getInt maxValue
    dy <- getInt maxValue
    dz <- getInt maxValue
    return
        Vector.Vector
        { Vector.x = dx - bias
        , Vector.y = dy - bias
        , Vector.z = dz - bias
        }

getVectorBytewise
    :: Bits.BitGet (Vector.Vector Int8.Int8)
getVectorBytewise = do
    hasX <- getBool
    x <- if Boolean.unpack hasX then getInt8 else return 0
    hasY <- getBool
    y <- if Boolean.unpack hasY then getInt8 else return 0
    hasZ <- getBool
    z <- if Boolean.unpack hasZ then getInt8 else return 0
    return
        Vector.Vector
        { Vector.x = x
        , Vector.y = y
        , Vector.z = z
        }

getFloatVector :: Bits.BitGet (Vector.Vector Float)
getFloatVector = do
    let maxValue = 1
    let numBits = 16
    x <- getFloat maxValue numBits
    y <- getFloat maxValue numBits
    z <- getFloat maxValue numBits
    return Vector.Vector { Vector.x = x, Vector.y = y, Vector.z = z }

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

getInitialization :: StrictText.Text -> Bits.BitGet Initialization.Initialization
getInitialization className = do
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
        Initialization.Initialization
        { Initialization.location = location
        , Initialization.rotation = rotation
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
                            if Boolean.unpack bit
                                then value + x
                                else value
                    go (i + 1) newValue
                else return value
    go 0 0

getInt32 :: Bits.BitGet Int32.Int32
getInt32 = BinaryBit.getBits unimportant

getInt8 :: Bits.BitGet Int8.Int8
getInt8 = BinaryBit.getBits unimportant

getWord64 :: Bits.BitGet Word64.Word64
getWord64 = BinaryBit.getBits unimportant

getWord32 :: Bits.BitGet Word32.Word32
getWord32 = BinaryBit.getBits unimportant

getWord8 :: Bits.BitGet Word8.Word8
getWord8 = BinaryBit.getBits unimportant

getActorId :: Bits.BitGet Int
getActorId = getInt 1024

getNumVectorBits :: Bits.BitGet Int
getNumVectorBits = getInt 19

getInt7 :: Bits.BitGet Int
getInt7 = getInt 7

getBool :: Bits.BitGet Boolean.Boolean
getBool = BinaryBit.getBits unimportant

-- | The 'getBits' function from "Data.Binary.Bits" requires a size parameter.
-- None of Octane's instances use it.
unimportant :: Int
unimportant = 0