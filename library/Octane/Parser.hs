{-# LANGUAGE DeriveGeneric #-}

module Octane.Parser where

import qualified Control.Newtype as Newtype
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Debug.Trace as Trace
import qualified GHC.Generics as Generics
import qualified Octane.Type as Type
import qualified Text.Printf as Printf

parseFrames :: Type.Replay -> [Frame]
parseFrames replay = do
    let get = replay & extractContext & getFrames & Bits.runBitGet
        stream = replay & Type.replayStream & Newtype.unpack & BSL.fromStrict
    Binary.runGet get stream

getFrames :: Context -> Bits.BitGet [Frame]
getFrames context = do
    maybeFrame <- getMaybeFrame context
    case maybeFrame of
        Nothing -> return []
        Just frame -> do
            frames <- getFrames context
            return (frame : frames)

getMaybeFrame :: Context -> Bits.BitGet (Maybe Frame)
getMaybeFrame context = do
    timeBytes <- Bits.getByteString 4
    let time = byteStringToFloat timeBytes
    deltaBytes <- Bits.getByteString 4
    let delta = byteStringToFloat deltaBytes
    if BS.all (== 0) timeBytes && BS.all (== 0) deltaBytes
        then return Nothing
        else do
            frame <- getFrame context time delta
            return (Just frame)

getFrame :: Context -> Time -> Delta -> Bits.BitGet Frame
getFrame context time delta = do
    replications <- getReplications context
    let frame =
            Frame
            { frameTime = time
            , frameDelta = delta
            , frameReplications = replications
            }
    return frame

getReplications :: Context -> Bits.BitGet [Replication]
getReplications context = do
    (context',maybeReplication) <- getMaybeReplication context
    case maybeReplication of
        Nothing -> return []
        Just replication -> do
            replications <- getReplications context'
            return (replication : replications)

getMaybeReplication :: Context -> Bits.BitGet (Context, Maybe Replication)
getMaybeReplication context = do
    hasReplication <- Bits.getBool
    if not hasReplication
        then return (context, Nothing)
        else do
            (newContext,replication) <- getReplication context
            return (newContext, Just replication)

getReplication :: Context -> Bits.BitGet (Context, Replication)
getReplication context = do
    actorId <- getInt maxChannels
    isOpen <- Bits.getBool
    let go =
            if isOpen
                then getOpenReplication
                else getClosedReplication
    go context actorId

getOpenReplication :: Context
                   -> ActorId
                   -> Bits.BitGet (Context, Replication)
getOpenReplication context actorId = do
    isNew <- Bits.getBool
    let go =
            if isNew
                then getNewReplication
                else getExistingReplication
    go context actorId

getNewReplication :: Context
                  -> ActorId
                  -> Bits.BitGet (Context, Replication)
getNewReplication context actorId = do
    unknownFlag <- Bits.getBool
    objectId <- getInt (2 ^ (32 :: Int))
    let objectName = case context & contextObjectMap & IntMap.lookup objectId of
            Nothing -> error ("could not find object name for id " ++ show objectId)
            Just x -> x
    let (classId,className) = case getClass (contextObjectMap context) objectId of
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
          , replicationIsOpen = True
          , replicationIsNew = Just True
          , replicationProps = []
          })

getExistingReplication :: Context
                       -> ActorId
                       -> Bits.BitGet (Context, Replication)
getExistingReplication context actorId = do
    Trace.traceM ("Getting existing replication for " ++ show actorId)
    let thing = case context & contextThings & IntMap.lookup actorId of
            Nothing -> error ("could not find thing for actor id " ++ show actorId)
            Just x -> x
    Trace.traceM ("Getting props for " ++ show thing)
    props <- getProps context thing
    Trace.traceM ("Got props " ++ show props)
    return (context, Replication
        { replicationActorId = actorId
        , replicationIsOpen = True
        , replicationIsNew = Just False
        , replicationProps = props
        })

getClosedReplication :: Context
                     -> ActorId
                     -> Bits.BitGet (Context, Replication)
getClosedReplication context actorId = do
    let newThings = context & contextThings & IntMap.delete actorId
    let newContext = context { contextThings = newThings }
    return
        ( newContext
        , Replication
          { replicationActorId = actorId
          , replicationIsOpen = False
          , replicationIsNew = Nothing
          , replicationProps = []
          })

getProps :: Context -> Thing -> Bits.BitGet [Prop]
getProps context thing = do
    maybeProp <- getMaybeProp context thing
    case maybeProp of
        Nothing -> return []
        Just prop -> do
            props <- getProps context thing
            return (prop : props)

getMaybeProp :: Context -> Thing -> Bits.BitGet (Maybe Prop)
getMaybeProp context thing = do
    hasProp <- Bits.getBool
    if hasProp
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
    let maxId = props & IntMap.keys & maximum
    Trace.traceM ("Max ID: " ++ show maxId)
    pid <- getInt maxId
    Trace.traceM ("Prop ID: " ++ show pid)
    let propName = case props & IntMap.lookup pid of
            Nothing -> error ("could not find property name for property id " ++ show pid)
            Just x -> x
    Trace.traceM ("Prop name: " ++ show propName)
    value <- getPropValue propName
    Trace.traceM ("Prop value: " ++ show value)
    return (Prop { propId = pid, propValue = value })

getPropValue :: Text.Text -> Bits.BitGet PropValue
getPropValue name = case Text.unpack name of
    _ | Set.member name propsWithRigidBodyState -> do
        flag <- Bits.getBool
        position <- getVector
        rotation <- getFloatVector
        x <- if flag then return Nothing else fmap Just getVector
        y <- if flag then return Nothing else fmap Just getVector
        return (RigidBodyState flag position rotation x y)
    _ | Set.member name propsWithFlaggedInt -> do
        flag <- Bits.getBool
        int <- getInt (2 ^ (32 :: Int))
        return (FlaggedInt flag (fromIntegral int))
    _ | Set.member name propsWithString -> do
        -- TODO: This has a lot of overlap with PCString.
        rawSize <- getInt (2 ^ (32 :: Int))
        rawText <- if rawSize < 0
            then do
                let size = -2 * rawSize
                bytes <- Bits.getByteString size
                bytes & BS.map Type.reverseBits & Encoding.decodeUtf16LE & return
            else do
                bytes <- Bits.getByteString rawSize
                bytes & BS.map Type.reverseBits & Encoding.decodeLatin1 & return
        let text = rawText & Text.dropEnd 1
        return (String text)
    -- TODO: Parse other prop types.
    _ -> fail ("don't know how to read property " ++ show name)

propsWithRigidBodyState :: Set.Set Text.Text
propsWithRigidBodyState =
    [ "TAGame.RBActor_TA:ReplicatedRBState"
    ] & map Text.pack & Set.fromList

propsWithFlaggedInt :: Set.Set Text.Text
propsWithFlaggedInt =
    [ "Engine.GameReplicationInfo:GameClass"
    , "TAGame.Ball_TA:GameEvent"
    , "TAGame.Team_TA:GameEvent"
    ] & map Text.pack & Set.fromList

propsWithString :: Set.Set Text.Text
propsWithString =
    [ "Engine.GameReplicationInfo:ServerName"
    ] & map Text.pack & Set.fromList

data Prop = Prop
    { propId :: Int
    , propValue :: PropValue
    } deriving (Eq, Show)

data PropValue
    = RigidBodyState Bool (Vector Int) (Vector Float) (Maybe (Vector Int)) (Maybe (Vector Int))
    | FlaggedInt Bool Int
    | String Text.Text
    deriving (Eq, Show)

-- | A frame in the net stream. Each frame has the time since the beginning of
-- | the match, the time since the last frame, and a list of replications.
data Frame = Frame
    { frameTime :: Float
    , frameDelta :: Float
    , frameReplications :: [Replication]
    } deriving (Eq,Generics.Generic,Show)

-- | Replication information about an actor in the net stream.
data Replication = Replication
    { replicationActorId :: Int
    , replicationIsOpen :: Bool
    , replicationIsNew :: Maybe Bool
    , replicationProps :: [Prop]
    } deriving (Eq,Generics.Generic,Show)

data Thing = Thing
    { thingFlag :: Bool
    , thingObjectId :: Int
    , thingObjectName :: Text.Text
    , thingClassId :: Int
    , thingClassName :: Text.Text
    , thingClassInit :: ClassInit
    } deriving (Show)

type Time = Float

type Delta = Float

type ActorId = Int

data Vector a = Vector
    { vectorX :: a
    , vectorY :: a
    , vectorZ :: a
    } deriving (Eq, Show)

data ClassInit = ClassInit
    { classInitLocation :: Maybe (Vector Int)
    , classInitRotation :: Maybe (Vector Int)
    } deriving (Show)

data CacheNode = CacheNode
    { cacheNodeClassId :: Int
    , cacheNodeParentCacheId :: Int
    , cacheNodeCacheId :: Int
    , cacheNodeProperties :: IntMap.IntMap Text.Text
    } deriving (Show)

-- { class id => node }
type Cache = IntMap.IntMap CacheNode

-- { class stream id => { property stream id => name } }
type ClassPropertyMap = IntMap.IntMap (IntMap.IntMap Text.Text)

-- { stream id => object name }
type ObjectMap = IntMap.IntMap Text.Text

data Context = Context
    { contextObjectMap :: ObjectMap
    , contextClassPropertyMap :: ClassPropertyMap
    , contextThings :: IntMap.IntMap Thing
    } deriving (Show)

showAsHex :: BS.ByteString -> String
showAsHex bytes
    = bytes
    & BS.unpack
    & concatMap (\ byte -> Printf.printf "%02x" byte)

buildObjectMap :: Type.Replay -> ObjectMap
buildObjectMap replay =
    replay & Type.replayObjects & Newtype.unpack & map Newtype.unpack &
    zip [0 ..] &
    IntMap.fromAscList

-- { stream id => class name }
type ClassMap = IntMap.IntMap Text.Text

buildClassMap :: Type.Replay -> ClassMap
buildClassMap replay =
    replay & Type.replayActors & Newtype.unpack &
    map
        (\x ->
              ( x & Type.actorStreamId & Newtype.unpack & fromIntegral
              , x & Type.actorName & Newtype.unpack)) &
    IntMap.fromList

buildCache :: Type.Replay -> Cache
buildCache replay =
    let objectMap = buildObjectMap replay
    in replay & Type.replayCacheItems & Newtype.unpack &
       map
           (\item ->
                 CacheNode
                 { cacheNodeClassId = item & Type.cacheItemClassId &
                   Newtype.unpack &
                   fromIntegral
                 , cacheNodeParentCacheId = item & Type.cacheItemParentCacheId &
                   Newtype.unpack &
                   fromIntegral
                 , cacheNodeCacheId = item & Type.cacheItemCacheId &
                   Newtype.unpack &
                   fromIntegral
                 , cacheNodeProperties = item & Type.cacheItemCacheProperties &
                   Newtype.unpack &
                   map
                       (\property ->
                             ( property & Type.cachePropertyStreamId &
                               Newtype.unpack &
                               fromIntegral
                             , case property & Type.cachePropertyObjectId & Newtype.unpack & fromIntegral & flip IntMap.lookup objectMap of
                                Nothing -> error ("could not find object for property " ++ show property)
                                Just x -> x)) &
                   IntMap.fromList
                 }) &
       map
           (\node ->
                 (cacheNodeClassId node, node)) &
       IntMap.fromList

getPropertyMap :: Cache -> Int -> IntMap.IntMap Text.Text
getPropertyMap cache cacheId =
    case IntMap.lookup cacheId cache of
        Nothing -> IntMap.empty
        Just node ->
            if cacheNodeParentCacheId node == 0 ||
               cacheNodeParentCacheId node == cacheId
                then cacheNodeProperties node
                else IntMap.union
                         (cacheNodeProperties node)
                         (getPropertyMap cache (cacheNodeParentCacheId node))

buildClassPropertyMap :: Type.Replay -> ClassPropertyMap
buildClassPropertyMap replay =
    let classMap = buildClassMap replay
        cacheByStreamId = buildCache replay
        cacheByCacheId =
            cacheByStreamId & IntMap.toDescList & map snd &
            map
                (\node ->
                      (cacheNodeCacheId node, node)) &
            IntMap.fromList
        f streamId _ m =
            case IntMap.lookup streamId cacheByStreamId of
                Nothing -> m
                Just node ->
                    IntMap.insert
                        streamId
                        (getPropertyMap cacheByCacheId (cacheNodeCacheId node))
                        m
    in IntMap.foldrWithKey f IntMap.empty classMap

getClass :: ObjectMap -> Int -> Maybe (Int, Text.Text)
getClass objectMap objectId =
    case IntMap.lookup objectId objectMap of
        Nothing -> Nothing
        Just name ->
            if name == Text.pack "TAGame.Default__PRI_TA" ||
               Text.isInfixOf (Text.pack "Archetype") name
                then getClass objectMap (objectId - 1)
                else Just (objectId, name)

extractContext :: Type.Replay -> Context
extractContext replay =
    Context
    { contextObjectMap = buildObjectMap replay
    , contextClassPropertyMap = buildClassPropertyMap replay
    , contextThings = IntMap.empty
    }

classesWithLocation :: Set.Set Text.Text
classesWithLocation =
    [ "Engine.GameReplicationInfo"
    , "TAGame.Ball_TA"
    , "TAGame.CarComponent_Boost_TA"
    , "TAGame.CarComponent_Dodge_TA"
    , "TAGame.CarComponent_DoubleJump_TA"
    , "TAGame.CarComponent_FlipCar_TA"
    , "TAGame.CarComponent_Jump_TA"
    , "TAGame.Car_TA"
    , "TAGame.Default__CameraSettingsActor_TA"
    , "TAGame.Default__PRI_TA"
    , "TAGame.GRI_TA"
    , "TAGame.GameEvent_Season_TA"
    , "TAGame.GameEvent_SoccarPrivate_TA"
    , "TAGame.GameEvent_SoccarSplitscreen_TA"
    , "TAGame.GameEvent_Soccar_TA"
    , "TAGame.PRI_TA"
    , "TAGame.Team_Soccar_TA"
    , "TAGame.Team_TA"] &
    map Text.pack &
    Set.fromList

classesWithRotation :: Set.Set Text.Text
classesWithRotation =
    ["TAGame.Ball_TA", "TAGame.Car_Season_TA", "TAGame.Car_TA"] & map Text.pack &
    Set.fromList

maxVectorValue :: Int
maxVectorValue = 19

byteStringToFloat :: BS.ByteString -> Float
byteStringToFloat bytes = Binary.runGet
    IEEE754.getFloat32le
    (bytes & BSL.fromStrict & BSL.map Type.reverseBits)

getVector :: Bits.BitGet (Vector Int)
getVector = do
    numBits <- getInt maxVectorValue
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
    hasX <- Bits.getBool
    x <-
        if hasX
            then do
                word <- Bits.getWord8 8
                word & Type.reverseBits & fromIntegral & return
            else return 0
    hasY <- Bits.getBool
    y <-
        if hasY
            then do
                word <- Bits.getWord8 8
                word & Type.reverseBits & fromIntegral & return
            else return 0
    hasZ <- Bits.getBool
    z <-
        if hasZ
            then do
                word <- Bits.getWord8 8
                word & Type.reverseBits & fromIntegral & return
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
        if Set.member className classesWithLocation
            then do
                vector <- getVector
                return (Just vector)
            else return Nothing
    rotation <-
        if Set.member className classesWithRotation
            then do
                vector <- getVectorBytewise
                return (Just vector)
            else return Nothing
    return
        ClassInit
        { classInitLocation = location
        , classInitRotation = rotation
        }

maxChannels
    :: (Integral a)
    => a
maxChannels = 1024

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
                    bit <- Bits.getBool
                    let newValue =
                            if bit
                                then value + x
                                else value
                    go (i + 1) newValue
                else return value
    go 0 0
