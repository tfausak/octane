module Octane.Parser where

import qualified Control.Newtype as Newtype
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Octane.Type as Type

parseFrames :: Type.Replay -> [Type.Frame]
parseFrames replay =
    let get = replay & extractContext & getFrames & Bits.runBitGet
        stream = replay & Type.replayStream & Newtype.unpack & BSL.fromStrict
    in Binary.runGet get stream

type ObjectMap = IntMap.IntMap Text.Text

buildObjectMap :: Type.Replay -> ObjectMap
buildObjectMap replay =
    replay & Type.replayObjects & Newtype.unpack & map Newtype.unpack &
    zip [0 ..] &
    IntMap.fromAscList

type ClassMap = IntMap.IntMap Text.Text

buildClassMap :: Type.Replay -> ClassMap
buildClassMap replay =
    replay & Type.replayActors & Newtype.unpack &
    map
        (\x ->
              ( x & Type.actorStreamId & Newtype.unpack & fromIntegral
              , x & Type.actorName & Newtype.unpack)) &
    IntMap.fromList

data CacheNode = CacheNode
    { cacheNodeClassId :: Int
    , cacheNodeParentCacheId :: Int
    , cacheNodeCacheId :: Int
    , cacheNodeProperties :: IntMap.IntMap Text.Text
    }

-- { class id => node }
type Cache = IntMap.IntMap CacheNode

buildCache :: Type.Replay -> Cache
buildCache replay =
    let objectMap = buildObjectMap replay
    in  replay
        & Type.replayCacheItems
        & Newtype.unpack
        & map (\ item -> CacheNode
            { cacheNodeClassId = item & Type.cacheItemClassId & Newtype.unpack & fromIntegral
            , cacheNodeParentCacheId = item & Type.cacheItemParentCacheId & Newtype.unpack & fromIntegral
            , cacheNodeCacheId = item & Type.cacheItemCacheId & Newtype.unpack & fromIntegral
            , cacheNodeProperties = item
                & Type.cacheItemCacheProperties
                & Newtype.unpack
                & map (\ property ->
                    ( property & Type.cachePropertyStreamId & Newtype.unpack & fromIntegral
                    , property
                        & Type.cachePropertyObjectId
                        & Newtype.unpack
                        & fromIntegral
                        & (\ objectId -> IntMap.lookup objectId objectMap)
                        & Maybe.fromJust
                    ))
                & IntMap.fromList
            })
        & map (\ node -> (cacheNodeClassId node, node))
        & IntMap.fromList

-- { class stream id => { property stream id => name } }
type ClassPropertyMap = IntMap.IntMap (IntMap.IntMap Text.Text)

getPropertyMap :: Cache -> Int -> IntMap.IntMap Text.Text
getPropertyMap cache cacheId =
    let cache' = cache & IntMap.toList & map snd & map (\ node -> (cacheNodeCacheId node, node)) & IntMap.fromList
    in  case IntMap.lookup cacheId cache' of
        Nothing -> IntMap.empty
        Just node -> if cacheNodeParentCacheId node == 0 || cacheNodeParentCacheId node == cacheId
            then cacheNodeProperties node
            else IntMap.union (cacheNodeProperties node) (getPropertyMap cache (cacheNodeParentCacheId node))

buildClassPropertyMap :: Type.Replay -> ClassPropertyMap
buildClassPropertyMap replay =
    let classMap = buildClassMap replay
        cache = buildCache replay
        f streamId _ m = case IntMap.lookup streamId cache of
            Nothing -> m
            Just node -> IntMap.insert streamId (getPropertyMap cache (cacheNodeCacheId node)) m
    in  IntMap.foldrWithKey f IntMap.empty classMap

getClass :: ObjectMap -> Int -> Maybe (Int, Text.Text)
getClass objectMap objectId =
    case IntMap.lookup objectId objectMap of
        Nothing -> Nothing
        Just name ->
            if name == Text.pack "TAGame.Default__PRI_TA" ||
               Text.isInfixOf (Text.pack "Archetype") name
                then getClass objectMap (objectId - 1)
                else Just (objectId, name)

data Context = Context
    { contextObjectMap :: ObjectMap
    , contextClassPropertyMap :: ClassPropertyMap
    }

extractContext :: Type.Replay -> Context
extractContext replay =
    Context
    { contextObjectMap = buildObjectMap replay
    , contextClassPropertyMap = buildClassPropertyMap replay
    }

getFrames :: Context -> Bits.BitGet [Type.Frame]
getFrames context = do
    maybeFrame <- getMaybeFrame context
    case maybeFrame of
        Nothing -> return []
        Just frame -> do
            frames <- getFrames context
            return (frame : frames)

getMaybeFrame :: Context -> Bits.BitGet (Maybe Type.Frame)
getMaybeFrame context = do
    -- TODO: Convert time bytes into a float.
    time <- Bits.getByteString 32
    -- TODO: Convert delta bytes into a float.
    delta <- Bits.getByteString 32
    if BS.all (== 0) time && BS.all (== 0) delta
        then return Nothing
        else do
            frame <- getFrame context time delta
            return (Just frame)

type Time = BS.ByteString

type Delta = BS.ByteString

getFrame :: Context -> Time -> Delta -> Bits.BitGet Type.Frame
getFrame context time delta = do
    replications <- getReplications context
    let frame =
            Type.Frame
            { Type.frameTime = time
            , Type.frameDelta = delta
            , Type.frameReplications = replications
            }
    return frame

getReplications :: Context -> Bits.BitGet [Type.Replication]
getReplications context = do
    (context',maybeReplication) <- getMaybeReplication context
    case maybeReplication of
        Nothing -> return []
        Just replication -> do
            replications <- getReplications context'
            return (replication : replications)

getMaybeReplication :: Context -> Bits.BitGet (Context, Maybe Type.Replication)
getMaybeReplication context = do
    hasReplication <- Bits.getBool
    if not hasReplication
        then return (context, Nothing)
        else do
            (newContext,replication) <- getReplication context
            return (newContext, Just replication)

getReplication :: Context -> Bits.BitGet (Context, Type.Replication)
getReplication context = do
    actorId <- getInt maxChannels
    isOpen <- Bits.getBool
    let go =
            if isOpen
                then getOpenReplication
                else getClosedReplication
    go context actorId

type ActorId = Int

getOpenReplication :: Context
                   -> ActorId
                   -> Bits.BitGet (Context, Type.Replication)
getOpenReplication context actorId = do
    isNew <- Bits.getBool
    let go =
            if isNew
                then getNewReplication
                else getExistingReplication
    go context actorId

getNewReplication :: Context
                  -> ActorId
                  -> Bits.BitGet (Context, Type.Replication)
getNewReplication context actorId = do
    _unknownFlag <- Bits.getBool
    objectId <- getInt 32
    let _objectName = context & contextObjectMap & IntMap.lookup objectId & Maybe.fromJust
    let (_classId, _className) = getClass (contextObjectMap context) objectId & Maybe.fromJust
    -- TODO: Parse class initialization.
    -- let classInit = getClassInit className
    -- https://github.com/rustyfausak/gizmo-elixir/blob/10452da/lib/gizmo/netstream/class_init.ex#L36
    -- TODO: Add all this to the context.
    -- { unknownFlag, objectId, objectName, classId, className, classInit }
    -- https://github.com/rustyfausak/gizmo-elixir/blob/10452da/lib/gizmo/netstream/replication.ex#L42
    return
        ( context
        , Type.Replication
          { Type.replicationActorId = actorId
          , Type.replicationIsOpen = True
          , Type.replicationIsNew = Just True
          })

getExistingReplication :: Context
                       -> ActorId
                       -> Bits.BitGet (Context, Type.Replication)
getExistingReplication context actorId = do
    let maybeClass = getClass (contextObjectMap context) actorId
    case maybeClass of
        Nothing ->
            fail ("TODO: Could not get class for object " ++ show actorId)
        Just (_classId,_className) ->
            -- TODO: Parse existing actor.
            return
                ( context
                , Type.Replication
                  { Type.replicationActorId = actorId
                  , Type.replicationIsOpen = True
                  , Type.replicationIsNew = Just True
                  })

getClosedReplication :: Context
                     -> ActorId
                     -> Bits.BitGet (Context, Type.Replication)
getClosedReplication context actorId = do
    return
        ( context
        , Type.Replication
          { Type.replicationActorId = actorId
          , Type.replicationIsOpen = False
          , Type.replicationIsNew = Nothing
          })

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
