module Octane.Parser where

import qualified Control.Newtype as Newtype
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import qualified Octane.Type as Type

parseFrames :: Type.Replay -> [Type.Frame]
parseFrames replay =
    Binary.runGet
        (replay & extractContext & getFrames & Bits.runBitGet)
        (replay & Type.replayStream & Newtype.unpack & BSL.fromStrict)

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
              ( x & Type.actorTag & Newtype.unpack & fromIntegral
              , x & Type.actorName & Newtype.unpack)) &
    IntMap.fromList

type Cache = IntMap.IntMap Type.CacheItem

buildCache :: Type.Replay -> Cache
buildCache replay =
    replay & Type.replayCacheItems & Newtype.unpack &
    map
        (\x ->
              (x & Type.cacheItemTag & Newtype.unpack & fromIntegral, x)) &
    IntMap.fromList

-- TODO: This will need at least the actors and cache items.
data Context = Context
    {
    }

extractContext :: Type.Replay -> Context
extractContext _replay =
    Context
    {
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
    _objectId <- getInt 32
    -- TODO: Parse new actor.
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
    let maybeClassId = getClassId context actorId
    case maybeClassId of
        Nothing -> fail "TODO: Could not get class ID."
        Just _classId ->
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

type ClassId = ()

-- TODO: Actually implement this.
getClassId
    :: Context -> ActorId -> Maybe ClassId
getClassId _context _actorId = Nothing
