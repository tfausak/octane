module Octane.Types.Replay where

import qualified Data.ByteString as BS
import Octane.Core
import Octane.Types.ActorMap
import Octane.Types.CacheItem
import Octane.Types.Int32LE
import Octane.Types.KeyFrame
import Octane.Types.List
import Octane.Types.Mark
import Octane.Types.Message
import Octane.Types.ObjectMap
import Octane.Types.PCString
import Octane.Types.Property
import Octane.Types.Table

data Replay = NewReplay {
    replaySize1 :: Int32LE,
    replayCRC1 :: Int32LE,
    replayVersion1 :: Int32LE,
    replayVersion2 :: Int32LE,
    replayLabel :: PCString,
    replayProperties :: Table Property,
    replaySize2 :: Int32LE,
    replayCRC2 :: Int32LE,
    replayEffects :: List PCString,
    replayKeyFrames :: List KeyFrame,
    replayFrames :: ByteString,
    replayMessages :: List Message,
    replayMarks :: List Mark,
    replayPackages :: List PCString,
    replayObjectMap :: ObjectMap,
    replayNames :: List PCString,
    replayActorMap :: ActorMap,
    replayCacheItems :: List CacheItem
} deriving (Show)

instance Binary Replay where
    get = do
        size1 <- get
        crc1 <- get
        version1 <- get
        version2 <- get
        label <- get
        properties <- get
        size2 <- get
        crc2 <- get
        effects <- get
        keyFrames <- get
        frames <- getFrameBytes
        messages <- get
        marks <- get
        packages <- get
        objectMap <- get
        names <- get
        actorMap <- get
        cacheItems <- get
        return NewReplay {
            replaySize1 = size1,
            replayCRC1 = crc1,
            replayVersion1 = version1,
            replayVersion2 = version2,
            replayLabel = label,
            replayProperties = properties,
            replaySize2 = size2,
            replayCRC2 = crc2,
            replayEffects = effects,
            replayKeyFrames = keyFrames,
            replayFrames = frames,
            replayMessages = messages,
            replayMarks = marks,
            replayPackages = packages,
            replayObjectMap = objectMap,
            replayNames = names,
            replayActorMap = actorMap,
            replayCacheItems = cacheItems
        }

    put replay = do
        replay & replaySize1 & put
        replay & replayCRC1 & put
        replay & replayVersion1 & put
        replay & replayVersion2 & put
        replay & replayLabel & put
        replay & replayProperties & put
        replay & replaySize2 & put
        replay & replayCRC2 & put
        replay & replayEffects & put
        replay & replayKeyFrames & put
        replay & replayFrames & putFrameBytes
        replay & replayMessages & put
        replay & replayMarks & put
        replay & replayPackages & put
        replay & replayObjectMap & put
        replay & replayNames & put
        replay & replayActorMap & put
        replay & replayCacheItems & put

getFrameBytes :: Get ByteString
getFrameBytes = do
    NewInt32LE size <- get
    frames <- getByteString (fromIntegral size)
    return frames

putFrameBytes :: ByteString -> Put
putFrameBytes frames = do
    frames & BS.length & fromIntegral & NewInt32LE & put
    frames & putByteString
