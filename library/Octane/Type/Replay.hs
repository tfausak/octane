module Octane.Type.Replay (Replay(..)) where

import qualified Data.ByteString as BS
import Octane.Core
import Octane.Type.Actor
import Octane.Type.CacheItem
import Octane.Type.KeyFrame
import Octane.Type.Mark
import Octane.Type.Message
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Int32LE
import Octane.Type.Primitive.List
import Octane.Type.Property
import Octane.Type.Primitive.Dictionary

data Replay = NewReplay
    { replaySize1 :: Int32LE
    , replayCRC1 :: Int32LE
    , replayVersion1 :: Int32LE
    , replayVersion2 :: Int32LE
    , replayLabel :: PCString
    , replayProperties :: Dictionary Property
    , replaySize2 :: Int32LE
    , replayCRC2 :: Int32LE
    , replayEffects :: List PCString
    , replayKeyFrames :: List KeyFrame
    , replayFrames :: ByteString
    , replayMessages :: List Message
    , replayMarks :: List Mark
    , replayPackages :: List PCString
    , replayObjects :: List PCString
    , replayNames :: List PCString
    , replayActors :: List Actor
    , replayCacheItems :: List CacheItem
    } deriving (Eq, Show)

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
        objects <- get
        names <- get
        actors <- get
        cacheItems <- get
        return NewReplay
            { replaySize1 = size1
            , replayCRC1 = crc1
            , replayVersion1 = version1
            , replayVersion2 = version2
            , replayLabel = label
            , replayProperties = properties
            , replaySize2 = size2
            , replayCRC2 = crc2
            , replayEffects = effects
            , replayKeyFrames = keyFrames
            , replayFrames = frames
            , replayMessages = messages
            , replayMarks = marks
            , replayPackages = packages
            , replayObjects = objects
            , replayNames = names
            , replayActors = actors
            , replayCacheItems = cacheItems
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
        replay & replayObjects & put
        replay & replayNames & put
        replay & replayActors & put
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
