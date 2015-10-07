{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Replay where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as ByteString
import Flow ((|>))
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
    replayCRC1 :: ByteString.ByteString,
    replayVersion1 :: Int32LE,
    replayVersion2 :: Int32LE,
    replayLabel :: PCString,
    replayProperties :: Table Property,
    replaySize2 :: Int32LE,
    replayCRC2 :: ByteString.ByteString,
    replayEffects :: List PCString,
    replayKeyFrames :: List KeyFrame,
    replayFrames :: ByteString.ByteString,
    replayMessages :: List Message,
    replayMarks :: List Mark,
    replayPackages :: List PCString,
    replayObjectMap :: ObjectMap,
    replayNames :: List PCString,
    replayActorMap :: ActorMap,
    replayCacheItems :: List CacheItem
} deriving (Show)

instance Binary.Binary Replay where
    get = do
        size1 <- Binary.get
        crc1 <- Binary.getByteString 4
        version1 <- Binary.get
        version2 <- Binary.get
        label <- Binary.get
        properties <- Binary.get
        size2 <- Binary.get
        crc2 <- Binary.getByteString 4
        effects <- Binary.get
        keyFrames <- Binary.get
        frames <- getFrames
        messages <- Binary.get
        marks <- Binary.get
        packages <- Binary.get
        objectMap <- Binary.get
        names <- Binary.get
        actorMap <- Binary.get
        cacheItems <- Binary.get
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
        replay |> replaySize1 |> Binary.put
        replay |> replayCRC1 |> Binary.putByteString
        replay |> replayVersion1 |> Binary.put
        replay |> replayVersion2 |> Binary.put
        replay |> replayLabel |> Binary.put
        replay |> replayProperties |> Binary.put
        replay |> replaySize2 |> Binary.put
        replay |> replayCRC2 |> Binary.putByteString
        replay |> replayEffects |> Binary.put
        replay |> replayKeyFrames |> Binary.put
        replay |> replayFrames |> putFrames
        replay |> replayMessages |> Binary.put
        replay |> replayMarks |> Binary.put
        replay |> replayPackages |> Binary.put
        replay |> replayObjectMap |> Binary.put
        replay |> replayNames |> Binary.put
        replay |> replayActorMap |> Binary.put
        replay |> replayCacheItems |> Binary.put

getFrames :: Binary.Get ByteString.ByteString
getFrames = do
    NewInt32LE size <- Binary.get
    frames <- Binary.getByteString size
    return frames

putFrames :: ByteString.ByteString -> Binary.Put
putFrames frames = do
    frames |> ByteString.length |> NewInt32LE |> Binary.put
    frames |> Binary.putByteString
