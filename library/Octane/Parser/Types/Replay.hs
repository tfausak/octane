{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.Replay where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import Flow ((|>))
import Octane.Parser.Types.ActorMap
import Octane.Parser.Types.CacheItem
import Octane.Parser.Types.Int32LE
import Octane.Parser.Types.KeyFrame
import Octane.Parser.Types.List
import Octane.Parser.Types.Mark
import Octane.Parser.Types.Message
import Octane.Parser.Types.ObjectMap
import Octane.Parser.Types.PCString
import Octane.Parser.Types.Property
import Octane.Parser.Types.Table

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
    replayFrames :: BS.ByteString,
    replayMessages :: List Message,
    replayMarks :: List Mark,
    replayPackages :: List PCString,
    replayObjectMap :: ObjectMap,
    replayNames :: List PCString,
    replayActorMap :: ActorMap,
    replayCacheItems :: List CacheItem
} deriving (Show)

instance Aeson.ToJSON Replay where
    toJSON replay = Aeson.object [
        "size-1" .= replaySize1 replay,
        "crc-1" .= replayCRC1 replay,
        "version-1" .= replayVersion1 replay,
        "version-2" .= replayVersion2 replay,
        "label" .= replayLabel replay,
        "properties" .= replayProperties replay,
        "size-2" .= replaySize2 replay,
        "crc-2" .= replayCRC2 replay,
        "effects" .= replayEffects replay,
        "key-frames" .= replayKeyFrames replay,
        -- TODO: frames,
        "messages" .= replayMessages replay,
        "marks" .= replayMarks replay,
        "packages" .= replayPackages replay,
        "objects" .= replayObjectMap replay,
        "names" .= replayNames replay,
        "actors" .= replayActorMap replay,
        "cache" .= replayCacheItems replay
        ]

instance Binary.Binary Replay where
    get = do
        size1 <- Binary.get
        crc1 <- Binary.get
        version1 <- Binary.get
        version2 <- Binary.get
        label <- Binary.get
        properties <- Binary.get
        size2 <- Binary.get
        crc2 <- Binary.get
        effects <- Binary.get
        keyFrames <- Binary.get
        frames <- getFrameBytes
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
        replay |> replayCRC1 |> Binary.put
        replay |> replayVersion1 |> Binary.put
        replay |> replayVersion2 |> Binary.put
        replay |> replayLabel |> Binary.put
        replay |> replayProperties |> Binary.put
        replay |> replaySize2 |> Binary.put
        replay |> replayCRC2 |> Binary.put
        replay |> replayEffects |> Binary.put
        replay |> replayKeyFrames |> Binary.put
        replay |> replayFrames |> putFrameBytes
        replay |> replayMessages |> Binary.put
        replay |> replayMarks |> Binary.put
        replay |> replayPackages |> Binary.put
        replay |> replayObjectMap |> Binary.put
        replay |> replayNames |> Binary.put
        replay |> replayActorMap |> Binary.put
        replay |> replayCacheItems |> Binary.put

getFrameBytes :: Binary.Get BS.ByteString
getFrameBytes = do
    NewInt32LE size <- Binary.get
    frames <- Binary.getByteString (fromIntegral size)
    return frames

putFrameBytes :: BS.ByteString -> Binary.Put
putFrameBytes frames = do
    frames |> BS.length |> fromIntegral |> NewInt32LE |> Binary.put
    frames |> Binary.putByteString