{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Replay where

import Octane.Types.ActorMap (ActorMap)
import Octane.Types.CacheItem (CacheItem)
import Octane.Types.Goal (Goal)
import Octane.Types.Int32LE (Int32LE)
import Octane.Types.KeyFrame (KeyFrame)
import Octane.Types.List (List)
import Octane.Types.Message (Message)
import Octane.Types.ObjectMap (ObjectMap)
import Octane.Types.PCString (PCString)
import Octane.Types.Property (Property)
import Octane.Types.Table (Table)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS

data Replay = NewReplay
    { replayIntro :: Int32LE
    , replayCRC :: BS.ByteString
    , replayVersion1 :: Int32LE
    , replayVersion2 :: Int32LE
    , replayLabel :: PCString
    , replayProperties :: Table Property
    , replaySize :: Int32LE
    , replaySeparator :: BS.ByteString
    , replayEffects :: List PCString
    , replayKeyFrames :: List KeyFrame
    , replayFrames :: BS.ByteString -- TODO: issue #1
    , replayMessages :: List Message
    , replayGoals :: List Goal
    , replayPackages :: List PCString
    , replayObjectMap :: ObjectMap
    , replayNames :: List PCString
    , replayActorMap :: ActorMap
    -- NOTE: This is a "class net cache".
    , replayCacheItems :: List CacheItem
    } deriving (Show)

instance B.Binary Replay where
    get = NewReplay
        <$> B.get
        <*> B.getByteString 4
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.getByteString 4
        <*> B.get
        <*> B.get
        <*> getFrames
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.get
        <*> B.get

    put replay = do
        B.put (replayIntro replay)
        B.putByteString (replayCRC replay)
        B.put (replayVersion1 replay)
        B.put (replayVersion2 replay)
        B.put (replayLabel replay)
        B.put (replayProperties replay)
        B.put (replaySize replay)
        B.putByteString (replaySeparator replay)
        B.put (replayEffects replay)
        B.put (replayKeyFrames replay)
        putFrames (replayFrames replay)
        B.put (replayMessages replay)
        B.put (replayGoals replay)
        B.put (replayPackages replay)
        B.put (replayObjectMap replay)
        B.put (replayNames replay)
        B.put (replayActorMap replay)
        B.put (replayCacheItems replay)

getFrames :: B.Get BS.ByteString
getFrames = do
    size <- B.getWord32le
    frames <- B.getByteString (fromIntegral size)
    return frames

putFrames :: BS.ByteString -> B.Put
putFrames frames = do
    B.putWord32le (fromIntegral (BS.length frames))
    B.putByteString frames
