{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Replay where

import Octane.Types.Actor (Actor)
import Octane.Types.CacheItem (CacheItem)
import Octane.Types.Goal (Goal)
import Octane.Types.KeyFrame (KeyFrame)
import Octane.Types.List (List)
import Octane.Types.Message (Message)
import Octane.Types.PCString (PCString)
import Octane.Types.Property (Property)
import Octane.Types.Table (Table)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS

data Replay = NewReplay
    -- NOTE: This always has the format "xxxx0000xxxxxxxx6303000009000000".
    --   Apparently it contains a CRC check and a version number.
    { replayIntro :: BS.ByteString
    -- NOTE: This is always "TAGame.Replay_Soccar_TA". It is unlikely to
    --   change.
    , replayLabel :: PCString
    , replayProperties :: Table Property
    -- NOTE: This always has the format "xxxxxx00xxxxxxxx". So far, nobody
    --   has any idea what it is.
    , replaySeparator :: BS.ByteString
    , replayEffects :: List PCString
    , replayKeyFrames :: List KeyFrame
    -- TODO: Actually parse the individual frames. This is hard work that
    --   none of the other parsers have done yet.
    , replayFrames :: BS.ByteString
    , replayMessages :: List Message
    , replayGoals :: List Goal
    , replayPackages :: List PCString
    , replayObjects :: List PCString
    , replayNames :: List PCString
    , replayActors :: List Actor
    , replayCacheItems :: List CacheItem
    } deriving (Show)

instance B.Binary Replay where
    get = NewReplay
        <$> B.getByteString 16
        <*> B.get
        <*> B.get
        <*> B.getByteString 8
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
        B.putByteString (replayIntro replay)
        B.put (replayLabel replay)
        B.put (replayProperties replay)
        B.putByteString (replaySeparator replay)
        B.put (replayEffects replay)
        B.put (replayKeyFrames replay)
        putFrames (replayFrames replay)
        B.put (replayMessages replay)
        B.put (replayGoals replay)
        B.put (replayPackages replay)
        B.put (replayObjects replay)
        B.put (replayNames replay)
        B.put (replayActors replay)
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
