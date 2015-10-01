{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Replay where

import Octane.Types.Actor (Actor)
import Octane.Types.Goal (Goal)
import Octane.Types.KeyFrame (KeyFrame)
import Octane.Types.List (List)
import Octane.Types.Message (Message)
import Octane.Types.PCString (PCString)
import Octane.Types.Property (Property)
import Octane.Types.Table (Table)

import Control.Monad (replicateM)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Replay = NewReplay
    { replayIntro :: BS.ByteString
    , replayLabel :: PCString
    , replayProperties :: Table Property
    , replaySeparator :: BS.ByteString
    , replayEffects :: List PCString
    , replayKeyFrames :: List KeyFrame
    , replayFrames :: BS.ByteString
    , replayMessages :: List Message
    , replayGoals :: List Goal
    , replayPackages :: List PCString
    , replayObjects :: List PCString
    , replayUnknown :: BS.ByteString
    , replayActors :: List Actor
    , replayOutro :: BSL.ByteString
    } deriving (Show)

instance B.Binary Replay where
    get = do
        intro <- B.getByteString 16 -- NOTE: xxxx0000xxxxxxxx6303000009000000
        label <- B.get -- NOTE: Always "TAGame.Replay_Soccar_TA".
        properties <- B.get
        separator <- B.getByteString 8 -- NOTE: xxxxxx00xxxxxxxx
        effects <- B.get
        keyFrames <- B.get
        frames <- getFrames -- TODO
        messages <- B.get
        goals <- B.get
        packages <- B.get
        objects <- B.get
        unknown <- getUnknown
        actors <- B.get
        outro <- B.getRemainingLazyByteString -- TODO

        return NewReplay
            { replayIntro = intro
            , replayLabel = label
            , replayProperties = properties
            , replaySeparator = separator
            , replayEffects = effects
            , replayKeyFrames = keyFrames
            , replayFrames = frames
            , replayMessages = messages
            , replayGoals = goals
            , replayPackages = packages
            , replayObjects = objects
            , replayUnknown = unknown
            , replayActors = actors
            , replayOutro = outro
            }

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
        B.putByteString (replayUnknown replay)
        B.put (replayActors replay)
        B.putLazyByteString (replayOutro replay)

-- * Readers

getTexts :: B.Get [PCString]
getTexts = do
    size <- B.getWord32le
    texts <- replicateM (fromIntegral size) B.get
    return texts

getFrames :: B.Get BS.ByteString
getFrames = do
    size <- B.getWord32le
    frames <- B.getByteString (fromIntegral size)
    return frames

getUnknown :: B.Get BS.ByteString
getUnknown = do
    unknown <- B.getByteString 4
    if unknown == "\NUL\NUL\NUL\NUL"
    then return unknown
    else fail ("unexpected value " ++ show unknown)

-- * Writers

putTexts :: [PCString] -> B.Put
putTexts texts = do
    B.putWord32le (fromIntegral (length texts))
    mapM_ B.put texts

putFrames :: BS.ByteString -> B.Put
putFrames frames = do
    B.putWord32le (fromIntegral (BS.length frames))
    B.putByteString frames
