{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Octane.Types.Actor (Actor)
import Octane.Types.Goal (Goal)
import Octane.Types.KeyFrame (KeyFrame)
import Octane.Types.List (List)
import Octane.Types.Message (Message)
import Octane.Types.PCString (PCString)
import Octane.Types.Property (Property)
import Octane.Types.Table (Table)

import Control.Monad (replicateM)
import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- * High-level interface

main :: IO ()
main = do
    files <- getArgs
    results <- mapM B.decodeFileOrFail files
    mapM_ printResult results

printResult :: Either (B.ByteOffset, String) Replay -> IO ()
printResult result = case result of
    Left (offset, message) -> putStrLn (show offset ++ ": " ++ message)
    Right replay -> print replay

-- * Types

data Replay = NewReplay
    { replayIntro :: BS.ByteString
    , replayLabel :: PCString
    , replayProperties :: Properties
    , replaySeparator :: BS.ByteString
    , replayEffects :: Effects
    , replayKeyFrames :: KeyFrames
    , replayFrames :: Frames
    , replayMessages :: Messages
    , replayGoals :: Goals
    , replayPackages :: Packages
    , replayObjects :: Objects
    , replayUnknown :: BS.ByteString
    , replayActors :: Actors
    , replayOutro :: BSL.ByteString
    } deriving (Show)

instance B.Binary Replay where
    get = getReplay
    put = putReplay

type Properties = Table Property

type Effects = List Effect

type Effect = PCString

type KeyFrames = List KeyFrame

type Frames = BS.ByteString

type Messages = List Message

type Goals = List Goal

type Packages = List Package

type Package = PCString

type Objects = List Object

type Object = PCString

type Actors = List Actor

-- * Readers

getReplay :: B.Get Replay
getReplay = do
    intro <- B.getByteString 16 -- TODO
    label <- B.get -- NOTE: Always "TAGame.Replay_Soccar_TA".
    properties <- B.get
    separator <- B.getByteString 8 -- TODO
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

getTexts :: B.Get [PCString]
getTexts = do
    size <- B.getWord32le
    texts <- replicateM (fromIntegral size) B.get
    return texts

getFrames :: B.Get Frames
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

putReplay :: Replay -> B.Put
putReplay replay = do
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

putTexts :: [PCString] -> B.Put
putTexts texts = do
    B.putWord32le (fromIntegral (length texts))
    mapM_ B.put texts

putFrames :: Frames -> B.Put
putFrames frames = do
    B.putWord32le (fromIntegral (BS.length frames))
    B.putByteString frames
