{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Octane.Types.Actor (Actor)
import Octane.Types.Goal (Goal)
import Octane.Types.Int32LE (Int32LE)
import Octane.Types.Int64LE (Int64LE)
import Octane.Types.List (List)
import Octane.Types.PCString (PCString)

import Control.Monad (replicateM)
import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.IEEE754 as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M

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

type Properties = M.Map PCString Property

data Property
    = ArrayProperty Int64LE [Properties]
    | FloatProperty Int64LE Float
    | IntProperty Int64LE Int32LE
    | NameProperty Int64LE PCString
    | StrProperty Int64LE PCString
    deriving (Show)

type Effects = List Effect

type Effect = PCString

type KeyFrames = [KeyFrame]

data KeyFrame = NewKeyFrame
    { keyFrameTime :: Float
    , keyFrameFrame :: Int32LE
    , keyFramePosition :: Int32LE
    } deriving (Show)

type Frames = BS.ByteString

type Messages = [Message]

data Message = NewMessage
    { messageFrame :: Int32LE
    , messageName :: PCString
    , messageContent :: PCString
    } deriving (Show)

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
    properties <- getProperties
    separator <- B.getByteString 8 -- TODO
    effects <- B.get
    keyFrames <- getKeyFrames
    frames <- getFrames -- TODO
    messages <- getMessages
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

getProperties :: B.Get Properties
getProperties = do
    key <- B.get
    if key == "None"
    then do
        return M.empty
    else do
        value <- getProperty
        let property = M.singleton key value
        properties <- getProperties
        return (M.union property properties)

getProperty :: B.Get Property
getProperty = do
    kind <- B.get
    size <- B.get
    property <- case kind :: PCString of
        "ArrayProperty" -> getArrayProperty size
        "FloatProperty" -> getFloatProperty size
        "IntProperty" -> getIntProperty size
        "NameProperty" -> getNameProperty size
        "StrProperty" -> getStrProperty size
        _ -> fail ("unknown property type " ++ show kind)
    return property

getArrayProperty :: Int64LE -> B.Get Property
getArrayProperty size = do
    otherSize <- B.getWord32le
    array <- replicateM (fromIntegral otherSize) getProperties
    return (ArrayProperty size array)

getFloatProperty :: Int64LE -> B.Get Property
getFloatProperty size = do
    float <- case size of
        4 -> B.getFloat32le
        _ -> fail ("unknown FloatProperty size " ++ show size)
    return (FloatProperty size float)

getIntProperty :: Int64LE -> B.Get Property
getIntProperty size = do
    int <- case size of
        4 -> B.get
        _ -> fail ("unknown IntProperty size " ++ show size)
    return (IntProperty size int)

getNameProperty :: Int64LE -> B.Get Property
getNameProperty size = do
    name <- B.get
    return (NameProperty size name)

getStrProperty :: Int64LE -> B.Get Property
getStrProperty size = do
    string <- B.get
    return (StrProperty size string)

getKeyFrames :: B.Get KeyFrames
getKeyFrames = do
    size <- B.getWord32le
    keyFrames <- replicateM (fromIntegral size) getKeyFrame
    return keyFrames

getKeyFrame :: B.Get KeyFrame
getKeyFrame = do
    time <- B.getFloat32le
    frame <- B.get
    position <- B.get
    return NewKeyFrame
        { keyFrameTime = time
        , keyFrameFrame = frame
        , keyFramePosition = position
        }

getFrames :: B.Get Frames
getFrames = do
    size <- B.getWord32le
    frames <- B.getByteString (fromIntegral size)
    return frames

getMessages :: B.Get Messages
getMessages = do
    size <- B.getWord32le
    messages <- replicateM (fromIntegral size) getMessage
    return messages

getMessage :: B.Get Message
getMessage = do
    frame <- B.get
    name <- B.get
    content <- B.get
    return NewMessage
        { messageFrame = frame
        , messageName = name
        , messageContent = content
        }

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
    putProperties (replayProperties replay)
    B.putByteString (replaySeparator replay)
    B.put (replayEffects replay)
    putKeyFrames (replayKeyFrames replay)
    putFrames (replayFrames replay)
    putMessages (replayMessages replay)
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

putProperties :: Properties -> B.Put
putProperties properties = do
    mapM_ putProperty (M.assocs properties)
    B.put ("None" :: PCString)

putProperty :: (PCString, Property) -> B.Put
putProperty (key, value) = do
    B.put key
    case value of
        ArrayProperty _ _ -> putArrayProperty value
        FloatProperty _ _ -> putFloatProperty value
        IntProperty _ _ -> putIntProperty value
        NameProperty _ _ -> putNameProperty value
        StrProperty _ _ -> putStrProperty value

putArrayProperty :: Property -> B.Put
putArrayProperty (ArrayProperty size array) = do
    B.put ("ArrayProperty" :: PCString)
    B.put size
    let otherSize = fromIntegral (length array)
    B.putWord32le otherSize
    mapM_ putProperties array
putArrayProperty _ = undefined

putFloatProperty :: Property -> B.Put
putFloatProperty (FloatProperty size float) = do
    B.put ("FloatProperty" :: PCString)
    B.put size
    case size of
        4 -> B.putFloat32le float
        _ -> undefined
putFloatProperty _ = undefined

putIntProperty :: Property -> B.Put
putIntProperty (IntProperty size int) = do
    B.put ("IntProperty" :: PCString)
    B.put size
    case size of
        4 -> B.put int
        _ -> undefined
putIntProperty _ = undefined

putNameProperty :: Property -> B.Put
putNameProperty (NameProperty size name) = do
    B.put ("NameProperty" :: PCString)
    B.put size
    B.put name
putNameProperty _ = undefined

putStrProperty :: Property -> B.Put
putStrProperty (StrProperty size string) = do
    B.put ("StrProperty" :: PCString)
    B.put size
    B.put string
putStrProperty _ = undefined

putKeyFrames :: KeyFrames -> B.Put
putKeyFrames keyFrames = do
    B.putWord32le (fromIntegral (length keyFrames))
    mapM_ putKeyFrame keyFrames

putKeyFrame :: KeyFrame -> B.Put
putKeyFrame keyFrame = do
    B.putFloat32le (keyFrameTime keyFrame)
    B.put (keyFrameFrame keyFrame)
    B.put (keyFramePosition keyFrame)

putFrames :: Frames -> B.Put
putFrames frames = do
    B.putWord32le (fromIntegral (BS.length frames))
    B.putByteString frames

putMessages :: Messages -> B.Put
putMessages messages = do
    B.putWord32le (fromIntegral (length messages))
    mapM_ putMessage messages

putMessage :: Message -> B.Put
putMessage message = do
    B.put (messageFrame message)
    B.put (messageName message)
    B.put (messageContent message)
