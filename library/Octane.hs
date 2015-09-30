{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Octane.Types.Int32LE (Int32LE)

import Control.Monad (replicateM)
import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.IEEE754 as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
    , replayLabel :: T.Text
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

type Properties = M.Map T.Text Property

data Property
    = ArrayProperty B.Word64 [Properties]
    | FloatProperty B.Word64 Float
    | IntProperty B.Word64 Int32LE
    | NameProperty B.Word64 T.Text
    | StrProperty B.Word64 T.Text
    deriving (Show)

type Effects = [Effect]

type Effect = T.Text

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
    , messageName :: T.Text
    , messageContent :: T.Text
    } deriving (Show)

type Goals = [Goal]

data Goal = NewGoal
    { goalTeam :: Team
    , goalFrame :: Int32LE
    } deriving (Show)

data Team
    = Blue
    | Orange
    deriving (Show)

type Packages = [Package]

type Package = T.Text

type Objects = [Object]

type Object = T.Text

type Actors = [Actor]

data Actor = NewActor
    { actorName :: T.Text
    , actorValue :: Int32LE
    } deriving (Show)

-- * Readers

getReplay :: B.Get Replay
getReplay = do
    intro <- B.getByteString 16 -- TODO
    label <- getText -- NOTE: Always "TAGame.Replay_Soccar_TA".
    properties <- getProperties
    separator <- B.getByteString 8 -- TODO
    effects <- getTexts
    keyFrames <- getKeyFrames
    frames <- getFrames -- TODO
    messages <- getMessages
    goals <- getGoals
    packages <- getPackages
    objects <- getObjects
    unknown <- getUnknown
    actors <- getActors
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

getText :: B.Get T.Text
getText = do
    size <- B.getWord32le
    bytes <- B.getByteString (fromIntegral size)
    let text = T.dropEnd 1 (T.decodeUtf8 bytes)
    return text

getTexts :: B.Get [T.Text]
getTexts = do
    size <- B.getWord32le
    texts <- replicateM (fromIntegral size) getText
    return texts

getProperties :: B.Get Properties
getProperties = do
    key <- getText
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
    kind <- getText
    size <- B.getWord64le
    property <- case kind of
        "ArrayProperty" -> getArrayProperty size
        "FloatProperty" -> getFloatProperty size
        "IntProperty" -> getIntProperty size
        "NameProperty" -> getNameProperty size
        "StrProperty" -> getStrProperty size
        _ -> fail ("unknown property type " ++ show kind)
    return property

getArrayProperty :: B.Word64 -> B.Get Property
getArrayProperty size = do
    otherSize <- B.getWord32le
    array <- replicateM (fromIntegral otherSize) getProperties
    return (ArrayProperty size array)

getFloatProperty :: B.Word64 -> B.Get Property
getFloatProperty size = do
    float <- case size of
        4 -> B.getFloat32le
        _ -> fail ("unknown FloatProperty size " ++ show size)
    return (FloatProperty size float)

getIntProperty :: B.Word64 -> B.Get Property
getIntProperty size = do
    int <- case size of
        4 -> B.get
        _ -> fail ("unknown IntProperty size " ++ show size)
    return (IntProperty size int)

getNameProperty :: B.Word64 -> B.Get Property
getNameProperty size = do
    name <- getText
    return (NameProperty size name)

getStrProperty :: B.Word64 -> B.Get Property
getStrProperty size = do
    string <- getText
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
    name <- getText
    content <- getText
    return NewMessage
        { messageFrame = frame
        , messageName = name
        , messageContent = content
        }

getGoals :: B.Get Goals
getGoals = do
    size <- B.getWord32le
    goals <- replicateM (fromIntegral size) getGoal
    return goals

getGoal :: B.Get Goal
getGoal = do
    kind <- getText
    team <- case kind of
        "Team0Goal" -> return Blue
        "Team1Goal" -> return Orange
        _ -> fail ("unknown goal type " ++ show kind)
    frame <- B.get
    return NewGoal
            { goalTeam = team
            , goalFrame = frame
            }

getPackages :: B.Get Packages
getPackages = do
    size <- B.getWord32le
    replicateM (fromIntegral size) getText

getObjects :: B.Get Objects
getObjects = do
    size <- B.getWord32le
    replicateM (fromIntegral size) getText

getUnknown :: B.Get BS.ByteString
getUnknown = do
    unknown <- B.getByteString 4
    if unknown == "\NUL\NUL\NUL\NUL"
    then return unknown
    else fail ("unexpected value " ++ show unknown)

getActors :: B.Get Actors
getActors = do
    size <- B.getWord32le
    actors <- replicateM (fromIntegral size) getActor
    return actors

getActor :: B.Get Actor
getActor = do
    name <- getText
    value <- B.get
    return NewActor
        { actorName = name
        , actorValue = value
        }

-- * Writers

putReplay :: Replay -> B.Put
putReplay replay = do
    B.putByteString (replayIntro replay)
    putText (replayLabel replay)
    putProperties (replayProperties replay)
    B.putByteString (replaySeparator replay)
    putTexts (replayEffects replay)
    putKeyFrames (replayKeyFrames replay)
    putFrames (replayFrames replay)
    putMessages (replayMessages replay)
    putGoals (replayGoals replay)
    putPackages (replayPackages replay)
    putObjects (replayObjects replay)
    B.putByteString (replayUnknown replay)
    putActors (replayActors replay)
    B.putLazyByteString (replayOutro replay)

putText :: T.Text -> B.Put
putText text = do
    B.putWord32le (fromIntegral (T.length text) + 1)
    B.putByteString (BS.concat [T.encodeUtf8 text, "\NUL"])

putTexts :: [T.Text] -> B.Put
putTexts texts = do
    B.putWord32le (fromIntegral (length texts))
    mapM_ putText texts

putProperties :: Properties -> B.Put
putProperties properties = do
    mapM_ putProperty (M.assocs properties)
    putText "None"

putProperty :: (T.Text, Property) -> B.Put
putProperty (key, value) = do
    putText key
    case value of
        ArrayProperty _ _ -> putArrayProperty value
        FloatProperty _ _ -> putFloatProperty value
        IntProperty _ _ -> putIntProperty value
        NameProperty _ _ -> putNameProperty value
        StrProperty _ _ -> putStrProperty value

putArrayProperty :: Property -> B.Put
putArrayProperty (ArrayProperty size array) = do
    putText "ArrayProperty"
    B.putWord64le size
    let otherSize = fromIntegral (length array)
    B.putWord32le otherSize
    mapM_ putProperties array
putArrayProperty _ = undefined

putFloatProperty :: Property -> B.Put
putFloatProperty (FloatProperty size float) = do
    putText "FloatProperty"
    B.putWord64le size
    case size of
        4 -> B.putFloat32le float
        _ -> undefined
putFloatProperty _ = undefined

putIntProperty :: Property -> B.Put
putIntProperty (IntProperty size int) = do
    putText "IntProperty"
    B.putWord64le size
    case size of
        4 -> B.put int
        _ -> undefined
putIntProperty _ = undefined

putNameProperty :: Property -> B.Put
putNameProperty (NameProperty size name) = do
    putText "NameProperty"
    B.putWord64le size
    putText name
putNameProperty _ = undefined

putStrProperty :: Property -> B.Put
putStrProperty (StrProperty size string) = do
    putText "StrProperty"
    B.putWord64le size
    putText string
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
    putText (messageName message)
    putText (messageContent message)

putGoals :: Goals -> B.Put
putGoals goals = do
    let size = fromIntegral (length goals)
    B.putWord32le size
    mapM_ putGoal goals

putGoal :: Goal -> B.Put
putGoal goal = do
    putText (case goalTeam goal of
        Blue -> "Team0Goal"
        Orange -> "Team1Goal")
    B.put (goalFrame goal)

putPackages :: Packages -> B.Put
putPackages packages = do
    B.putWord32le (fromIntegral (length packages))
    mapM_ putText packages

putObjects :: Objects -> B.Put
putObjects objects = do
    B.putWord32le (fromIntegral (length objects))
    mapM_ putText objects

putActors :: Actors -> B.Put
putActors actors = do
    B.putWord32le (fromIntegral (length actors))
    mapM_ putActor actors

putActor :: Actor -> B.Put
putActor actor = do
    putText (actorName actor)
    B.put (actorValue actor)
