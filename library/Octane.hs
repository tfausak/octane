{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Control.Monad (replicateM)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.IEEE754 as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T

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
    , replayEntities :: Entities
    , replayOutro :: BSL.ByteString
    } deriving (Show)

instance B.Binary Replay where
    get = getReplay
    put = putReplay

type Properties = M.Map T.Text Property

data Property
    = ArrayProperty B.Word64 [Properties]
    | FloatProperty B.Word64 Float
    | IntProperty B.Word64 Int
    | NameProperty B.Word64 T.Text
    | StrProperty B.Word64 T.Text
    deriving (Show)

type Effects = [Effect]

type Effect = T.Text

type KeyFrames = [KeyFrame]

data KeyFrame = NewKeyFrame
    { keyFrameTime :: Float
    , keyFrameFrame :: Int
    , keyFramePosition :: Int
    } deriving (Show)

type Frames = BS.ByteString

type Messages = [Message]

data Message = NewMessage
    { messageFrame :: Int
    , messageName :: T.Text
    , messageContent :: T.Text
    } deriving (Show)

type Goals = [Goal]

data Goal = NewGoal
    { goalTeam :: Team
    , goalFrame :: Int
    } deriving (Show)

data Team
    = Blue
    | Orange
    deriving (Show)

type Packages = [Package]

type Package = T.Text

type Objects = [Object]

type Object = T.Text

type Entities = [Entity]

data Entity = NewEntity
    { entityName :: T.Text
    , entityValue :: Int
    } deriving (Show)

-- * Readers

getReplay :: B.Get Replay
getReplay = do
    -- TODO: The meaning of these bytes is unclear.
    intro <- B.getByteString 16

    -- NOTE: This label appears to always be "TAGame.Replay_Soccar_TA".
    label <- getText

    properties <- getProperties

    -- TODO: The meaning of these bytes is also unclear.
    separator <- B.getByteString 8

    effects <- getTexts
    keyFrames <- getKeyFrames

    -- TODO: Actually parse these frames.
    frames <- getFrames

    messages <- getMessages
    goals <- getGoals
    packages <- getPackages
    objects <- getObjects

    -- TODO: It is not clear what is in these bytes.
    unknown <- B.getByteString 4
    if unknown == "\NUL\NUL\NUL\NUL"
    then return ()
    else fail ("unexpected value " ++ show unknown)

    entities <- getEntities

    -- TODO: The meaning of these bytes is unclear.
    outro <- B.getRemainingLazyByteString

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
        , replayEntities = entities
        , replayOutro = outro
        }

getText :: B.Get T.Text
getText = do
    size <- B.getWord32le
    bytes <- B.getByteString (fromIntegral size)
    let text = T.dropEnd 1 (decodeUtf8 bytes)
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
    word <- case size of
        4 -> B.getWord32le
        _ -> fail ("unknown IntProperty size " ++ show size)
    let integer = fromIntegral word
    return (IntProperty size integer)

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
    frame <- B.getWord32le
    position <- B.getWord32le
    return NewKeyFrame
        { keyFrameTime = time
        , keyFrameFrame = fromIntegral frame
        , keyFramePosition = fromIntegral position
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
    frame <- B.getWord32le
    name <- getText
    content <- getText
    return NewMessage
        { messageFrame = fromIntegral frame
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
    frame <- B.getWord32le
    return NewGoal
            { goalTeam = team
            , goalFrame = fromIntegral frame
            }

getPackages :: B.Get Packages
getPackages = do
    size <- B.getWord32le
    packages <- replicateM (fromIntegral size) getPackage
    return packages

getPackage :: B.Get Package
getPackage = do
    package <- getText
    return package

getObjects :: B.Get Objects
getObjects = do
    size <- B.getWord32le
    objects <- replicateM (fromIntegral size) getObject
    return objects

getObject :: B.Get Object
getObject = do
    object <- getText
    return object

getEntities :: B.Get Entities
getEntities = do
    size <- B.getWord32le
    entities <- replicateM (fromIntegral size) getEntity
    return entities

getEntity :: B.Get Entity
getEntity = do
    name <- getText
    value <- B.getWord32le
    return NewEntity
        { entityName = name
        , entityValue = fromIntegral value
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

putText :: T.Text -> B.Put
putText text = do
    let size = fromIntegral (T.length text) + 1
    B.putWord32le size
    let bytes = BS.concat [encodeUtf8 text, "\NUL"]
    B.putByteString bytes

putTexts :: [T.Text] -> B.Put
putTexts texts = do
    let size = fromIntegral (length texts)
    B.putWord32le size
    mapM_ putText texts

putProperties :: Properties -> B.Put
putProperties properties = do
    mapM_ putProperty properties
    putText "None"

putProperty :: Property -> B.Put
putProperty property = do
    case property of
        ArrayProperty _ _ -> putArrayProperty property
        FloatProperty _ _ -> putFloatProperty property
        IntProperty _ _ -> putIntProperty property
        NameProperty _ _ -> putNameProperty property
        StrProperty _ _ -> putStrProperty property

putArrayProperty :: Property -> B.Put
putArrayProperty (ArrayProperty size array) = do
    putText "ArrayProperty"
    B.putWord64le size
    let otherSize = fromIntegral (length array)
    B.putWord32le otherSize
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
putIntProperty (IntProperty size integer) = do
    putText "IntProperty"
    B.putWord64le size
    case size of
        4 -> B.putWord32le (fromIntegral integer)
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
    putText "StryProperty"
    B.putWord64le size
    putText string
putStrProperty _ = undefined

putKeyFrames :: KeyFrames -> B.Put
putKeyFrames keyFrames = do
    let size = fromIntegral (length keyFrames)
    B.putWord32le size
    mapM_ putKeyFrame keyFrames

putKeyFrame :: KeyFrame -> B.Put
putKeyFrame keyFrame = do
    B.putFloat32le (keyFrameTime keyFrame)
    B.putWord32le (fromIntegral (keyFrameFrame keyFrame))
    B.putWord32le (fromIntegral (keyFramePosition keyFrame))

putFrames :: Frames -> B.Put
putFrames frames = do
    let size = fromIntegral (BS.length frames)
    B.putWord32le size
    B.putByteString frames

putMessages :: Messages -> B.Put
putMessages messages = do
    let size = fromIntegral (length messages)
    B.putWord32le size
    mapM_ putMessage messages

putMessage :: Message -> B.Put
putMessage message = do
    B.putWord32le (fromIntegral (messageFrame message))
    putText (messageName message)
    putText (messageContent message)
