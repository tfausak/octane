{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Control.Monad (replicateM)
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.IEEE754 as B
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
    , replaySeperator :: BS.ByteString
    , replayEffects :: Effects
    , replayKeyFrames :: KeyFrames
    , replayFrames :: BS.ByteString
    , replayMessages :: Messages
    , replayGoals :: Goals
    , replayPackages :: Packages
    , replayObjects :: Objects
    , replayUnknown :: BS.ByteString
    , replayEntities :: Entities
    , replayOutro :: BSL.ByteString
    } deriving (Eq, Ord, Read, Show)

instance B.Binary Replay where
    get = getReplay
    put = putReplay

type Properties = M.Map T.Text Property

data Property
    = ArrayProperty [Properties]
    | FloatProperty Float
    | IntProperty Int
    | NameProperty T.Text
    | StrProperty T.Text
    deriving (Eq, Ord, Read, Show)

type Effects = [Effect]

type Effect = T.Text

type KeyFrames = [KeyFrame]

data KeyFrame = NewKeyFrame
    { keyFrameTime :: Float
    , keyFrameFrame :: Int
    , keyFramePosition :: Int
    } deriving (Eq, Ord, Read, Show)

type Messages = [Message]

data Message = NewMessage
    { messageFrame :: Int
    , messageName :: T.Text
    , messageContent :: T.Text
    } deriving (Eq, Ord, Read, Show)

type Goals = [Goal]

data Goal = NewGoal
    { goalTeam :: Team
    , goalFrame :: Int
    } deriving (Eq, Ord, Read, Show)

data Team
    = Blue
    | Orange
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

type Packages = [Package]

type Package = T.Text

type Objects = [Object]

type Object = T.Text

type Entities = [Entity]

data Entity = NewEntity
    { entityName :: T.Text
    , entityValue :: Int
    } deriving (Eq, Ord, Read, Show)

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
        , replaySeperator = separator
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
getArrayProperty _ = do
    size <- B.getWord32le
    array <- replicateM (fromIntegral size) getProperties
    return (ArrayProperty array)

getFloatProperty :: B.Word64 -> B.Get Property
getFloatProperty size = do
    float <- case size of
        4 -> B.getFloat32le
        _ -> fail ("unknown FloatProperty size " ++ show size)
    return (FloatProperty float)

getIntProperty :: B.Word64 -> B.Get Property
getIntProperty size = do
    word <- case size of
        4 -> B.getWord32le
        _ -> fail ("unknown IntProperty size " ++ show size)
    let integer = fromIntegral word
    return (IntProperty integer)

getNameProperty :: B.Word64 -> B.Get Property
getNameProperty _ = do
    name <- getText
    return (NameProperty name)

getStrProperty :: B.Word64 -> B.Get Property
getStrProperty _ = do
    string <- getText
    return (StrProperty string)

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

getFrames :: B.Get BS.ByteString
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

-- TODO: This library should be able to write replays, but it's not currently a
--   high priority.
putReplay :: Replay -> B.Put
putReplay _replay = do
    undefined
