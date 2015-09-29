{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.IEEE754 as B
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
    { replayIntro :: ByteString
    , replayLabel :: T.Text
    , replayProperties :: Properties
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

-- * Readers

getReplay :: B.Get Replay
getReplay = do
    -- TODO: The meaning of these bytes is unclear.
    intro <- B.getByteString 16

    -- NOTE: This label appears to always be "TAGame.Replay_Soccar_TA".
    label <- getText

    properties <- getProperties

    return NewReplay
        { replayIntro = intro
        , replayLabel = label
        , replayProperties = properties
        }

getText :: B.Get T.Text
getText = do
    size <- B.getWord32le
    bytes <- B.getByteString (fromIntegral size)
    let text = T.dropEnd 1 (decodeUtf8 bytes)
    return text

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

-- * Writers

-- TODO: This library should be able to write replays, but it's not currently a
--   high priority.
putReplay :: Replay -> B.Put
putReplay _replay = do
    undefined
