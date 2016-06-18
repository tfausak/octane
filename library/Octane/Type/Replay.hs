{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replay (Replay(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified GHC.Generics as Generics
import qualified Octane.Json as Json
import qualified Octane.Type.Actor as Actor
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.KeyFrame as KeyFrame
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.Primitive.Dictionary as Dictionary
import qualified Octane.Type.Primitive.Int32 as Int32
import qualified Octane.Type.Primitive.List as List
import qualified Octane.Type.Primitive.Stream as Stream
import qualified Octane.Type.Primitive.Text as Text
import qualified Octane.Type.Property as Property
import qualified Octane.Utility as Utility
import qualified Text.Printf as Printf

-- | An entire replay. All of the metadata has been parsed, but the actual net
-- stream has not.
data Replay = Replay
    -- Number of bytes in the first section.
    { replaySize1 :: !Int32.Int32
    -- CRC to check the first section.
    , replayCRC1 :: !Int32.Int32
    -- Major replay version number.
    , replayVersion1 :: !Int32.Int32
    -- Minor replay version number.
    , replayVersion2 :: !Int32.Int32
    -- Label, which is always "TAGame.Replay_Soccar_TA".
    , replayLabel :: !Text.Text
    -- High-level metadata about the replay.
    , replayProperties :: !(Dictionary.Dictionary Property.Property)
    -- Number of bytes in the last section.
    , replaySize2 :: !Int32.Int32
    -- CRC to check the last section.
    , replayCRC2 :: !Int32.Int32
    -- Array of strings for all of the levels that need to be loaded (array
    -- length followed by each string)
    , replayLevels :: !(List.List Text.Text)
    -- Array of Keyframe information used for timeline scrubbing (array
    -- length followed by each keyframe struct) (Time, Frame, File Position)
    , replayKeyFrames :: !(List.List KeyFrame.KeyFrame)
    -- Array of bytes that is the bulk of the data. This is the raw network
    -- stream. (array length followed by a bunch of bytes)
    , replayStream :: !Stream.Stream
    -- Array of debugging logs (strings). This reminds me that I should
    -- probably turn these off to make the replays smaller. (array length
    -- followed by each string)
    , replayMessages :: !(List.List Message.Message)
    -- Array of information used to display the Tick marks in the replay (goal
    -- scores). (array length followed by each tick struct) (Type, Frame)
    , replayMarks :: !(List.List Mark.Mark)
    -- Array of strings of replicated Packages
    , replayPackages :: !(List.List Text.Text)
    -- Array of strings for the Object table. Whenever a persistent object gets
    -- referenced in the network stream its path gets added to this array. Then
    -- its index in this array is used in the network stream.
    , replayObjects :: !(List.List Text.Text)
    -- Array of strings for the Name table. "Names" are commonly used strings
    -- that get assigned an integer for use in the network stream.
    , replayNames :: !(List.List Text.Text)
    -- Map of string, integer pairs for the Class Index Map. Whenever a class
    -- is used in the network stream it is given an integer id by this map.
    , replayActors :: !(List.List Actor.Actor)
    -- "Class Net Cache Map" maps each replicated property in a class to an
    -- integer id used in the network stream.
    , replayCacheItems :: !(List.List CacheItem.CacheItem)
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Replay where
    get = do
        size1 <- Binary.getWord32le
        expectedCRC1 <- Binary.getWord32le
        data1 <- Binary.getLazyByteString (fromIntegral size1)
        let actualCRC1 = Utility.crc32 data1
        Monad.when (actualCRC1 /= expectedCRC1) (fail (Printf.printf "First CRC 0x%08x does not match expected value 0x%08x" actualCRC1 expectedCRC1))

        size2 <- Binary.getWord32le
        expectedCRC2 <- Binary.getWord32le
        data2 <- Binary.getLazyByteString (fromIntegral size2)
        let actualCRC2 = Utility.crc32 data2
        Monad.when (actualCRC2 /= expectedCRC2) (fail (Printf.printf "Second CRC 0x%08x does not match expected value 0x%08x" actualCRC2 expectedCRC2))

        pure (flip Binary.runGet (LazyBytes.append data1 data2) (do
            version1 <- Binary.get
            version2 <- Binary.get
            label <- Binary.get
            properties <- Binary.get

            levels <- Binary.get
            keyFrames <- Binary.get
            stream <- Binary.get
            messages <- Binary.get
            marks <- Binary.get
            packages <- Binary.get
            objects <- Binary.get
            names <- Binary.get
            actors <- Binary.get
            cacheItems <- Binary.get

            pure Replay
                { replaySize1 = size1 & fromIntegral & Int32.Int32
                , replayCRC1 = expectedCRC1 & fromIntegral & Int32.Int32
                , replayVersion1 = version1
                , replayVersion2 = version2
                , replayLabel = label
                , replayProperties = properties
                , replaySize2 = size2 & fromIntegral & Int32.Int32
                , replayCRC2 = expectedCRC2 & fromIntegral & Int32.Int32
                , replayLevels = levels
                , replayKeyFrames = keyFrames
                , replayStream = stream
                , replayMessages = messages
                , replayMarks = marks
                , replayPackages = packages
                , replayObjects = objects
                , replayNames = names
                , replayActors = actors
                , replayCacheItems = cacheItems
                }))

    put replay = do
        replay & replaySize1 & Binary.put
        replay & replayCRC1 & Binary.put
        replay & replayVersion1 & Binary.put
        replay & replayVersion2 & Binary.put
        replay & replayLabel & Binary.put
        replay & replayProperties & Binary.put
        replay & replaySize2 & Binary.put
        replay & replayCRC2 & Binary.put
        replay & replayLevels & Binary.put
        replay & replayKeyFrames & Binary.put
        replay & replayStream & Binary.put
        replay & replayMessages & Binary.put
        replay & replayMarks & Binary.put
        replay & replayPackages & Binary.put
        replay & replayObjects & Binary.put
        replay & replayNames & Binary.put
        replay & replayActors & Binary.put
        replay & replayCacheItems & Binary.put

instance DeepSeq.NFData Replay

instance Aeson.ToJSON Replay where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Replay")
