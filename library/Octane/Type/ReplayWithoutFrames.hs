module Octane.Type.ReplayWithoutFrames (ReplayWithoutFrames(..), fromRawReplay, toRawReplay) where

import Basics

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.KeyFrame as KeyFrame
import qualified Octane.Type.List as List
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.Property as Property
import qualified Octane.Type.RawReplay as RawReplay
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


-- | A partially-processed replay. This has parsed all of the high-level
-- metadata, but it has not parsed any of the network stream.
--
-- See 'Octane.Type.ReplayWithFrames.ReplayWithFrames'.
data ReplayWithoutFrames = ReplayWithoutFrames
    { replayWithoutFramesVersion1 :: Word32.Word32
    , replayWithoutFramesVersion2 :: Word32.Word32
    , replayWithoutFramesLabel :: Text.Text
    , replayWithoutFramesProperties :: Dictionary.Dictionary Property.Property
    , replayWithoutFramesLevels :: List.List Text.Text
    , replayWithoutFramesKeyFrames :: List.List KeyFrame.KeyFrame
    , replayWithoutFramesStream :: Stream.Stream
    , replayWithoutFramesMessages :: List.List Message.Message
    , replayWithoutFramesMarks :: List.List Mark.Mark
    , replayWithoutFramesPackages :: List.List Text.Text
    , replayWithoutFramesObjects :: List.List Text.Text
    , replayWithoutFramesNames :: List.List Text.Text
    , replayWithoutFramesClasses :: List.List ClassItem.ClassItem
    , replayWithoutFramesCache :: List.List CacheItem.CacheItem
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''ReplayWithoutFrames)

instance Binary ReplayWithoutFrames where
    get = do
        rawReplay <- get
        fromRawReplay rawReplay

    put replayWithoutFrames = do
        rawReplay <- toRawReplay replayWithoutFrames
        put rawReplay

instance NFData ReplayWithoutFrames where


-- | Converts a 'RawReplay.RawReplay' into a 'ReplayWithoutFrames'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
fromRawReplay :: (Monad m) => RawReplay.RawReplay -> m ReplayWithoutFrames
fromRawReplay rawReplay = do
    let header = #header rawReplay
    let content = #content rawReplay

    let getter = do
            version1 <- get
            version2 <- get
            label <- get
            properties <- get
            levels <- get
            keyFrames <- get
            stream <- get
            messages <- get
            marks <- get
            packages <- get
            objects <- get
            names <- get
            classes <- get
            cache <- get

            pure (ReplayWithoutFrames version1 version2 label properties levels keyFrames stream messages marks packages objects names classes cache)
    let bytes = LazyBytes.append header content

    pure (Binary.runGet getter bytes)


-- | Converts a 'ReplayWithoutFrames' into a 'RawReplay.RawReplay'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
toRawReplay :: (Monad m) => ReplayWithoutFrames -> m RawReplay.RawReplay
toRawReplay replay = do
    let header = Binary.runPut (do
            put (#version1 replay)
            put (#version2 replay)
            put (#label replay)
            put (#properties replay))

    let content = Binary.runPut (do
            put (#levels replay)
            put (#keyFrames replay)
            put (#stream replay)
            put (#messages replay)
            put (#marks replay)
            put (#packages replay)
            put (#objects replay)
            put (#names replay)
            put (#classes replay)
            put (#cache replay))

    let footer = LazyBytes.empty

    pure (RawReplay.newRawReplay header content footer)
