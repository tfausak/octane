{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.ReplayWithoutFrames (ReplayWithoutFrames(..), fromRawReplay, toRawReplay) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
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
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ReplayWithoutFrames)

instance Binary.Binary ReplayWithoutFrames where
    get = do
        rawReplay <- Binary.get
        fromRawReplay rawReplay

    put replayWithoutFrames = do
        rawReplay <- toRawReplay replayWithoutFrames
        Binary.put rawReplay

instance DeepSeq.NFData ReplayWithoutFrames where


-- | Converts a 'RawReplay.RawReplay' into a 'ReplayWithoutFrames'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
fromRawReplay :: (Monad m) => RawReplay.RawReplay -> m ReplayWithoutFrames
fromRawReplay rawReplay = do
    let header = #header rawReplay
    let content = #content rawReplay

    let get = do
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
            classes <- Binary.get
            cache <- Binary.get

            pure (ReplayWithoutFrames version1 version2 label properties levels keyFrames stream messages marks packages objects names classes cache)
    let bytes = LazyBytes.append header content

    pure (Binary.runGet get bytes)


-- | Converts a 'ReplayWithoutFrames' into a 'RawReplay.RawReplay'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
toRawReplay :: (Monad m) => ReplayWithoutFrames -> m RawReplay.RawReplay
toRawReplay replay = do
    let header = Binary.runPut (do
            Binary.put (#version1 replay)
            Binary.put (#version2 replay)
            Binary.put (#label replay)
            Binary.put (#properties replay))

    let content = Binary.runPut (do
            Binary.put (#levels replay)
            Binary.put (#keyFrames replay)
            Binary.put (#stream replay)
            Binary.put (#messages replay)
            Binary.put (#marks replay)
            Binary.put (#packages replay)
            Binary.put (#objects replay)
            Binary.put (#names replay)
            Binary.put (#classes replay)
            Binary.put (#cache replay))

    let footer = LazyBytes.empty

    pure (RawReplay.newRawReplay header content footer)
