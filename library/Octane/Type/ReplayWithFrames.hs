{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.ReplayWithFrames
    ( ReplayWithFrames(..)
    , fromReplayWithoutFrames
    , toReplayWithoutFrames
    ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.KeyFrame as KeyFrame
import qualified Octane.Type.List as List
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.Property as Property
import qualified Octane.Type.ReplayWithoutFrames as ReplayWithoutFrames
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Utility.Generator as Generator
import qualified Octane.Utility.Parser as Parser


-- | A fully-processed replay. This has all of the frames from the network
-- stream as well as all of the metadata.
--
-- See 'Octane.Type.OptimizedReplay.OptimizedReplay'.
data ReplayWithFrames = ReplayWithFrames
    { replayWithFramesVersion1 :: Word32.Word32
    , replayWithFramesVersion2 :: Word32.Word32
    , replayWithFramesLabel :: Text.Text
    , replayWithFramesProperties :: Dictionary.Dictionary Property.Property
    , replayWithFramesLevels :: List.List Text.Text
    , replayWithFramesKeyFrames :: List.List KeyFrame.KeyFrame
    , replayWithFramesFrames :: [Frame.Frame]
    , replayWithFramesMessages :: List.List Message.Message
    , replayWithFramesMarks :: List.List Mark.Mark
    , replayWithFramesPackages :: List.List Text.Text
    , replayWithFramesObjects :: List.List Text.Text
    , replayWithFramesNames :: List.List Text.Text
    , replayWithFramesClasses :: List.List ClassItem.ClassItem
    , replayWithFramesCache :: List.List CacheItem.CacheItem
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ReplayWithFrames)

instance Binary.Binary ReplayWithFrames where
    get = do
        replayWithoutFrames <- Binary.get
        fromReplayWithoutFrames replayWithoutFrames

    put replay = do
        replayWithoutFrames <- toReplayWithoutFrames replay
        Binary.put replayWithoutFrames

instance DeepSeq.NFData ReplayWithFrames where


-- | Converts a 'ReplayWithoutFrames.ReplayWithoutFrames' into a 'ReplayWithFrames'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
fromReplayWithoutFrames :: (Monad m) => ReplayWithoutFrames.ReplayWithoutFrames -> m ReplayWithFrames
fromReplayWithoutFrames replayWithoutFrames = do
    pure (ReplayWithFrames
        (#version1 replayWithoutFrames)
        (#version2 replayWithoutFrames)
        (#label replayWithoutFrames)
        (#properties replayWithoutFrames)
        (#levels replayWithoutFrames)
        (#keyFrames replayWithoutFrames)
        (Parser.parseStream replayWithoutFrames)
        (#messages replayWithoutFrames)
        (#marks replayWithoutFrames)
        (#packages replayWithoutFrames)
        (#objects replayWithoutFrames)
        (#names replayWithoutFrames)
        (#classes replayWithoutFrames)
        (#cache replayWithoutFrames))


-- | Converts a 'ReplayWithFrames' into a 'ReplayWithoutFrames.ReplayWithoutFrames'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
toReplayWithoutFrames :: (Monad m) => ReplayWithFrames -> m ReplayWithoutFrames.ReplayWithoutFrames
toReplayWithoutFrames replayWithFrames = do
    let stream = Generator.generateStream
            (#frames replayWithFrames)
            (#objects replayWithFrames)
            (#names replayWithFrames)
            (#classes replayWithFrames)
            (#cache replayWithFrames)
    pure (ReplayWithoutFrames.ReplayWithoutFrames
        (#version1 replayWithFrames)
        (#version2 replayWithFrames)
        (#label replayWithFrames)
        (#properties replayWithFrames)
        (#levels replayWithFrames)
        (#keyFrames replayWithFrames)
        stream
        (#messages replayWithFrames)
        (#marks replayWithFrames)
        (#packages replayWithFrames)
        (#objects replayWithFrames)
        (#names replayWithFrames)
        (#classes replayWithFrames)
        (#cache replayWithFrames))
