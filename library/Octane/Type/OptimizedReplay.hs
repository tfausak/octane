{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.OptimizedReplay
    ( OptimizedReplay(..)
    , fromReplayWithFrames
    , toReplayWithFrames
    ) where

import Data.Function ((&))

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
import qualified Octane.Type.ReplayWithFrames as ReplayWithFrames
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Utility.Optimizer as Optimizer


-- | A fully-processed replay with optimized frames. That means any unnecessary
-- replications have been removed.
--
-- See 'Octane.Type.Replay.Replay'.
data OptimizedReplay = OptimizedReplay
    { optimizedReplayVersion1 :: Word32.Word32
    , optimizedReplayVersion2 :: Word32.Word32
    , optimizedReplayLabel :: Text.Text
    , optimizedReplayProperties :: Dictionary.Dictionary Property.Property
    , optimizedReplayLevels :: List.List Text.Text
    , optimizedReplayKeyFrames :: List.List KeyFrame.KeyFrame
    , optimizedReplayFrames :: [Frame.Frame]
    , optimizedReplayMessages :: List.List Message.Message
    , optimizedReplayMarks :: List.List Mark.Mark
    , optimizedReplayPackages :: List.List Text.Text
    , optimizedReplayObjects :: List.List Text.Text
    , optimizedReplayNames :: List.List Text.Text
    , optimizedReplayClasses :: List.List ClassItem.ClassItem
    , optimizedReplayCache :: List.List CacheItem.CacheItem
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''OptimizedReplay)

instance Binary.Binary OptimizedReplay where
    get = do
        replayWithFrames <- Binary.get
        fromReplayWithFrames replayWithFrames

    put replay = do
        replayWithFrames <- toReplayWithFrames replay
        Binary.put replayWithFrames

instance DeepSeq.NFData OptimizedReplay where


-- | Converts a 'ReplayWithFrames.ReplayWithFrames' into an 'OptimizedReplay'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
fromReplayWithFrames :: (Monad m) => ReplayWithFrames.ReplayWithFrames -> m OptimizedReplay
fromReplayWithFrames replayWithFrames = do
    pure OptimizedReplay
        { optimizedReplayVersion1 = replayWithFrames & ReplayWithFrames.version1
        , optimizedReplayVersion2 = replayWithFrames & ReplayWithFrames.version2
        , optimizedReplayLabel = replayWithFrames & ReplayWithFrames.label
        , optimizedReplayProperties = replayWithFrames & ReplayWithFrames.properties
        , optimizedReplayLevels = replayWithFrames & ReplayWithFrames.levels
        , optimizedReplayKeyFrames = replayWithFrames & ReplayWithFrames.keyFrames
        , optimizedReplayFrames = replayWithFrames & ReplayWithFrames.frames & Optimizer.optimizeFrames
        , optimizedReplayMessages = replayWithFrames & ReplayWithFrames.messages
        , optimizedReplayMarks = replayWithFrames & ReplayWithFrames.marks
        , optimizedReplayPackages = replayWithFrames & ReplayWithFrames.packages
        , optimizedReplayObjects = replayWithFrames & ReplayWithFrames.objects
        , optimizedReplayNames = replayWithFrames & ReplayWithFrames.names
        , optimizedReplayClasses = replayWithFrames & ReplayWithFrames.classes
        , optimizedReplayCache = replayWithFrames & ReplayWithFrames.cache
        }


-- | Converts an 'OptimizedReplay' into a 'ReplayWithFrames.ReplayWithFrames'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
toReplayWithFrames :: (Monad m) => OptimizedReplay -> m ReplayWithFrames.ReplayWithFrames
toReplayWithFrames optimizedReplay = do
    pure ReplayWithFrames.ReplayWithFrames
        { ReplayWithFrames.version1 = optimizedReplay & #version1
        , ReplayWithFrames.version2 = optimizedReplay & #version2
        , ReplayWithFrames.label = optimizedReplay & #label
        , ReplayWithFrames.properties = optimizedReplay & #properties
        , ReplayWithFrames.levels = optimizedReplay & #levels
        , ReplayWithFrames.keyFrames = optimizedReplay & #keyFrames
        , ReplayWithFrames.frames = optimizedReplay & #frames
        , ReplayWithFrames.messages = optimizedReplay & #messages
        , ReplayWithFrames.marks = optimizedReplay & #marks
        , ReplayWithFrames.packages = optimizedReplay & #packages
        , ReplayWithFrames.objects = optimizedReplay & #objects
        , ReplayWithFrames.names = optimizedReplay & #names
        , ReplayWithFrames.classes = optimizedReplay & #classes
        , ReplayWithFrames.cache = optimizedReplay & #cache
        }
