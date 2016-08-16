module Octane.Type.ReplayWithFrames (ReplayWithFrames(..), fromReplayWithoutFrames, toReplayWithoutFrames) where

import Basics

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
    pure ReplayWithFrames
        { replayWithFramesVersion1 = replayWithoutFrames & #version1
        , replayWithFramesVersion2 = replayWithoutFrames & #version2
        , replayWithFramesLabel = replayWithoutFrames & #label
        , replayWithFramesProperties = replayWithoutFrames & #properties
        , replayWithFramesLevels = replayWithoutFrames & #levels
        , replayWithFramesKeyFrames = replayWithoutFrames & #keyFrames
        , replayWithFramesFrames = replayWithoutFrames & Parser.parseStream
        , replayWithFramesMessages = replayWithoutFrames & #messages
        , replayWithFramesMarks = replayWithoutFrames & #marks
        , replayWithFramesPackages = replayWithoutFrames & #packages
        , replayWithFramesObjects = replayWithoutFrames & #objects
        , replayWithFramesNames = replayWithoutFrames & #names
        , replayWithFramesClasses = replayWithoutFrames & #classes
        , replayWithFramesCache = replayWithoutFrames & #cache
        }


-- | Converts a 'ReplayWithFrames' into a 'ReplayWithoutFrames.ReplayWithoutFrames'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
toReplayWithoutFrames :: (Monad m) => ReplayWithFrames -> m ReplayWithoutFrames.ReplayWithoutFrames
toReplayWithoutFrames replayWithFrames = do
    pure ReplayWithoutFrames.ReplayWithoutFrames
        { ReplayWithoutFrames.replayWithoutFramesVersion1 = replayWithFrames & #version1
        , ReplayWithoutFrames.replayWithoutFramesVersion2 = replayWithFrames & #version2
        , ReplayWithoutFrames.replayWithoutFramesLabel = replayWithFrames & #label
        , ReplayWithoutFrames.replayWithoutFramesProperties = replayWithFrames & #properties
        , ReplayWithoutFrames.replayWithoutFramesLevels = replayWithFrames & #levels
        , ReplayWithoutFrames.replayWithoutFramesKeyFrames = replayWithFrames & #keyFrames
        , ReplayWithoutFrames.replayWithoutFramesStream = Generator.generateStream
            (replayWithFrames & #frames)
            (replayWithFrames & #objects)
            (replayWithFrames & #names)
            (replayWithFrames & #classes)
            (replayWithFrames & #cache)
        , ReplayWithoutFrames.replayWithoutFramesMessages = replayWithFrames & #messages
        , ReplayWithoutFrames.replayWithoutFramesMarks = replayWithFrames & #marks
        , ReplayWithoutFrames.replayWithoutFramesPackages = replayWithFrames & #packages
        , ReplayWithoutFrames.replayWithoutFramesObjects = replayWithFrames & #objects
        , ReplayWithoutFrames.replayWithoutFramesNames = replayWithFrames & #names
        , ReplayWithoutFrames.replayWithoutFramesClasses = replayWithFrames & #classes
        , ReplayWithoutFrames.replayWithoutFramesCache = replayWithFrames & #cache
        }
