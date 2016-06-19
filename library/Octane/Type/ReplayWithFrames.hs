{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.ReplayWithFrames (ReplayWithFrames(..), fromReplayWithoutFrames, toReplayWithoutFrames) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LazyBytes
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
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


data ReplayWithFrames = ReplayWithFrames
    { version1 :: Word32.Word32
    , version2 :: Word32.Word32
    , label :: Text.Text
    , properties :: Dictionary.Dictionary Property.Property
    , levels :: List.List Text.Text
    , keyFrames :: List.List KeyFrame.KeyFrame
    , frames :: List.List Frame.Frame
    , messages :: List.List Message.Message
    , marks :: List.List Mark.Mark
    , packages :: List.List Text.Text
    , objects :: List.List Text.Text
    , names :: List.List Text.Text
    , classes :: List.List ClassItem.ClassItem
    , cache :: List.List CacheItem.CacheItem
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary ReplayWithFrames where
    get = do
        replayWithoutFrames <- Binary.get
        fromReplayWithoutFrames replayWithoutFrames

    put replay = do
        replayWithoutFrames <- toReplayWithoutFrames replay
        Binary.put replayWithoutFrames

instance DeepSeq.NFData ReplayWithFrames where


fromReplayWithoutFrames :: (Monad m) => ReplayWithoutFrames.ReplayWithoutFrames -> m ReplayWithFrames
fromReplayWithoutFrames replayWithoutFrames = do
    pure ReplayWithFrames
        { version1 = replayWithoutFrames & ReplayWithoutFrames.version1
        , version2 = replayWithoutFrames & ReplayWithoutFrames.version2
        , label = replayWithoutFrames & ReplayWithoutFrames.label
        , properties = replayWithoutFrames & ReplayWithoutFrames.properties
        , levels = replayWithoutFrames & ReplayWithoutFrames.levels
        , keyFrames = replayWithoutFrames & ReplayWithoutFrames.keyFrames
        , frames = List.List [] -- TODO
        , messages = replayWithoutFrames & ReplayWithoutFrames.messages
        , marks = replayWithoutFrames & ReplayWithoutFrames.marks
        , packages = replayWithoutFrames & ReplayWithoutFrames.packages
        , objects = replayWithoutFrames & ReplayWithoutFrames.objects
        , names = replayWithoutFrames & ReplayWithoutFrames.names
        , classes = replayWithoutFrames & ReplayWithoutFrames.classes
        , cache = replayWithoutFrames & ReplayWithoutFrames.cache
        }


toReplayWithoutFrames :: (Monad m) => ReplayWithFrames -> m ReplayWithoutFrames.ReplayWithoutFrames
toReplayWithoutFrames replayWithFrames = do
    pure ReplayWithoutFrames.ReplayWithoutFrames
        { ReplayWithoutFrames.version1 = replayWithFrames & version1
        , ReplayWithoutFrames.version2 = replayWithFrames & version2
        , ReplayWithoutFrames.label = replayWithFrames & label
        , ReplayWithoutFrames.properties = replayWithFrames & properties
        , ReplayWithoutFrames.levels = replayWithFrames & levels
        , ReplayWithoutFrames.keyFrames = replayWithFrames & keyFrames
        , ReplayWithoutFrames.stream = Stream.Stream LazyBytes.empty -- TODO
        , ReplayWithoutFrames.messages = replayWithFrames & messages
        , ReplayWithoutFrames.marks = replayWithFrames & marks
        , ReplayWithoutFrames.packages = replayWithFrames & packages
        , ReplayWithoutFrames.objects = replayWithFrames & objects
        , ReplayWithoutFrames.names = replayWithFrames & names
        , ReplayWithoutFrames.classes = replayWithFrames & classes
        , ReplayWithoutFrames.cache = replayWithFrames & cache
        }
