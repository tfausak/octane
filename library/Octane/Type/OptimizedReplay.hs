{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.OptimizedReplay (OptimizedReplay(..), fromReplayWithFrames, toReplayWithFrames) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
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


data OptimizedReplay = OptimizedReplay
    { version1 :: Word32.Word32
    , version2 :: Word32.Word32
    , label :: Text.Text
    , properties :: Dictionary.Dictionary Property.Property
    , levels :: List.List Text.Text
    , keyFrames :: List.List KeyFrame.KeyFrame
    , frames :: [Frame.Frame]
    , messages :: List.List Message.Message
    , marks :: List.List Mark.Mark
    , packages :: List.List Text.Text
    , objects :: List.List Text.Text
    , names :: List.List Text.Text
    , classes :: List.List ClassItem.ClassItem
    , cache :: List.List CacheItem.CacheItem
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary OptimizedReplay where
    get = do
        replayWithFrames <- Binary.get
        fromReplayWithFrames replayWithFrames

    put replay = do
        replayWithFrames <- toReplayWithFrames replay
        Binary.put replayWithFrames

instance DeepSeq.NFData OptimizedReplay where


fromReplayWithFrames :: (Monad m) => ReplayWithFrames.ReplayWithFrames -> m OptimizedReplay
fromReplayWithFrames replayWithFrames = do
    pure OptimizedReplay
        { version1 = replayWithFrames & ReplayWithFrames.version1
        , version2 = replayWithFrames & ReplayWithFrames.version2
        , label = replayWithFrames & ReplayWithFrames.label
        , properties = replayWithFrames & ReplayWithFrames.properties
        , levels = replayWithFrames & ReplayWithFrames.levels
        , keyFrames = replayWithFrames & ReplayWithFrames.keyFrames
        , frames = replayWithFrames & ReplayWithFrames.frames & optimizeFrames
        , messages = replayWithFrames & ReplayWithFrames.messages
        , marks = replayWithFrames & ReplayWithFrames.marks
        , packages = replayWithFrames & ReplayWithFrames.packages
        , objects = replayWithFrames & ReplayWithFrames.objects
        , names = replayWithFrames & ReplayWithFrames.names
        , classes = replayWithFrames & ReplayWithFrames.classes
        , cache = replayWithFrames & ReplayWithFrames.cache
        }


toReplayWithFrames :: (Monad m) => OptimizedReplay -> m ReplayWithFrames.ReplayWithFrames
toReplayWithFrames optimizedReplay = do
    pure ReplayWithFrames.ReplayWithFrames
        { ReplayWithFrames.version1 = optimizedReplay & version1
        , ReplayWithFrames.version2 = optimizedReplay & version2
        , ReplayWithFrames.label = optimizedReplay & label
        , ReplayWithFrames.properties = optimizedReplay & properties
        , ReplayWithFrames.levels = optimizedReplay & levels
        , ReplayWithFrames.keyFrames = optimizedReplay & keyFrames
        , ReplayWithFrames.frames = optimizedReplay & frames
        , ReplayWithFrames.messages = optimizedReplay & messages
        , ReplayWithFrames.marks = optimizedReplay & marks
        , ReplayWithFrames.packages = optimizedReplay & packages
        , ReplayWithFrames.objects = optimizedReplay & objects
        , ReplayWithFrames.names = optimizedReplay & names
        , ReplayWithFrames.classes = optimizedReplay & classes
        , ReplayWithFrames.cache = optimizedReplay & cache
        }


optimizeFrames :: [Frame.Frame] -> [Frame.Frame]
optimizeFrames = id -- TODO
