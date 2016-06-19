{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromReplayWithoutFrames, toReplayWithoutFrames) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.List as List
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.Property as Property
import qualified Octane.Type.ReplayWithoutFrames as ReplayWithoutFrames
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


data Replay = Replay
    { version :: Version.Version
    , metadata :: Map.Map StrictText.Text Property.Property
    , levels :: [StrictText.Text]
    , messages :: [StrictText.Text]
    , tickMarks :: Map.Map StrictText.Text StrictText.Text
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Replay where
    get = do
        replayWithoutFrames <- Binary.get
        fromReplayWithoutFrames replayWithoutFrames

    put replay = do
        replayWithoutFrames <- toReplayWithoutFrames replay
        Binary.put replayWithoutFrames

instance Aeson.FromJSON Replay where

instance DeepSeq.NFData Replay where

instance Aeson.ToJSON Replay where


fromReplayWithoutFrames :: (Monad m) => ReplayWithoutFrames.ReplayWithoutFrames -> m Replay
fromReplayWithoutFrames replayWithoutFrames = do
    pure Replay
        { version =
            [ ReplayWithoutFrames.version1 replayWithoutFrames
            , ReplayWithoutFrames.version2 replayWithoutFrames
            ] & map Word32.fromWord32 & Version.makeVersion
        , metadata = replayWithoutFrames
            & ReplayWithoutFrames.properties
            & Dictionary.unpack
            & Map.mapKeys Text.unpack
        , levels = replayWithoutFrames
            & ReplayWithoutFrames.levels
            & List.unpack
            & map Text.unpack
        , messages = replayWithoutFrames
            & ReplayWithoutFrames.messages
            & List.unpack
            & map Message.content
            & map Text.unpack
        , tickMarks = replayWithoutFrames
            & ReplayWithoutFrames.marks
            & List.unpack & map (\ mark -> do
                let key = mark
                        & Mark.frame
                        & Word32.unpack
                        & show
                        & StrictText.pack
                let value = mark
                        & Mark.label
                        & Text.unpack
                (key, value))
            & Map.fromList
        }


toReplayWithoutFrames :: (Monad m) => Replay -> m ReplayWithoutFrames.ReplayWithoutFrames
toReplayWithoutFrames replay = do
    let [version1, version2] = replay
            & version
            & Version.versionBranch
            & map Word32.toWord32

    pure ReplayWithoutFrames.ReplayWithoutFrames
        { ReplayWithoutFrames.version1 = version1
        , ReplayWithoutFrames.version2 = version2
        , ReplayWithoutFrames.label = "TAGame.Replay_Soccar_TA"
        , ReplayWithoutFrames.properties = replay & metadata & Map.mapKeys Text.Text & Dictionary.Dictionary
        , ReplayWithoutFrames.levels = replay & levels & map Text.Text & List.List
        , ReplayWithoutFrames.keyFrames = List.List [] -- TODO
        , ReplayWithoutFrames.stream = Stream.Stream LazyBytes.empty -- TODO
        , ReplayWithoutFrames.messages = replay
            & messages
            & map (\ message -> message
                    & Text.Text
                    & Message.Message 0 "")
            & List.List
        , ReplayWithoutFrames.marks = replay
            & tickMarks
            & Map.toList
            & map (\ (key, value) -> do
                let label = value & Text.Text
                let frame = key & StrictText.unpack & read & Word32.Word32
                Mark.Mark label frame)
            & List.List
        , ReplayWithoutFrames.packages = List.List [] -- TODO
        , ReplayWithoutFrames.objects = List.List [] -- TODO
        , ReplayWithoutFrames.names = List.List [] -- TODO
        , ReplayWithoutFrames.classes = List.List [] -- TODO
        , ReplayWithoutFrames.cache = List.List [] -- TODO
        }
