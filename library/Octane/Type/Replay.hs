{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromReplayWithFrames, toReplayWithFrames) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.List as List
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.Property as Property
import qualified Octane.Type.ReplayWithFrames as ReplayWithFrames
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


data Replay = Replay
    { version :: Version.Version
    , metadata :: Map.Map StrictText.Text Property.Property
    , levels :: [StrictText.Text]
    , messages :: [StrictText.Text]
    , tickMarks :: Map.Map StrictText.Text StrictText.Text
    , frames :: [Frame.Frame]
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Replay where
    get = do
        replayWithFrames <- Binary.get
        fromReplayWithFrames replayWithFrames

    put replay = do
        replayWithFrames <- toReplayWithFrames replay
        Binary.put replayWithFrames

instance Aeson.FromJSON Replay where

instance DeepSeq.NFData Replay where

instance Aeson.ToJSON Replay where


fromReplayWithFrames :: (Monad m) => ReplayWithFrames.ReplayWithFrames -> m Replay
fromReplayWithFrames replayWithFrames = do
    pure Replay
        { version =
            [ ReplayWithFrames.version1 replayWithFrames
            , ReplayWithFrames.version2 replayWithFrames
            ] & map Word32.fromWord32 & Version.makeVersion
        , metadata = replayWithFrames
            & ReplayWithFrames.properties
            & Dictionary.unpack
            & Map.mapKeys Text.unpack
        , levels = replayWithFrames
            & ReplayWithFrames.levels
            & List.unpack
            & map Text.unpack
        , messages = replayWithFrames
            & ReplayWithFrames.messages
            & List.unpack
            & map Message.content
            & map Text.unpack
        , tickMarks = replayWithFrames
            & ReplayWithFrames.marks
            & List.unpack
            & map (\ mark -> do
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
        , frames = replayWithFrames
            & ReplayWithFrames.frames
            & List.unpack
        }


toReplayWithFrames :: (Monad m) => Replay -> m ReplayWithFrames.ReplayWithFrames
toReplayWithFrames replay = do
    let [version1, version2] = replay
            & version
            & Version.versionBranch
            & map Word32.toWord32

    pure ReplayWithFrames.ReplayWithFrames
        { ReplayWithFrames.version1 = version1
        , ReplayWithFrames.version2 = version2
        , ReplayWithFrames.label = "TAGame.Replay_Soccar_TA"
        , ReplayWithFrames.properties = replay & metadata & Map.mapKeys Text.Text & Dictionary.Dictionary
        , ReplayWithFrames.levels = replay & levels & map Text.Text & List.List
        , ReplayWithFrames.keyFrames = List.List [] -- TODO
        , ReplayWithFrames.frames = replay & frames & List.List
        , ReplayWithFrames.messages = replay
            & messages
            & map (\ message -> message
                    & Text.Text
                    & Message.Message 0 "")
            & List.List
        , ReplayWithFrames.marks = replay
            & tickMarks
            & Map.toList
            & map (\ (key, value) -> do
                let label = value & Text.Text
                let frame = key & StrictText.unpack & read & Word32.Word32
                Mark.Mark label frame)
            & List.List
        , ReplayWithFrames.packages = List.List [] -- TODO
        , ReplayWithFrames.objects = List.List [] -- TODO
        , ReplayWithFrames.names = List.List [] -- TODO
        , ReplayWithFrames.classes = List.List [] -- TODO
        , ReplayWithFrames.cache = List.List [] -- TODO
        }
