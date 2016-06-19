{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromOptimizedReplay, toOptimizedReplay) where

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
import qualified Octane.Type.OptimizedReplay as OptimizedReplay
import qualified Octane.Type.Property as Property
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
        optimizedReplay <- Binary.get
        fromOptimizedReplay optimizedReplay

    put replay = do
        optimizedReplay <- toOptimizedReplay replay
        Binary.put optimizedReplay

instance Aeson.FromJSON Replay where

instance DeepSeq.NFData Replay where

instance Aeson.ToJSON Replay where


fromOptimizedReplay :: (Monad m) => OptimizedReplay.OptimizedReplay -> m Replay
fromOptimizedReplay optimizedReplay = do
    pure Replay
        { version =
            [ OptimizedReplay.version1 optimizedReplay
            , OptimizedReplay.version2 optimizedReplay
            ] & map Word32.fromWord32 & Version.makeVersion
        , metadata = optimizedReplay
            & OptimizedReplay.properties
            & Dictionary.unpack
            & Map.mapKeys Text.unpack
        , levels = optimizedReplay
            & OptimizedReplay.levels
            & List.unpack
            & map Text.unpack
        , messages = optimizedReplay
            & OptimizedReplay.messages
            & List.unpack
            & map Message.content
            & map Text.unpack
        , tickMarks = optimizedReplay
            & OptimizedReplay.marks
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
        , frames = optimizedReplay
            & OptimizedReplay.frames
        }


toOptimizedReplay :: (Monad m) => Replay -> m OptimizedReplay.OptimizedReplay
toOptimizedReplay replay = do
    let [version1, version2] = replay
            & version
            & Version.versionBranch
            & map Word32.toWord32

    pure OptimizedReplay.OptimizedReplay
        { OptimizedReplay.version1 = version1
        , OptimizedReplay.version2 = version2
        , OptimizedReplay.label = "TAGame.Replay_Soccar_TA"
        , OptimizedReplay.properties = replay & metadata & Map.mapKeys Text.Text & Dictionary.Dictionary
        , OptimizedReplay.levels = replay & levels & map Text.Text & List.List
        , OptimizedReplay.keyFrames = List.List [] -- TODO
        , OptimizedReplay.frames = replay & frames
        , OptimizedReplay.messages = replay
            & messages
            & map (\ message -> message
                    & Text.Text
                    & Message.Message 0 "")
            & List.List
        , OptimizedReplay.marks = replay
            & tickMarks
            & Map.toList
            & map (\ (key, value) -> do
                let label = value & Text.Text
                let frame = key & StrictText.unpack & read & Word32.Word32
                Mark.Mark label frame)
            & List.List
        , OptimizedReplay.packages = List.List [] -- TODO
        , OptimizedReplay.objects = List.List [] -- TODO
        , OptimizedReplay.names = List.List [] -- TODO
        , OptimizedReplay.classes = List.List [] -- TODO
        , OptimizedReplay.cache = List.List [] -- TODO
        }
