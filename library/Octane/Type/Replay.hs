{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromReplayWithoutFrames, toReplayWithoutFrames) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.List as List
import qualified Octane.Type.Property as Property
import qualified Octane.Type.ReplayWithoutFrames as ReplayWithoutFrames
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


data Replay = Replay
    { version :: Version.Version
    , metadata :: Map.Map StrictText.Text Property.Property
    , maps :: [StrictText.Text]
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
    let version = Version.makeVersion (map Word32.fromWord32
            [ ReplayWithoutFrames.version1 replayWithoutFrames
            , ReplayWithoutFrames.version2 replayWithoutFrames
            ])
    let metadata = Map.mapKeys Text.unpack (Dictionary.unpack (ReplayWithoutFrames.properties replayWithoutFrames))
    let maps = map Text.unpack (List.unpack (ReplayWithoutFrames.levels replayWithoutFrames))

    pure Replay { .. }


toReplayWithoutFrames :: (Monad m) => Replay -> m ReplayWithoutFrames.ReplayWithoutFrames
toReplayWithoutFrames replay = do
    let [version1, version2] = map Word32.toWord32
            (Version.versionBranch (version replay))
    let label = "TAGame.Replay_Soccar_TA"
    let properties = Dictionary.Dictionary (Map.mapKeys Text.Text (metadata replay))
    let levels = List.List (map Text.Text (maps replay))
    let keyFrames = List.List [] -- TODO

    pure ReplayWithoutFrames.ReplayWithoutFrames { .. }
