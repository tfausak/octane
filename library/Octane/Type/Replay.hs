{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromReplayWithoutFrames, toReplayWithoutFrames) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.ReplayWithoutFrames as ReplayWithoutFrames
import qualified Octane.Type.Word32 as Word32


data Replay = Replay
    { version :: Version.Version
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Replay where
    get = do
        replayWithoutFrames <- Binary.get
        fromReplayWithoutFrames replayWithoutFrames

    put replay = do
        replayWithoutFrames <- toReplayWithoutFrames replay
        Binary.put replayWithoutFrames

instance Aeson.FromJSON Replay where
    parseJSON json = case json of
        Aeson.Object _object -> pure Replay { version = Version.makeVersion [1, 2] }
        _ -> Aeson.typeMismatch "Replay" json

instance DeepSeq.NFData Replay where

instance Aeson.ToJSON Replay where
    toJSON _replay = Aeson.object []


fromReplayWithoutFrames :: (Monad m) => ReplayWithoutFrames.ReplayWithoutFrames -> m Replay
fromReplayWithoutFrames replayWithoutFrames = do
    let version = Version.makeVersion (map Word32.fromWord32
            [ ReplayWithoutFrames.version1 replayWithoutFrames
            , ReplayWithoutFrames.version2 replayWithoutFrames
            ])

    pure Replay { .. }


toReplayWithoutFrames :: (Monad m) => Replay -> m ReplayWithoutFrames.ReplayWithoutFrames
toReplayWithoutFrames replay = do
    let [version1, version2] = map Word32.toWord32
            (Version.versionBranch (version replay))
    let label = "TAGame.Replay_Soccar_TA"

    pure ReplayWithoutFrames.ReplayWithoutFrames { .. }
