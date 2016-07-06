{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromOptimizedReplay, toOptimizedReplay) where

import Data.Aeson ((.=))
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
import qualified Octane.Type.KeyFrame as KeyFrame
import qualified Octane.Type.List as List
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.OptimizedReplay as OptimizedReplay
import qualified Octane.Type.Property as Property
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


-- | A fully-processed, optimized replay. This is the nicest format for humans
-- to work with. It can be converted all the way back down to a
-- 'Octane.Type.RawReplay.RawReplay' for serialization.
data Replay = Replay
    { version :: Version.Version
    , metadata :: Map.Map StrictText.Text Property.Property
    -- ^ High-level metadata about the replay. Only one key is actually
    -- required to be able to view the replay in Rocket League:
    --
    -- - MapName: This is a 'Property.NameProperty'. It is a case-insensitive
    --   map identifier, like @"Stadium_P"@.
    --
    -- There are many other properties that affect how the replay looks in the
    -- list of replays in Rocket League:
    --
    -- - Date: A 'Property.StrProperty' with the format @"YYYY-mm-dd:HH-MM"@.
    --   Dates are not validated, but the month must be between 1 and 12 to
    --   show up. The hour is shown modulo 12 with AM or PM.
    --
    -- - MatchType: A 'Property.NameProperty'. If this is not one of the
    --   expected values, nothing will be shown next to the replay's map. The
    --   expected values are: @"Online"@, @"Offline"@, @"Private"@, and
    --   @"Season"@.
    --
    -- - NumFrames: This 'Property.IntProperty' is used to calculate the length
    --   of the match. There are 30 frames per second, meaning @9000@ frames is
    --   a 5-minute match.
    --
    -- - PrimaryPlayerTeam: This is an 'Property.IntProperty'. It is either 0
    --   (blue) or 1 (orange). Any other value is ignored. If this would be 0,
    --   you don't have to set it at all.
    --
    -- - ReplayName: An optional 'Property.StrProperty' with a user-supplied
    --   name for the replay.
    --
    -- - Team0Score: The blue team's score as an 'Property.IntProperty'. Can be
    --   omitted if the score is 0.
    --
    -- - Team1Score: The orange team's score as an 'Property.IntProperty'. Can
    --   also be omitted if the score is 0.
    --
    -- - TeamSize: An 'Property.IntProperty' with the number of players per
    --   team. This value is not validated, so you can put absurd values like
    --   @99@. To get an "unfair" team size like 1v4, you must set the
    --   @"bUnfairBots"@ 'Property.BoolProperty' to @True@.
    , levels :: [StrictText.Text]
    , messages :: Map.Map StrictText.Text StrictText.Text
    , tickMarks :: Map.Map StrictText.Text StrictText.Text
    , packages :: [StrictText.Text]
    , frames :: [Frame.Frame]
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Replay where
    get = do
        optimizedReplay <- Binary.get
        fromOptimizedReplay optimizedReplay

    put replay = do
        optimizedReplay <- toOptimizedReplay replay
        Binary.put optimizedReplay

instance DeepSeq.NFData Replay where

instance Aeson.ToJSON Replay where
    toJSON replay = Aeson.object
        [ "Version" .= version replay
        , "Metadata" .= metadata replay
        , "Levels" .= levels replay
        , "Messages" .= messages replay
        , "TickMarks" .= tickMarks replay
        , "Packages" .= packages replay
        , "Frames" .= frames replay
        ]


-- | Converts an 'OptimizedReplay.OptimizedReplay' into a 'Replay'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
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
            & map (\ message -> do
                let key = message
                        & Message.frame
                        & Word32.unpack
                        & show
                        & StrictText.pack
                let value = message
                        & Message.content
                        & Text.unpack
                (key, value))
            & Map.fromList
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
        , packages = optimizedReplay
            & OptimizedReplay.packages
            & List.unpack
            & map Text.unpack
        , frames = optimizedReplay
            & OptimizedReplay.frames
        }


-- | Converts a 'Replay' into an 'OptimizedReplay.OptimizedReplay'.
-- Operates in a 'Monad' so that it can 'fail' somewhat gracefully.
toOptimizedReplay :: (Monad m) => Replay -> m OptimizedReplay.OptimizedReplay
toOptimizedReplay replay = do
    let [version1, version2] = replay
            & version
            & Version.versionBranch
            & map Word32.toWord32
    -- Key frames aren't important for replays. Mark the first frame as a key
    -- frame and the rest as regular frames.
    let frames_ = replay
            & frames
            & zip [0 :: Int ..]
            & map (\ (index, frame) -> frame { Frame.isKeyFrame = index == 0 })

    pure OptimizedReplay.OptimizedReplay
        { OptimizedReplay.version1 = version1
        , OptimizedReplay.version2 = version2
        , OptimizedReplay.label = "TAGame.Replay_Soccar_TA"
        , OptimizedReplay.properties = replay & metadata & Map.mapKeys Text.Text & Dictionary.Dictionary
        , OptimizedReplay.levels = replay & levels & map Text.Text & List.List
        , OptimizedReplay.keyFrames = frames_
            & filter Frame.isKeyFrame
            & map (\ frame -> KeyFrame.KeyFrame
                (Frame.time frame)
                (frame & Frame.number & Word32.toWord32)
                0)
            & List.List
        , OptimizedReplay.frames = frames_
        , OptimizedReplay.messages = replay
            & messages
            & Map.toList
            & map (\ (key, value) -> do
                let frame = key & StrictText.unpack & read & Word32.Word32
                let content = value & Text.Text
                Message.Message frame "" content)
            & List.List
        , OptimizedReplay.marks = replay
            & tickMarks
            & Map.toList
            & map (\ (key, value) -> do
                let label = value & Text.Text
                let frame = key & StrictText.unpack & read & Word32.Word32
                Mark.Mark label frame)
            & List.List
        , OptimizedReplay.packages = replay
            & packages
            & map Text.Text
            & List.List
        , OptimizedReplay.objects = List.List [] -- TODO
        , OptimizedReplay.names = List.List [] -- TODO
        , OptimizedReplay.classes = List.List [] -- TODO
        , OptimizedReplay.cache = List.List [] -- TODO
        }
