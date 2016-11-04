{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Replay
  ( Replay(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Property as Property
import qualified Rattletrap

-- | A fully-processed, optimized replay.
data Replay = Replay
  { replayVersion :: Version.Version
  , replayMetadata :: Map.Map StrictText.Text Property.Property
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
  , replayLevels :: [StrictText.Text]
  , replayMessages :: Map.Map StrictText.Text StrictText.Text
  , replayTickMarks :: Map.Map StrictText.Text StrictText.Text
  , replayPackages :: [StrictText.Text]
  , replayFrames :: [Frame.Frame]
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Replay)

instance Binary.Binary Replay where
  get = do
    rawReplay <- Rattletrap.getReplay
    let replay = fromRawReplay rawReplay
    pure replay
  put replay = do
    let rawReplay = toRawReplay replay
    Rattletrap.putReplay rawReplay

instance DeepSeq.NFData Replay

instance Aeson.ToJSON Replay where
  toJSON replay =
    Aeson.object
      [ "Version" .= #version replay
      , "Metadata" .= #metadata replay
      , "Levels" .= #levels replay
      , "Messages" .= #messages replay
      , "TickMarks" .= #tickMarks replay
      , "Packages" .= #packages replay
      , "Frames" .= #frames replay
      ]

fromRawReplay :: Rattletrap.Replay -> Replay
fromRawReplay rawReplay =
  let header = Rattletrap.replayHeader rawReplay
      fromWord32 x = x & Rattletrap.word32Value & fromIntegral
      version =
        Version.makeVersion
          [ header & Rattletrap.headerEngineVersion & fromWord32
          , header & Rattletrap.headerLicenseeVersion & fromWord32
          ]
      metadata = undefined
      levels = undefined
      messages = undefined
      tickMarks = undefined
      packages = undefined
      frames = undefined
  in Replay
     { replayVersion = version
     , replayMetadata = metadata
     , replayLevels = levels
     , replayMessages = messages
     , replayTickMarks = tickMarks
     , replayPackages = packages
     , replayFrames = frames
     }

toRawReplay :: Replay -> Rattletrap.Replay
toRawReplay replay =
  let toWord32 x = x & fromIntegral & Rattletrap.Word32
      [majorVersion, minorVersion] =
        replay & replayVersion & Version.versionBranch & map toWord32
      header =
        Rattletrap.Header
        { Rattletrap.headerEngineVersion = majorVersion
        , Rattletrap.headerLicenseeVersion = minorVersion
        , Rattletrap.headerLabel = undefined
        , Rattletrap.headerProperties = undefined
        }
      content =
        Rattletrap.Content
        { Rattletrap.contentLevels = undefined
        , Rattletrap.contentKeyFrames = undefined
        , Rattletrap.contentStreamSize = undefined
        , Rattletrap.contentFrames = undefined
        , Rattletrap.contentTrailingBits = undefined
        , Rattletrap.contentMessages = undefined
        , Rattletrap.contentMarks = undefined
        , Rattletrap.contentPackages = undefined
        , Rattletrap.contentObjects = undefined
        , Rattletrap.contentNames = undefined
        , Rattletrap.contentClassMappings = undefined
        , Rattletrap.contentCaches = undefined
        }
  in Rattletrap.Replay
     { Rattletrap.replayHeader = header
     , Rattletrap.replayContent = content
     }
