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
import qualified Data.Maybe as Maybe
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.List as List
import qualified Octane.Type.Property as Property
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64
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
fromRawReplay replay =
  let header = Rattletrap.replayHeader replay
      fromWord32 x = x & Rattletrap.word32Value & fromIntegral
      content = Rattletrap.replayContent replay
      version =
        Version.makeVersion
          [ header & Rattletrap.headerEngineVersion & fromWord32
          , header & Rattletrap.headerLicenseeVersion & fromWord32
          ]
      metadata =
        header & Rattletrap.headerProperties & Rattletrap.dictionaryValue &
        Maybe.mapMaybe
          (\(key, maybeValue) ->
              case maybeValue of
                Nothing -> Nothing
                Just value ->
                  Just
                    ( key & Rattletrap.textToString & StrictText.pack
                    , toProperty value)) &
        Map.fromList
      levels =
        content & Rattletrap.contentLevels & Rattletrap.listValue &
        map Rattletrap.textToString &
        map StrictText.pack
      messages =
        content & Rattletrap.contentMessages & Rattletrap.listValue &
        map
          (\message ->
              ( message & Rattletrap.messageFrame & Rattletrap.word32Value & show &
                StrictText.pack
              , message & Rattletrap.messageValue & Rattletrap.textToString &
                StrictText.pack)) &
        Map.fromList
      tickMarks =
        content & Rattletrap.contentMarks & Rattletrap.listValue &
        map
          (\mark ->
              ( mark & Rattletrap.markFrame & Rattletrap.word32Value & show &
                StrictText.pack
              , mark & Rattletrap.markValue & Rattletrap.textToString &
                StrictText.pack)) &
        Map.fromList
      packages =
        content & Rattletrap.contentPackages & Rattletrap.listValue &
        map Rattletrap.textToString &
        map StrictText.pack
      frames = [] -- TODO
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
        , Rattletrap.headerLabel =
          Rattletrap.stringToText "TAGame.Replay_Soccar_TA"
        , Rattletrap.headerProperties = Rattletrap.Dictionary [] -- TODO
        }
      content =
        Rattletrap.Content
        { Rattletrap.contentLevels = Rattletrap.List [] -- TODO
        , Rattletrap.contentKeyFrames = Rattletrap.List []
        , Rattletrap.contentStreamSize = Rattletrap.Word32 0 -- TODO
        , Rattletrap.contentFrames = [] -- TODO
        , Rattletrap.contentTrailingBits = []
        , Rattletrap.contentMessages = Rattletrap.List [] -- TODO
        , Rattletrap.contentMarks = Rattletrap.List [] -- TODO
        , Rattletrap.contentPackages = Rattletrap.List [] -- TODO
        , Rattletrap.contentObjects = Rattletrap.List [] -- TODO
        , Rattletrap.contentNames = Rattletrap.List []
        , Rattletrap.contentClassMappings = Rattletrap.List [] -- TODO
        , Rattletrap.contentCaches = Rattletrap.List [] -- TODO
        }
  in Rattletrap.Replay
     { Rattletrap.replayHeader = header
     , Rattletrap.replayContent = content
     }

toProperty :: Rattletrap.Property -> Property.Property
toProperty property =
  let size =
        property & Rattletrap.propertySize & Rattletrap.word64Value & Word64.Word64
  in case Rattletrap.propertyValue property of
       Rattletrap.ArrayProperty x ->
         let content =
               x & Rattletrap.listValue &
               map
                 (\y ->
                     y & Rattletrap.dictionaryValue &
                     Maybe.mapMaybe
                       (\(k, mv) ->
                           case mv of
                             Nothing -> Nothing
                             Just v -> Just (toText k, toProperty v)) &
                     Map.fromList &
                     Dictionary.Dictionary) &
               List.List
         in Property.PropertyArray (Property.ArrayProperty size content)
       Rattletrap.BoolProperty x ->
         let content = x & Rattletrap.word8Value & (/= 0) & Boolean.Boolean
         in Property.PropertyBool (Property.BoolProperty size content)
       Rattletrap.ByteProperty k mv ->
         let (key, value) =
               case mv of
                 Nothing -> ("OnlinePlatform", toText k)
                 Just v -> (toText k, toText v)
         in Property.PropertyByte (Property.ByteProperty size key value)
       Rattletrap.FloatProperty x ->
         let content = x & Rattletrap.float32Value & Float32.Float32
         in Property.PropertyFloat (Property.FloatProperty size content)
       Rattletrap.IntProperty x ->
         let content = x & Rattletrap.int32Value & Int32.Int32
         in Property.PropertyInt (Property.IntProperty size content)
       Rattletrap.NameProperty x ->
         let content = toText x
         in Property.PropertyName (Property.NameProperty size content)
       Rattletrap.QWordProperty x ->
         let content = x & Rattletrap.word64Value & Word64.Word64
         in Property.PropertyQWord (Property.QWordProperty size content)
       Rattletrap.StrProperty x ->
         let content = toText x
         in Property.PropertyStr (Property.StrProperty size content)

toText :: Rattletrap.Text -> Text.Text
toText text = text & Rattletrap.textToString & StrictText.pack & Text.Text
