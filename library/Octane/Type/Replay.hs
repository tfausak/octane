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
import qualified Data.Bits as Bits
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Int8 as Int8
import qualified Octane.Type.List as List
import qualified Octane.Type.Property as Property
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Vector as Vector
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
  put = undefined

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
  let header = replay & Rattletrap.replayHeader & Rattletrap.sectionBody
      fromWord32 x = x & Rattletrap.word32Value & fromIntegral
      content = replay & Rattletrap.replayContent & Rattletrap.sectionBody
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
      frames =
        content & Rattletrap.contentFrames & zip [0 ..] &
        map (toFrame Map.empty)
  in Replay
     { replayVersion = version
     , replayMetadata = metadata
     , replayLevels = levels
     , replayMessages = messages
     , replayTickMarks = tickMarks
     , replayPackages = packages
     , replayFrames = frames
     }

toProperty :: Rattletrap.Property -> Property.Property
toProperty property =
  let size =
        property & Rattletrap.propertySize & Rattletrap.word64Value &
        Word64.Word64
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
         let content = toFloat32 x
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

type ActorMap = Map.Map () ()

toFrame :: ActorMap -> (Word, Rattletrap.Frame) -> Frame.Frame
toFrame actorMap (number, frame) =
  Frame.Frame
  { Frame.frameNumber = number
  , Frame.frameIsKeyFrame = number == 0
  , Frame.frameTime = frame & Rattletrap.frameTime & toFloat32
  , Frame.frameDelta = frame & Rattletrap.frameDelta & toFloat32
  , Frame.frameReplications =
      frame & Rattletrap.frameReplications & map (toReplication actorMap)
  }

toFloat32 :: Rattletrap.Float32 -> Float32.Float32
toFloat32 float32 = float32 & Rattletrap.float32Value & Float32.Float32

toReplication :: ActorMap -> Rattletrap.Replication -> Replication.Replication
toReplication _actorMap replication =
  Replication.Replication
  { Replication.replicationActorId =
      replication & Rattletrap.replicationActorId & toCompressedWord
  , Replication.replicationObjectName =
      case Rattletrap.replicationValue replication of
        Rattletrap.SpawnedReplicationValue spawned ->
          spawned & Rattletrap.spawnedReplication_objectName &
          Rattletrap.textToString &
          StrictText.pack
        _ -> "" -- TODO
  , Replication.replicationClassName =
      case Rattletrap.replicationValue replication of
        Rattletrap.SpawnedReplicationValue spawned ->
          spawned & Rattletrap.spawnedReplication_className &
          Rattletrap.textToString &
          StrictText.pack
        _ -> "" -- TODO
  , Replication.replicationState =
      case Rattletrap.replicationValue replication of
        Rattletrap.SpawnedReplicationValue _ -> State.Opening
        Rattletrap.UpdatedReplicationValue _ -> State.Existing
        Rattletrap.DestroyedReplicationValue _ -> State.Closing
  , Replication.replicationInitialization =
      case Rattletrap.replicationValue replication of
        Rattletrap.SpawnedReplicationValue value ->
          Just
            Initialization.Initialization
            { Initialization.initializationLocation =
                value & Rattletrap.spawnedReplicationInitialization &
                Rattletrap.initializationLocation &
                fmap toIntVector
            , Initialization.initializationRotation =
                value & Rattletrap.spawnedReplicationInitialization &
                Rattletrap.initializationRotation &
                fmap toInt8Vector
            }
        _ -> Nothing
  , Replication.replicationProperties = Map.empty -- TODO
  }

toCompressedWord :: Rattletrap.CompressedWord -> CompressedWord.CompressedWord
toCompressedWord compressedWord =
  CompressedWord.CompressedWord
  { CompressedWord.compressedWordLimit =
      compressedWord & Rattletrap.compressedWordLimit
  , CompressedWord.compressedWordValue =
      compressedWord & Rattletrap.compressedWordValue
  }

toIntVector :: Rattletrap.Vector -> Vector.Vector Int
toIntVector vector =
  let numBits =
        vector & Rattletrap.vectorBitSize & Rattletrap.compressedWordValue &
        fromIntegral
      bias = Bits.shiftL 1 (numBits + 1)
      dx =
        vector & Rattletrap.vectorDx & Rattletrap.compressedWordValue &
        fromIntegral
      dy =
        vector & Rattletrap.vectorDy & Rattletrap.compressedWordValue &
        fromIntegral
      dz =
        vector & Rattletrap.vectorDz & Rattletrap.compressedWordValue &
        fromIntegral
      x = dx - bias
      y = dy - bias
      z = dz - bias
  in Vector.Vector x y z

toInt8Vector :: Rattletrap.Int8Vector -> Vector.Vector Int8.Int8
toInt8Vector vector =
  let convert a = a & fmap Rattletrap.int8Value & Maybe.fromMaybe 0 & Int8.Int8
      x = vector & Rattletrap.int8VectorX & convert
      y = vector & Rattletrap.int8VectorY & convert
      z = vector & Rattletrap.int8VectorZ & convert
  in Vector.Vector x y z
