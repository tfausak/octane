{-# LANGUAGE DataKinds #-}
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

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
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
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Value as Value
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word16 as Word16
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Type.Word8 as Word8
import qualified Octane.Utility.Endian as Endian
import qualified Octane.Utility.Optimizer as Optimizer
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
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Replay)

instance Binary.Binary Replay where
  get = do
    rawReplay <- Rattletrap.getReplay
    let replay = fromRawReplay rawReplay
    pure replay
  put = undefined

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
      (frames, _actorMap) =
        content & Rattletrap.contentFrames & zip [0 ..] &
        foldl
          (\(fs, m) f ->
             let (f', m') = toFrame m f
             in (f' : fs, m'))
          ([], Map.empty)
  in Replay
     { replayVersion = version
     , replayMetadata = metadata
     , replayLevels = levels
     , replayMessages = messages
     , replayTickMarks = tickMarks
     , replayPackages = packages
     , replayFrames = frames & reverse & Optimizer.optimizeFrames
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

type ActorMap = Map.Map CompressedWord.CompressedWord (StrictText.Text, StrictText.Text)

toFrame :: ActorMap -> (Word, Rattletrap.Frame) -> (Frame.Frame, ActorMap)
toFrame actorMap (number, frame) =
  let (replications, newActorMap) =
        frame & Rattletrap.frameReplications &
        foldl
          (\(rs, m) r ->
             let (r', m') = toReplication m r
             in (r' : rs, m'))
          ([], actorMap)
      newFrame =
        Frame.Frame
        { Frame.frameNumber = number
        , Frame.frameIsKeyFrame = number == 0
        , Frame.frameTime = frame & Rattletrap.frameTime & toFloat32
        , Frame.frameDelta = frame & Rattletrap.frameDelta & toFloat32
        , Frame.frameReplications = reverse replications
        }
  in (newFrame, newActorMap)

toFloat32 :: Rattletrap.Float32 -> Float32.Float32
toFloat32 float32 = float32 & Rattletrap.float32Value & Float32.Float32

toReplication :: ActorMap
              -> Rattletrap.Replication
              -> (Replication.Replication, ActorMap)
toReplication actorMap replication =
  let actorId = replication & Rattletrap.replicationActorId & toCompressedWord
      replicationValue = Rattletrap.replicationValue replication
      maybeNames =
        case replicationValue of
          Rattletrap.SpawnedReplicationValue spawned ->
            Just
              ( spawned & Rattletrap.spawnedReplication_objectName &
                Rattletrap.textToString &
                StrictText.pack
              , spawned & Rattletrap.spawnedReplication_className &
                Rattletrap.textToString &
                StrictText.pack)
          _ -> Nothing
      newActorMap =
        case maybeNames of
          Nothing -> actorMap
          Just names -> Map.insert actorId names actorMap
      objectName =
        newActorMap & Map.lookup actorId & fmap fst & Maybe.fromMaybe "unknown"
      className =
        newActorMap & Map.lookup actorId & fmap snd & Maybe.fromMaybe "unknown"
      newReplication =
        Replication.Replication
        { Replication.replicationActorId = actorId
        , Replication.replicationObjectName = objectName
        , Replication.replicationClassName = className
        , Replication.replicationState =
            case replicationValue of
              Rattletrap.SpawnedReplicationValue _ -> State.Opening
              Rattletrap.UpdatedReplicationValue _ -> State.Existing
              Rattletrap.DestroyedReplicationValue _ -> State.Closing
        , Replication.replicationInitialization =
            case replicationValue of
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
        , Replication.replicationProperties =
            case replicationValue of
              Rattletrap.UpdatedReplicationValue updated ->
                updated & Rattletrap.updatedReplicationAttributes & map toValue &
                Map.fromList
              _ -> Map.empty
        }
  in (newReplication, newActorMap)

toValue :: Rattletrap.Attribute -> (StrictText.Text, Value.Value)
toValue attribute =
  let key =
        attribute & Rattletrap.attribute_name & Rattletrap.textToString &
        StrictText.pack
      value =
        case Rattletrap.attributeValue attribute of
          Rattletrap.BooleanAttributeValue x ->
            Value.ValueBoolean
              (Value.BooleanValue
                 (Boolean.Boolean (Rattletrap.booleanAttributeValue x)))
          Rattletrap.ByteAttributeValue x ->
            Value.ValueByte
              (Value.ByteValue
                 (Word8.Word8
                    (Rattletrap.word8Value (Rattletrap.byteAttributeValue x))))
          Rattletrap.CamSettingsAttributeValue x ->
            Value.ValueCamSettings
              (Value.CamSettingsValue
                 (toFloat32 (Rattletrap.camSettingsAttributeFov x))
                 (toFloat32 (Rattletrap.camSettingsAttributeHeight x))
                 (toFloat32 (Rattletrap.camSettingsAttributeAngle x))
                 (toFloat32 (Rattletrap.camSettingsAttributeDistance x))
                 (toFloat32 (Rattletrap.camSettingsAttributeStiffness x))
                 (toFloat32 (Rattletrap.camSettingsAttributeSwivelSpeed x)))
          Rattletrap.ClubColorsAttributeValue x ->
            Value.ValueClubColors
              (Value.ClubColorsValue
                 (Boolean.Boolean (Rattletrap.clubColorsAttributeBlueFlag x))
                 (toWord8 (Rattletrap.clubColorsAttributeBlueColor x))
                 (Boolean.Boolean (Rattletrap.clubColorsAttributeOrangeFlag x))
                 (toWord8 (Rattletrap.clubColorsAttributeOrangeColor x)))
          Rattletrap.DemolishAttributeValue x ->
            Value.ValueDemolish
              (Value.DemolishValue
                 (Boolean.Boolean (Rattletrap.demolishAttributeAttackerFlag x))
                 (toWord32 (Rattletrap.demolishAttributeAttackerActorId x))
                 (Boolean.Boolean (Rattletrap.demolishAttributeVictimFlag x))
                 (toWord32 (Rattletrap.demolishAttributeVictimActorId x))
                 (toIntVector (Rattletrap.demolishAttributeAttackerVelocity x))
                 (toIntVector (Rattletrap.demolishAttributeVictimVelocity x)))
          Rattletrap.EnumAttributeValue x ->
            Value.ValueEnum
              (Value.EnumValue
                 (Word16.Word16 (Rattletrap.enumAttributeValue x))
                 (Boolean.Boolean False))
          Rattletrap.ExplosionAttributeValue x ->
            Value.ValueExplosion
              (Value.ExplosionValue
                 (Boolean.Boolean False)
                 (Just (toInt32 (Rattletrap.explosionAttributeActorId x)))
                 (toIntVector (Rattletrap.explosionAttributeLocation x)))
          Rattletrap.FlaggedIntAttributeValue x ->
            Value.ValueFlaggedInt
              (Value.FlaggedIntValue
                 (Boolean.Boolean (Rattletrap.flaggedIntAttributeFlag x))
                 (toInt32 (Rattletrap.flaggedIntAttributeInt x)))
          Rattletrap.FloatAttributeValue x ->
            Value.ValueFloat
              (Value.FloatValue (toFloat32 (Rattletrap.floatAttributeValue x)))
          Rattletrap.GameModeAttributeValue x ->
            Value.ValueGameMode
              (Value.GameModeValue
                 (Word8.Word8 (Rattletrap.gameModeAttributeWord x)))
          Rattletrap.IntAttributeValue x ->
            Value.ValueInt
              (Value.IntValue (toInt32 (Rattletrap.intAttributeValue x)))
          Rattletrap.LoadoutAttributeValue x -> Value.ValueLoadout (toLoadout x)
          Rattletrap.LoadoutOnlineAttributeValue x ->
            Value.ValueLoadoutOnline (toLoadoutOnline x)
          Rattletrap.LoadoutsAttributeValue x ->
            Value.ValueLoadouts
              (Value.LoadoutsValue
                 (toLoadout (Rattletrap.loadoutsAttributeBlue x))
                 (toLoadout (Rattletrap.loadoutsAttributeOrange x)))
          Rattletrap.LoadoutsOnlineAttributeValue x ->
            Value.ValueLoadoutsOnline
              (Value.LoadoutsOnlineValue
                 (toLoadoutOnline (Rattletrap.loadoutsOnlineAttributeBlue x))
                 (toLoadoutOnline (Rattletrap.loadoutsOnlineAttributeOrange x))
                 (Boolean.Boolean (Rattletrap.loadoutsOnlineAttributeUnknown1 x))
                 (Boolean.Boolean (Rattletrap.loadoutsOnlineAttributeUnknown2 x)))
          Rattletrap.LocationAttributeValue x ->
            Value.ValueLocation
              (Value.LocationValue
                 (toIntVector (Rattletrap.locationAttributeValue x)))
          Rattletrap.MusicStingerAttributeValue x ->
            Value.ValueMusicStinger
              (Value.MusicStingerValue
                 (Boolean.Boolean (Rattletrap.musicStingerAttributeFlag x))
                 (toWord32 (Rattletrap.musicStingerAttributeCue x))
                 (toWord8 (Rattletrap.musicStingerAttributeTrigger x)))
          Rattletrap.PartyLeaderAttributeValue x ->
            Value.ValueUniqueId
              (Value.UniqueIdValue
                 (toWord8 (Rattletrap.partyLeaderAttributeSystemId x))
                 (maybe
                    (RemoteId.RemoteSplitscreenId
                       (RemoteId.SplitscreenId Nothing))
                    (\(r, _) -> toRemoteId r)
                    (Rattletrap.partyLeaderAttributeId x))
                 (fmap (toWord8 . snd) (Rattletrap.partyLeaderAttributeId x)))
          Rattletrap.PickupAttributeValue x ->
            Value.ValuePickup
              (Value.PickupValue
                 (Boolean.Boolean
                    (maybe
                       False
                       (const True)
                       (Rattletrap.pickupAttributeInstigatorId x)))
                 (fmap toWord32 (Rattletrap.pickupAttributeInstigatorId x))
                 (Boolean.Boolean (Rattletrap.pickupAttributePickedUp x)))
          Rattletrap.PrivateMatchSettingsAttributeValue x ->
            Value.ValuePrivateMatchSettings
              (Value.PrivateMatchSettingsValue
                 (toText (Rattletrap.privateMatchSettingsAttributeMutators x))
                 (toWord32
                    (Rattletrap.privateMatchSettingsAttributeJoinableBy x))
                 (toWord32
                    (Rattletrap.privateMatchSettingsAttributeMaxPlayers x))
                 (toText (Rattletrap.privateMatchSettingsAttributeGameName x))
                 (toText (Rattletrap.privateMatchSettingsAttributePassword x))
                 (Boolean.Boolean
                    (Rattletrap.privateMatchSettingsAttributeFlag x)))
          Rattletrap.QWordAttributeValue x ->
            Value.ValueQWord
              (Value.QWordValue (toWord64 (Rattletrap.qWordAttributeValue x)))
          Rattletrap.ReservationAttributeValue x ->
            Value.ValueReservation
              (Value.ReservationValue
                 (toCompressedWord (Rattletrap.reservationAttributeNumber x))
                 (toWord8
                    (Rattletrap.uniqueIdAttributeSystemId
                       (Rattletrap.reservationAttributeUniqueId x)))
                 (toRemoteId
                    (Rattletrap.uniqueIdAttributeRemoteId
                       (Rattletrap.reservationAttributeUniqueId x)))
                 (Just
                    (toWord8
                       (Rattletrap.uniqueIdAttributeLocalId
                          (Rattletrap.reservationAttributeUniqueId x))))
                 (fmap toText (Rattletrap.reservationAttributeName x))
                 (Boolean.Boolean (Rattletrap.reservationAttributeUnknown1 x))
                 (Boolean.Boolean (Rattletrap.reservationAttributeUnknown2 x)))
          Rattletrap.RigidBodyStateAttributeValue x ->
            Value.ValueRigidBodyState
              (Value.RigidBodyStateValue
                 (Boolean.Boolean (Rattletrap.rigidBodyStateAttributeSleeping x))
                 (toIntVector (Rattletrap.rigidBodyStateAttributeLocation x))
                 (toFloatVector (Rattletrap.rigidBodyStateAttributeRotation x))
                 (fmap
                    toIntVector
                    (Rattletrap.rigidBodyStateAttributeLinearVelocity x))
                 (fmap
                    toIntVector
                    (Rattletrap.rigidBodyStateAttributeAngularVelocity x)))
          Rattletrap.StringAttributeValue x ->
            Value.ValueString
              (Value.StringValue (toText (Rattletrap.stringAttributeValue x)))
          Rattletrap.TeamPaintAttributeValue x ->
            Value.ValueTeamPaint
              (Value.TeamPaintValue
                 (toWord8 (Rattletrap.teamPaintAttributeTeam x))
                 (toWord8 (Rattletrap.teamPaintAttributePrimaryColor x))
                 (toWord8 (Rattletrap.teamPaintAttributeAccentColor x))
                 (toWord32 (Rattletrap.teamPaintAttributePrimaryFinish x))
                 (toWord32 (Rattletrap.teamPaintAttributeAccentFinish x)))
          Rattletrap.UniqueIdAttributeValue x ->
            Value.ValueUniqueId
              (Value.UniqueIdValue
                 (toWord8 (Rattletrap.uniqueIdAttributeSystemId x))
                 (toRemoteId (Rattletrap.uniqueIdAttributeRemoteId x))
                 (Just (toWord8 (Rattletrap.uniqueIdAttributeLocalId x))))
          Rattletrap.WeldedInfoAttributeValue x ->
            Value.ValueWeldedInfo
              (Value.WeldedInfoValue
                 (Boolean.Boolean (Rattletrap.weldedInfoAttributeActive x))
                 (toInt32 (Rattletrap.weldedInfoAttributeActorId x))
                 (toIntVector (Rattletrap.weldedInfoAttributeOffset x))
                 (toFloat32 (Rattletrap.weldedInfoAttributeMass x))
                 (toInt8Vector (Rattletrap.weldedInfoAttributeRotation x)))
  in (key, value)

toLoadout :: Rattletrap.LoadoutAttribute -> Value.LoadoutValue
toLoadout x =
  Value.LoadoutValue
    (toWord8 (Rattletrap.loadoutAttributeVersion x))
    (toWord32 (Rattletrap.loadoutAttributeBody x))
    (toWord32 (Rattletrap.loadoutAttributeDecal x))
    (toWord32 (Rattletrap.loadoutAttributeWheels x))
    (toWord32 (Rattletrap.loadoutAttributeRocketTrail x))
    (toWord32 (Rattletrap.loadoutAttributeAntenna x))
    (toWord32 (Rattletrap.loadoutAttributeTopper x))
    (toWord32 (Rattletrap.loadoutAttributeUnknown1 x))
    (fmap toWord32 (Rattletrap.loadoutAttributeUnknown2 x))

toLoadoutOnline :: Rattletrap.LoadoutOnlineAttribute -> Value.LoadoutOnlineValue
toLoadoutOnline x =
  Value.LoadoutOnlineValue
    (map
       (map (\(k, v) -> (toWord32 k, toCompressedWord v)))
       (Rattletrap.loadoutAttributeValue x))

toRemoteId :: Rattletrap.RemoteId -> RemoteId.RemoteId
toRemoteId remoteId =
  case remoteId of
    Rattletrap.PlayStationId x ->
      let (a, b) = splitAt 16 x
      in RemoteId.RemotePlayStationId
           (RemoteId.PlayStationId
              (a & LazyBytes.pack & LazyBytes.toStrict &
               Endian.reverseBitsInStrictBytes &
               Encoding.decodeLatin1 &
               StrictText.dropWhileEnd (== '\0') &
               Text.Text)
              (LazyBytes.pack b))
    Rattletrap.SplitscreenId _ ->
      RemoteId.RemoteSplitscreenId (RemoteId.SplitscreenId (Just 0))
    Rattletrap.SteamId x ->
      RemoteId.RemoteSteamId (RemoteId.SteamId (toWord64 x))
    Rattletrap.XboxId x -> RemoteId.RemoteXboxId (RemoteId.XboxId (toWord64 x))

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

toWord32 :: Rattletrap.Word32 -> Word32.Word32
toWord32 word32 = Word32.Word32 (Rattletrap.word32Value word32)

toInt32 :: Rattletrap.Int32 -> Int32.Int32
toInt32 int32 = Int32.Int32 (Rattletrap.int32Value int32)

toWord8 :: Rattletrap.Word8 -> Word8.Word8
toWord8 word8 = Word8.Word8 (Rattletrap.word8Value word8)

toWord64 :: Rattletrap.Word64 -> Word64.Word64
toWord64 word64 = Word64.Word64 (Rattletrap.word64Value word64)

toFloatVector :: Rattletrap.CompressedWordVector -> Vector.Vector Float
toFloatVector vector =
  Vector.Vector
    (toFloat (Rattletrap.compressedWordVectorX vector))
    (toFloat (Rattletrap.compressedWordVectorY vector))
    (toFloat (Rattletrap.compressedWordVectorZ vector))

toFloat :: Rattletrap.CompressedWord -> Float
toFloat compressedWord =
  let serIntMax = Rattletrap.compressedWordLimit compressedWord
      numBits = ceiling (log (fromIntegral serIntMax :: Float) / log 2)
      bias = Bits.shiftL 1 (numBits - 1)
      delta = fromIntegral (Rattletrap.compressedWordValue compressedWord)
      unscaledValue = (delta :: Int) - bias
      maxBitValue = (Bits.shiftL 1 (numBits - 1)) - 1
      maxValue = 1 :: Int
      invScale =
        if maxValue > maxBitValue
          then fromIntegral maxValue / fromIntegral maxBitValue
          else 1 / (fromIntegral maxBitValue / fromIntegral maxValue)
  in fromIntegral unscaledValue * invScale
