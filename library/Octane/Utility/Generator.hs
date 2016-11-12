{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Utility.Generator
  ( generateStream
  ) where

import Data.Function ((&))

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Tuple as Tuple
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.List as List
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Value as Value
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word16 as Word16
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word8 as Word8

data Context = Context
  { contextObjectMap :: Map.Map StrictText.Text Int32.Int32
    -- ^ { object name => object id }
  , contextClassPropertyMap :: Map.Map StrictText.Text (Map.Map StrictText.Text CompressedWord.CompressedWord)
    -- ^ { class name => { property name => property id } }
  }

$(OverloadedRecords.overloadedRecord Default.def ''Context)

-- | Generates a network stream.
generateStream
  :: [Frame.Frame]
  -> List.List Text.Text
  -> List.List Text.Text
  -> List.List ClassItem.ClassItem
  -> List.List CacheItem.CacheItem
  -> Stream.Stream
generateStream frames objects _names classes cache = do
  let context = makeContext objects classes cache
  let bitPut = putFrames context frames
  let bytePut = BinaryBit.runBitPut bitPut
  let bytes = Binary.runPut bytePut
  Stream.Stream bytes

makeContext
  :: List.List Text.Text
  -> List.List ClassItem.ClassItem
  -> List.List CacheItem.CacheItem
  -> Context
makeContext objects classes cache = do
  let objectMap =
        objects & #unpack & map #unpack & zip [0 ..] & map Tuple.swap &
        Map.fromList
  let classMap =
        classes & #unpack &
        map (\classItem -> (#streamId classItem, classItem & #name & #unpack)) &
        Map.fromList
  let classPropertyMap =
        cache & #unpack &
        map
          (\cacheItem -> do
             let className =
                   case Map.lookup (#classId cacheItem) classMap of
                     Nothing ->
                       error ("could not find class id for " ++ show className)
                     Just name -> name
             let maxPropertyId =
                   cacheItem & #properties & #unpack & map #streamId &
                   map Word32.fromWord32 &
                   (0 :) &
                   maximum
             let properties =
                   cacheItem & #properties & #unpack &
                   map
                     (\cacheProperty -> do
                        let propertyName =
                              case objectMap & Map.assocs & map Tuple.swap &
                                   Map.fromList &
                                   Map.lookup
                                     (cacheProperty & #objectId &
                                      Word32.fromWord32 &
                                      (\x -> x :: Int) &
                                      Int32.toInt32) of
                                Nothing ->
                                  error
                                    ("coult not find property name for " ++
                                     show cacheProperty)
                                Just name -> name
                        let propertyId =
                              cacheProperty & #streamId & Word32.fromWord32 &
                              CompressedWord.CompressedWord maxPropertyId
                        (propertyName, propertyId)) &
                   Map.fromList
             (className, properties)) &
        Map.fromList
  Context objectMap classPropertyMap

putFrames :: Context -> [Frame.Frame] -> BinaryBit.BitPut ()
putFrames context frames = do
  case frames of
    [] -> pure ()
    frame:rest -> do
      putFrame context frame
      putFrames context rest

putFrame :: Context -> Frame.Frame -> BinaryBit.BitPut ()
putFrame context frame = do
  frame & #time & BinaryBit.putBits 32
  frame & #delta & BinaryBit.putBits 32
  frame & #replications & putReplications context

putReplications :: Context -> [Replication.Replication] -> BinaryBit.BitPut ()
putReplications context replications = do
  case replications of
    [] -> do
      False & Boolean.Boolean & BinaryBit.putBits 1 -- no more replications
    replication:rest -> do
      True & Boolean.Boolean & BinaryBit.putBits 1 -- has replication
      putReplication context replication
      putReplications context rest

putReplication :: Context -> Replication.Replication -> BinaryBit.BitPut ()
putReplication context replication = do
  replication & #actorId & BinaryBit.putBits 0
  case #state replication of
    State.Opening -> putNewReplication context replication
    State.Existing -> putExistingReplication context replication
    State.Closing -> putClosedReplication

putNewReplication :: Context -> Replication.Replication -> BinaryBit.BitPut ()
putNewReplication context replication = do
  True & Boolean.Boolean & BinaryBit.putBits 1 -- open
  True & Boolean.Boolean & BinaryBit.putBits 1 -- new
  False & Boolean.Boolean & BinaryBit.putBits 1 -- unknown
  let objectName = #objectName replication
  case Map.lookup objectName (#objectMap context) of
    Nothing -> fail ("could not find object id for name " ++ show objectName)
    Just objectId -> BinaryBit.putBits 0 objectId
  case #initialization replication of
    Nothing -> pure ()
    Just x -> Initialization.putInitialization x

putExistingReplication :: Context
                       -> Replication.Replication
                       -> BinaryBit.BitPut ()
putExistingReplication context replication = do
  True & Boolean.Boolean & BinaryBit.putBits 1 -- open
  False & Boolean.Boolean & BinaryBit.putBits 1 -- existing
  let className = #className replication
  let properties = replication & #properties & Map.toAscList
  mapM_ (putProperty context className) properties
  False & Boolean.Boolean & BinaryBit.putBits 1 -- no more properties

putClosedReplication :: BinaryBit.BitPut ()
putClosedReplication = do
  False & Boolean.Boolean & BinaryBit.putBits 1 -- closed

putProperty
  :: Context
  -> StrictText.Text
  -> (StrictText.Text, Value.Value)
  -> BinaryBit.BitPut ()
putProperty context className (propertyName, value) = do
  True & Boolean.Boolean & BinaryBit.putBits 1 -- has property
  case Map.lookup className (#classPropertyMap context) of
    Nothing -> fail ("could not find properties for class " ++ show className)
    Just properties ->
      case Map.lookup propertyName properties of
        Nothing ->
          fail
            ("could not find property id for name " ++
             show propertyName ++ " in class " ++ show className)
        Just propertyId -> BinaryBit.putBits 0 propertyId
  putValue value

putValue :: Value.Value -> BinaryBit.BitPut ()
putValue value =
  case value of
    Value.ValueBoolean x -> putBooleanValue x
    Value.ValueByte x -> putByteValue x
    Value.ValueCamSettings x -> putCamSettingsValue x
    Value.ValueDemolish x -> putDemolishValue x
    Value.ValueEnum x -> putEnumValue x
    Value.ValueExplosion x -> putExplosionValue x
    Value.ValueFlaggedInt x -> putFlaggedIntValue x
    Value.ValueFloat x -> putFloatValue x
    Value.ValueGameMode x -> putGameModeValue x
    Value.ValueInt x -> putIntValue x
    Value.ValueLoadout x -> putLoadoutValue x
    Value.ValueLoadoutOnline x -> putLoadoutOnlineValue x
    Value.ValueLoadouts x -> putLoadoutsValue x
    Value.ValueLoadoutsOnline x -> putLoadoutsOnlineValue x
    Value.ValueLocation x -> putLocationValue x
    Value.ValueMusicStinger x -> putMusicStingerValue x
    Value.ValuePickup x -> putPickupValue x
    Value.ValuePrivateMatchSettings x -> putPrivateMatchSettingsValue x
    Value.ValueQWord x -> putQWordValue x
    Value.ValueRelativeRotation x -> putRelativeRotationValue x
    Value.ValueReservation x -> putReservationValue x
    Value.ValueRigidBodyState x -> putRigidBodyStateValue x
    Value.ValueString x -> putStringValue x
    Value.ValueTeamPaint x -> putTeamPaintValue x
    Value.ValueUniqueId x -> putUniqueIdValue x
    Value.ValueWeldedInfo x -> putWeldedInfoValue x
    _ -> undefined

putBooleanValue :: Value.BooleanValue -> BinaryBit.BitPut ()
putBooleanValue value = do
  value & #unpack & BinaryBit.putBits 0

putByteValue :: Value.ByteValue -> BinaryBit.BitPut ()
putByteValue value = do
  value & #unpack & BinaryBit.putBits 0

putCamSettingsValue :: Value.CamSettingsValue -> BinaryBit.BitPut ()
putCamSettingsValue value = do
  value & #fov & BinaryBit.putBits 0
  value & #height & BinaryBit.putBits 0
  value & #angle & BinaryBit.putBits 0
  value & #distance & BinaryBit.putBits 0
  value & #stiffness & BinaryBit.putBits 0
  value & #swivelSpeed & BinaryBit.putBits 0

putDemolishValue :: Value.DemolishValue -> BinaryBit.BitPut ()
putDemolishValue value = do
  value & #attackerFlag & BinaryBit.putBits 0
  value & #attackerActorId & BinaryBit.putBits 0
  value & #victimFlag & BinaryBit.putBits 0
  value & #victimActorId & BinaryBit.putBits 0
  value & #attackerVelocity & Vector.putIntVector
  value & #victimVelocity & Vector.putIntVector

putEnumValue :: Value.EnumValue -> BinaryBit.BitPut ()
putEnumValue value = do
  value & #value & Word16.fromWord16 & BinaryBit.putWord16be 10
  value & #flag & BinaryBit.putBits 0

putExplosionValue :: Value.ExplosionValue -> BinaryBit.BitPut ()
putExplosionValue value = do
  value & #actorless & BinaryBit.putBits 0
  value & #actorId & maybePutBits 0
  value & #position & Vector.putIntVector

putFlaggedIntValue :: Value.FlaggedIntValue -> BinaryBit.BitPut ()
putFlaggedIntValue value = do
  value & #flag & BinaryBit.putBits 0
  value & #int & BinaryBit.putBits 0

putFloatValue :: Value.FloatValue -> BinaryBit.BitPut ()
putFloatValue value = do
  value & #unpack & BinaryBit.putBits 0

putGameModeValue :: Value.GameModeValue -> BinaryBit.BitPut ()
putGameModeValue value
                 -- The parser only reads 2 bits for older replays. This generator only makes
                 -- replays that work with the latest version, so it always writes 8 bits.
 = do
  value & #unpack & BinaryBit.putBits 0

putIntValue :: Value.IntValue -> BinaryBit.BitPut ()
putIntValue value = do
  value & #unpack & BinaryBit.putBits 0

putLoadoutValue :: Value.LoadoutValue -> BinaryBit.BitPut ()
putLoadoutValue value = do
  value & #version & BinaryBit.putBits 0
  value & #body & BinaryBit.putBits 0
  value & #decal & BinaryBit.putBits 0
  value & #wheels & BinaryBit.putBits 0
  value & #rocketTrail & BinaryBit.putBits 0
  value & #antenna & BinaryBit.putBits 0
  value & #topper & BinaryBit.putBits 0
  value & #unknown1 & BinaryBit.putBits 0
  value & #unknown2 & maybePutBits 0

putLoadoutsValue :: Value.LoadoutsValue -> BinaryBit.BitPut ()
putLoadoutsValue value = do
  value & #loadout1 & putLoadoutValue
  value & #loadout2 & putLoadoutValue

putLoadoutOnlineValue :: Value.LoadoutOnlineValue -> BinaryBit.BitPut ()
putLoadoutOnlineValue value = do
  value & #unpack & length & Word8.toWord8 & BinaryBit.putBits 0
  Monad.forM_
    (#unpack value)
    (\tuples -> do
       tuples & length & Word8.toWord8 & BinaryBit.putBits 0
       Monad.forM_
         tuples
         (\(k, v) -> do
            BinaryBit.putBits 0 k
            BinaryBit.putBits 0 v))

putLoadoutsOnlineValue :: Value.LoadoutsOnlineValue -> BinaryBit.BitPut ()
putLoadoutsOnlineValue value = do
  value & #loadout1 & putLoadoutOnlineValue
  value & #loadout2 & putLoadoutOnlineValue
  value & #unknown1 & BinaryBit.putBits 0
  value & #unknown2 & BinaryBit.putBits 0

putLocationValue :: Value.LocationValue -> BinaryBit.BitPut ()
putLocationValue value = do
  value & #unpack & Vector.putIntVector

putMusicStingerValue :: Value.MusicStingerValue -> BinaryBit.BitPut ()
putMusicStingerValue value = do
  value & #flag & BinaryBit.putBits 0
  value & #cue & BinaryBit.putBits 0
  value & #trigger & BinaryBit.putBits 0

putPickupValue :: Value.PickupValue -> BinaryBit.BitPut ()
putPickupValue value = do
  value & #hasInstigator & BinaryBit.putBits 0
  value & #instigatorId & maybePutBits 0
  value & #pickedUp & BinaryBit.putBits 0

putPrivateMatchSettingsValue :: Value.PrivateMatchSettingsValue
                             -> BinaryBit.BitPut ()
putPrivateMatchSettingsValue value = do
  value & #mutators & BinaryBit.putBits 0
  value & #joinableBy & BinaryBit.putBits 0
  value & #maxPlayers & BinaryBit.putBits 0
  value & #gameName & BinaryBit.putBits 0
  value & #password & BinaryBit.putBits 0
  value & #flag & BinaryBit.putBits 0

putQWordValue :: Value.QWordValue -> BinaryBit.BitPut ()
putQWordValue value = do
  value & #unpack & BinaryBit.putBits 0

putRelativeRotationValue :: Value.RelativeRotationValue -> BinaryBit.BitPut ()
putRelativeRotationValue value = do
  value & #unpack & Vector.putFloatVector

putReservationValue :: Value.ReservationValue -> BinaryBit.BitPut ()
putReservationValue value = do
  value & #number & BinaryBit.putBits 0
  Value.UniqueIdValue (#systemId value) (#remoteId value) (#localId value) &
    putUniqueIdValue
  value & #playerName & maybePutBits 0
  value & #unknown1 & BinaryBit.putBits 0
  value & #unknown2 & BinaryBit.putBits 0
  -- The parser only reads 6 bits for newer replays. This generator only makes
  -- replays that work with the latest version, so it always writes 6 bits.
  BinaryBit.putWord8 6 0

putRigidBodyStateValue :: Value.RigidBodyStateValue -> BinaryBit.BitPut ()
putRigidBodyStateValue value = do
  value & #sleeping & BinaryBit.putBits 0
  value & #position & Vector.putIntVector
  value & #rotation & Vector.putFloatVector
  case #linearVelocity value of
    Nothing -> pure ()
    Just linearVelocity -> Vector.putIntVector linearVelocity
  case #angularVelocity value of
    Nothing -> pure ()
    Just angularVelocity -> Vector.putIntVector angularVelocity

putStringValue :: Value.StringValue -> BinaryBit.BitPut ()
putStringValue value = do
  value & #unpack & BinaryBit.putBits 0

putTeamPaintValue :: Value.TeamPaintValue -> BinaryBit.BitPut ()
putTeamPaintValue value = do
  value & #team & BinaryBit.putBits 0
  value & #primaryColor & BinaryBit.putBits 0
  value & #accentColor & BinaryBit.putBits 0
  value & #primaryFinish & BinaryBit.putBits 0
  value & #accentFinish & BinaryBit.putBits 0

putUniqueIdValue :: Value.UniqueIdValue -> BinaryBit.BitPut ()
putUniqueIdValue value = do
  value & #systemId & BinaryBit.putBits 0
  value & #remoteId & putRemoteId
  value & #localId & maybePutBits 0

putWeldedInfoValue :: Value.WeldedInfoValue -> BinaryBit.BitPut ()
putWeldedInfoValue value = do
  value & #active & BinaryBit.putBits 0
  value & #actorId & BinaryBit.putBits 0
  value & #offset & Vector.putIntVector
  value & #mass & BinaryBit.putBits 0
  value & #rotation & Vector.putInt8Vector

maybePutBits
  :: (BinaryBit.BinaryBit a)
  => Int -> Maybe a -> BinaryBit.BitPut ()
maybePutBits n mx =
  case mx of
    Nothing -> pure ()
    Just x -> BinaryBit.putBits n x

putRemoteId :: RemoteId.RemoteId -> BinaryBit.BitPut ()
putRemoteId remoteId =
  case remoteId of
    RemoteId.RemoteSplitscreenId x -> BinaryBit.putBits 0 x
    RemoteId.RemoteSteamId x -> BinaryBit.putBits 0 x
    RemoteId.RemotePlayStationId x -> BinaryBit.putBits 0 x
    RemoteId.RemoteXboxId x -> BinaryBit.putBits 0 x
