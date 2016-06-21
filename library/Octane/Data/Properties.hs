{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Properties
    ( booleanProperties
    , byteProperties
    , camSettingsProperties
    , demolishProperties
    , enumProperties
    , explosionProperties
    , flaggedIntProperties
    , floatProperties
    , gameModeProperties
    , intProperties
    , loadoutOnlineProperties
    , loadoutProperties
    , locationProperties
    , musicStingerProperties
    , partyLeaderProperties
    , pickupProperties
    , privateMatchSettingsProperties
    , qWordProperties
    , relativeRotationProperties
    , reservationProperties
    , rigidBodyStateProperties
    , stringProperties
    , teamPaintProperties
    , uniqueIdProperties
    ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as StrictText

-- $
-- >>> :set -XOverloadedStrings


-- | A set of properties that are booleans.
--
-- >>> Set.member "Engine.Actor:bBlockActors" booleanProperties
-- True
booleanProperties :: Set.Set StrictText.Text
booleanProperties = Map.findWithDefault Set.empty "boolean" properties


-- | A set of properties that are bytes.
--
-- >>> Set.member "Engine.PlayerReplicationInfo:Ping" byteProperties
-- True
byteProperties :: Set.Set StrictText.Text
byteProperties = Map.findWithDefault Set.empty "byte" properties


-- | A set of properties that are camera settings.
--
-- >>> Set.member "TAGame.CameraSettingsActor_TA:ProfileSettings" camSettingsProperties
-- True
camSettingsProperties :: Set.Set StrictText.Text
camSettingsProperties = Map.findWithDefault Set.empty "cam_settings" properties


-- | A set of properties that are demolitions.
--
-- >>> Set.member "TAGame.Car_TA:ReplicatedDemolish" demolishProperties
-- True
demolishProperties :: Set.Set StrictText.Text
demolishProperties = Map.findWithDefault Set.empty "demolish" properties


-- | A set of properties that are enumerations.
--
-- >>> Set.member "Engine.Actor:Role" enumProperties
-- True
enumProperties :: Set.Set StrictText.Text
enumProperties = Map.findWithDefault Set.empty "enum" properties


-- | A set of properties that are explosions.
--
-- >>> Set.member "TAGame.Ball_TA:ReplicatedExplosionData" explosionProperties
-- True
explosionProperties :: Set.Set StrictText.Text
explosionProperties = Map.findWithDefault Set.empty "explosion" properties


-- | A set of properties that are flagged integers.
--
-- >>> Set.member "Engine.Actor:Owner" flaggedIntProperties
-- True
flaggedIntProperties :: Set.Set StrictText.Text
flaggedIntProperties = Map.findWithDefault Set.empty "flagged_int" properties


-- | A set of properties that are floats.
--
-- >>> Set.member "Engine.Actor:DrawScale" floatProperties
-- True
floatProperties :: Set.Set StrictText.Text
floatProperties = Map.findWithDefault Set.empty "float" properties


-- | A set of properties that are game modes.
--
-- >>> Set.member "TAGame.GameEvent_TA:GameMode" gameModeProperties
-- True
gameModeProperties :: Set.Set StrictText.Text
gameModeProperties = Map.findWithDefault Set.empty "game_mode" properties


-- | A set of properties that are integers.
--
-- >>> Set.member "Engine.PlayerReplicationInfo:PlayerID" intProperties
-- True
intProperties :: Set.Set StrictText.Text
intProperties = Map.findWithDefault Set.empty "int" properties


-- | A set of properties that are online loadouts.
--
-- >>> Set.member "TAGame.PRI_TA:ClientLoadoutOnline" loadoutOnlineProperties
-- True
loadoutOnlineProperties :: Set.Set StrictText.Text
loadoutOnlineProperties = Map.findWithDefault Set.empty "loadout_online" properties


-- | A set of properties that are loadouts.
--
-- >>> Set.member "TAGame.PRI_TA:ClientLoadout" loadoutProperties
-- True
loadoutProperties :: Set.Set StrictText.Text
loadoutProperties = Map.findWithDefault Set.empty "loadout" properties


-- | A set of properties that are locations.
--
-- >>> Set.member "Engine.Actor:RelativeLocation" locationProperties
-- True
locationProperties :: Set.Set StrictText.Text
locationProperties = Map.findWithDefault Set.empty "location" properties


-- | A set of properties that are music stingers.
--
-- >>> Set.member "TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger" musicStingerProperties
-- True
musicStingerProperties :: Set.Set StrictText.Text
musicStingerProperties = Map.findWithDefault Set.empty "music_stinger" properties


-- | A set of properties that are party leaders.
--
-- >>> Set.member "TAGame.PRI_TA:PartyLeader" partyLeaderProperties
-- True
partyLeaderProperties :: Set.Set StrictText.Text
partyLeaderProperties = Map.findWithDefault Set.empty "party_leader" properties


-- | A set of properties that are pickups.
--
-- >>> Set.member "TAGame.VehiclePickup_TA:ReplicatedPickupData" pickupProperties
-- True
pickupProperties :: Set.Set StrictText.Text
pickupProperties = Map.findWithDefault Set.empty "pickup" properties


-- | A set of properties that are private match settings.
--
-- >>> Set.member "TAGame.GameEvent_SoccarPrivate_TA:MatchSettings" privateMatchSettingsProperties
-- True
privateMatchSettingsProperties :: Set.Set StrictText.Text
privateMatchSettingsProperties = Map.findWithDefault Set.empty "private_match_settings" properties


-- | A set of properties that are qwords.
--
-- >>> Set.member "ProjectX.GRI_X:GameServerID" qWordProperties
-- True
qWordProperties :: Set.Set StrictText.Text
qWordProperties = Map.findWithDefault Set.empty "qword" properties


-- | A set of properties that are relation rotations.
--
-- >>> Set.member "Engine.Actor:RelativeRotation" relativeRotationProperties
-- True
relativeRotationProperties :: Set.Set StrictText.Text
relativeRotationProperties = Map.findWithDefault Set.empty "relative_rotation" properties


-- | A set of properties that are reservations.
--
-- >>> Set.member "ProjectX.GRI_X:Reservations" reservationProperties
-- True
reservationProperties :: Set.Set StrictText.Text
reservationProperties = Map.findWithDefault Set.empty "reservation" properties


-- | A set of properties that are rigid body states.
--
-- >>> Set.member "TAGame.RBActor_TA:ReplicatedRBState" rigidBodyStateProperties
-- True
rigidBodyStateProperties :: Set.Set StrictText.Text
rigidBodyStateProperties = Map.findWithDefault Set.empty "rigid_body_state" properties


-- | A set of properties that are strings.
--
-- >>> Set.member "Engine.GameReplicationInfo:ServerName" stringProperties
-- True
stringProperties :: Set.Set StrictText.Text
stringProperties = Map.findWithDefault Set.empty "string" properties


-- | A set of properties that are team paints.
--
-- >>> Set.member "TAGame.Car_TA:TeamPaint" teamPaintProperties
-- True
teamPaintProperties :: Set.Set StrictText.Text
teamPaintProperties = Map.findWithDefault Set.empty "team_paint" properties


-- | A set of properties that are unique IDs.
--
-- >>> Set.member "Engine.PlayerReplicationInfo:UniqueId" uniqueIdProperties
-- True
uniqueIdProperties :: Set.Set StrictText.Text
uniqueIdProperties = Map.findWithDefault Set.empty "unique_id" properties


properties :: Map.Map StrictText.Text (Set.Set StrictText.Text)
properties = $(FileEmbed.embedFile "data/properties.json")
    & Aeson.decodeStrict
    & Maybe.fromMaybe Map.empty
