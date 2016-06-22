{-# LANGUAGE TemplateHaskell #-}

module Octane.Data where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed

-- $
-- >>> :set -XOverloadedStrings


-- | A one-to-one mapping between antenna IDs and their names.
--
-- >>> Bimap.lookup 1 antennas :: Maybe StrictText.Text
-- Just "8-Ball"
antennas :: Bimap.Bimap Int StrictText.Text
antennas = Embed.decodeBimap $(FileEmbed.embedFile "data/antennas.json")


-- | A one-to-one mapping between body IDs and their names.
--
-- >>> Bimap.lookup 23 bodies :: Maybe StrictText.Text
-- Just "Octane"
bodies :: Bimap.Bimap Int StrictText.Text
bodies = Embed.decodeBimap $(FileEmbed.embedFile "data/bodies.json")


-- | A one-to-one mapping between bot IDs and their names.
--
-- >>> Bimap.lookup 70 bots :: Maybe StrictText.Text
-- Just "Astro"
bots :: Bimap.Bimap Int StrictText.Text
bots = Embed.decodeBimap $(FileEmbed.embedFile "data/bots.json")


-- | A map from object names to their class names.
--
-- >>> Map.lookup "Archetypes.Ball.Ball_Default" classes
-- Just "TAGame.Ball_TA"
--
-- Note that some object names have been normalized to make lookup easier.
--
-- >>> Map.lookup "Neotokyo_p.TheWorld:PersistentLevel.InMapScoreboard_TA_0@" classes
-- Nothing
-- >>> Map.lookup "TheWorld:PersistentLevel.InMapScoreboard_TA" classes
-- Just "TAGame.InMapScoreboard_TA"
classes :: Map.Map StrictText.Text StrictText.Text
classes = Embed.decodeMap $(FileEmbed.embedFile "data/classes.json")


-- | A set of classes that have an initial location vector.
--
-- >>> Set.member "TAGame.Ball_TA" classesWithLocation
-- True
classesWithLocation :: Set.Set StrictText.Text
classesWithLocation = Embed.decodeSet $(FileEmbed.embedFile "data/classes-with-location.json")


-- | A set of classes that have an initial rotation vector.
--
-- >>> Set.member "TAGame.Ball_TA" classesWithRotation
-- True
classesWithRotation :: Set.Set StrictText.Text
classesWithRotation = Embed.decodeSet $(FileEmbed.embedFile "data/classes-with-rotation.json")


-- | A one-to-one mapping between decal IDs and their names.
--
-- >>> Bimap.lookup 288 decals :: Maybe StrictText.Text
-- Just "Flames [Backfire]"
decals :: Bimap.Bimap Int StrictText.Text
decals = Embed.decodeBimap $(FileEmbed.embedFile "data/decals.json")


-- | A one-to-one mapping between paint finish IDs and their names.
--
-- >>> Bimap.lookup 266 finishes :: Maybe StrictText.Text
-- Just "Brushed Metal"
finishes :: Bimap.Bimap Int StrictText.Text
finishes = Embed.decodeBimap $(FileEmbed.embedFile "data/finishes.json")


-- | A one-to-one mapping between game mode IDs and their names.
--
-- >>> Bimap.lookup 1 gameModes :: Maybe StrictText.Text
-- Just "Hockey"
gameModes :: Bimap.Bimap Int StrictText.Text
gameModes = Embed.decodeBimap $(FileEmbed.embedFile "data/game-modes.json")


-- | A one-to-one mapping between logo IDs and their names.
--
-- >>> Bimap.lookup 244 logos :: Maybe StrictText.Text
-- Just "Barracudas"
logos :: Bimap.Bimap Int StrictText.Text
logos = Embed.decodeBimap $(FileEmbed.embedFile "data/logos.json")


-- | A mapping between property names and their serialized type.
--
-- >>> Map.lookup "Engine.Actor:bBlockActors" properties
-- Just "boolean"
properties :: Map.Map StrictText.Text StrictText.Text
properties = Embed.decodeMap $(FileEmbed.embedFile "data/properties.json")


-- | A one-to-one mapping between body IDs and their names.
--
-- >>> Bimap.lookup 524 rocketTrails :: Maybe StrictText.Text
-- Just "Accelerato"
rocketTrails :: Bimap.Bimap Int StrictText.Text
rocketTrails = Embed.decodeBimap $(FileEmbed.embedFile "data/rocket-trails.json")


-- | A one-to-one mapping between topper IDs and their names.
--
-- >>> Bimap.lookup 774 toppers :: Maybe StrictText.Text
-- Just "Antlers"
toppers :: Bimap.Bimap Int StrictText.Text
toppers = Embed.decodeBimap $(FileEmbed.embedFile "data/toppers.json")


-- | A one-to-one mapping between wheel IDs and their names.
--
-- >>> Bimap.lookup 374 wheels :: Maybe StrictText.Text
-- Just "Alchemist"
wheels :: Bimap.Bimap Int StrictText.Text
wheels = Embed.decodeBimap $(FileEmbed.embedFile "data/wheels.json")
