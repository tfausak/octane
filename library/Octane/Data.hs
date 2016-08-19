{-# LANGUAGE TemplateHaskell #-}

module Octane.Data where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed

-- $setup
-- >>> :set -XOverloadedStrings


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
classes = Embed.decodeMap
    $(FileEmbed.embedFile "data/classes.json")


-- | A set of classes that have an initial location vector.
--
-- >>> Set.member "TAGame.Ball_TA" classesWithLocation
-- True
classesWithLocation :: Set.Set StrictText.Text
classesWithLocation = Embed.decodeSet
    $(FileEmbed.embedFile "data/classes-with-location.json")


-- | A set of classes that have an initial rotation vector.
--
-- >>> Set.member "TAGame.Ball_TA" classesWithRotation
-- True
classesWithRotation :: Set.Set StrictText.Text
classesWithRotation = Embed.decodeSet
    $(FileEmbed.embedFile "data/classes-with-rotation.json")


-- | A one-to-one mapping between game mode IDs and their names.
--
-- >>> Bimap.lookup 1 gameModes :: Maybe StrictText.Text
-- Just "Hockey"
gameModes :: Bimap.Bimap Int StrictText.Text
gameModes = Embed.decodeBimap
    $(FileEmbed.embedFile "data/game-modes.json")


-- | A one-to-one mapping between product IDs and their names.
--
-- >>> Bimap.lookup 1 products :: Maybe StrictText.Text
-- Just "Antenna_8Ball"
products :: Bimap.Bimap Word StrictText.Text
products = Embed.decodeBimap
    $(FileEmbed.embedFile "data/products.json")


-- | A mapping between property names and their serialized type.
--
-- >>> Map.lookup "Engine.Actor:bBlockActors" properties
-- Just "boolean"
properties :: Map.Map StrictText.Text StrictText.Text
properties = Embed.decodeMap
    $(FileEmbed.embedFile "data/properties.json")
