module Octane.Data where

import Basics

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Octane.Utility.Embed as Embed


-- | A map from object names to their class names.
--
-- Note that some object names have been normalized to make lookup easier.
classes :: StrictMap StrictText StrictText
classes = Embed.decodeMap $(FileEmbed.embedFile "data/classes.json")


-- | A set of classes that have an initial location vector.
classesWithLocation :: Set StrictText
classesWithLocation = Embed.decodeSet $(FileEmbed.embedFile "data/classes-with-location.json")


-- | A set of classes that have an initial rotation vector.
classesWithRotation :: Set StrictText
classesWithRotation = Embed.decodeSet $(FileEmbed.embedFile "data/classes-with-rotation.json")


-- | A one-to-one mapping between game mode IDs and their names.
gameModes :: Bimap.Bimap Int StrictText
gameModes = Embed.decodeBimap $(FileEmbed.embedFile "data/game-modes.json")


-- | A one-to-one mapping between product IDs and their names.
products :: Bimap.Bimap Word StrictText
products = Embed.decodeBimap $(FileEmbed.embedFile "data/products.json")


-- | A mapping between property names and their serialized type.
properties :: StrictMap StrictText StrictText
properties = Embed.decodeMap $(FileEmbed.embedFile "data/properties.json")
