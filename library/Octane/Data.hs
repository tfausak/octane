{-# LANGUAGE TemplateHaskell #-}

module Octane.Data where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A map from object names to their class names.
classes :: Map.Map StrictText.Text StrictText.Text
classes = Embed.decodeMap
    $(FileEmbed.embedFile "data/classes.json")


-- | A set of classes that have an initial location vector.
classesWithLocation :: Set.Set StrictText.Text
classesWithLocation = Embed.decodeSet
    $(FileEmbed.embedFile "data/classes-with-location.json")


-- | A set of classes that have an initial rotation vector.
classesWithRotation :: Set.Set StrictText.Text
classesWithRotation = Embed.decodeSet
    $(FileEmbed.embedFile "data/classes-with-rotation.json")


-- | A one-to-one mapping between game mode IDs and their names.
gameModes :: Bimap.Bimap Int StrictText.Text
gameModes = Embed.decodeBimap
    $(FileEmbed.embedFile "data/game-modes.json")


-- | A one-to-one mapping between product IDs and their names.
products :: Bimap.Bimap Word StrictText.Text
products = Embed.decodeBimap
    $(FileEmbed.embedFile "data/products.json")


-- | A mapping between property names and their serialized type.
properties :: Map.Map StrictText.Text StrictText.Text
properties = Embed.decodeMap
    $(FileEmbed.embedFile "data/properties.json")
