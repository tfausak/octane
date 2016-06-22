{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Properties (properties) where

import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed

-- $
-- >>> :set -XOverloadedStrings


-- | A mapping between property names and their serialized type.
--
-- >>> Map.lookup "Engine.Actor:bBlockActors" properties
-- Just "boolean"
properties :: Map.Map StrictText.Text StrictText.Text
properties = Embed.decodeMap $(FileEmbed.embedFile "data/properties.json")
