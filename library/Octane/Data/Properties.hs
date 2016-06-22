{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Properties (properties) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as StrictText

-- $
-- >>> :set -XOverloadedStrings


-- | A mapping between property names and their serialized type.
--
-- >>> Map.lookup "Engine.Actor:bBlockActors" properties
-- Just "boolean"
properties :: Map.Map StrictText.Text StrictText.Text
properties = $(FileEmbed.embedFile "data/properties.json")
    & Aeson.decodeStrict
    & Maybe.fromMaybe Map.empty
