{-# LANGUAGE TemplateHaskell #-}

module Octane.Data where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed

-- | A one-to-one mapping between game mode IDs and their names.
gameModes :: Bimap.Bimap Int StrictText.Text
gameModes = Embed.decodeBimap $(FileEmbed.embedFile "data/game-modes.json")

-- | A one-to-one mapping between product IDs and their names.
products :: Bimap.Bimap Word StrictText.Text
products = Embed.decodeBimap $(FileEmbed.embedFile "data/products.json")
