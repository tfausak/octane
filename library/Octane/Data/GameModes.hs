{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.GameModes (gameModes) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between game mode IDs and their names.
--
-- >>> Bimap.lookup 1 gameModes :: Maybe StrictText.Text
-- Just "Hockey"
gameModes :: Bimap.Bimap Int StrictText.Text
gameModes = Embed.decodeBimap $(FileEmbed.embedFile "data/game-modes.json")
