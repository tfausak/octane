{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Antennas (antennas) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between antenna IDs and their names.
--
-- >>> Bimap.lookup 1 antennas :: Maybe StrictText.Text
-- Just "8-Ball"
antennas :: Bimap.Bimap Int StrictText.Text
antennas = Embed.decodeBimap $(FileEmbed.embedFile "data/antennas.json")
