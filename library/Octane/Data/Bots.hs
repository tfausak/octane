{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Bots (bots) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between bot IDs and their names.
--
-- >>> Bimap.lookup 70 bots :: Maybe StrictText.Text
-- Just "Astro"
bots :: Bimap.Bimap Int StrictText.Text
bots = Embed.decodeBimap $(FileEmbed.embedFile "data/bots.json")
