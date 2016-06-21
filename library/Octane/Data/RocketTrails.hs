{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.RocketTrails (rocketTrails) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between body IDs and their names.
--
-- >>> Bimap.lookup 524 rocketTrails :: Maybe StrictText.Text
-- Just "Accelerato"
rocketTrails :: Bimap.Bimap Int StrictText.Text
rocketTrails = Embed.decodeBimap $(FileEmbed.embedFile "data/rocket-trails.json")
