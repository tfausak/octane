{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Decals (decals) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between decal IDs and their names.
--
-- >>> Bimap.lookup 288 decals :: Maybe StrictText.Text
-- Just "Flames [Backfire]"
decals :: Bimap.Bimap Int StrictText.Text
decals = Embed.decodeBimap $(FileEmbed.embedFile "data/decals.json")
