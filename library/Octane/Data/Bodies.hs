{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Bodies (bodies) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between body IDs and their names.
--
-- >>> Bimap.lookup 23 bodies :: Maybe StrictText.Text
-- Just "Octane"
bodies :: Bimap.Bimap Int StrictText.Text
bodies = Embed.decodeBimap $(FileEmbed.embedFile "data/bodies.json")
