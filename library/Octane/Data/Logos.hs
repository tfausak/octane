{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Logos (logos) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between logo IDs and their names.
--
-- >>> Bimap.lookup 244 logos :: Maybe StrictText.Text
-- Just "Barracudas"
logos :: Bimap.Bimap Int StrictText.Text
logos = Embed.decodeBimap $(FileEmbed.embedFile "data/logos.json")
