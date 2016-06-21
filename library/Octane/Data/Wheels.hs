{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Wheels (wheels) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between wheel IDs and their names.
--
-- >>> Bimap.lookup 374 wheels :: Maybe StrictText.Text
-- Just "Alchemist"
wheels :: Bimap.Bimap Int StrictText.Text
wheels = Embed.decodeBimap $(FileEmbed.embedFile "data/wheels.json")
