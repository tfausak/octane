{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Toppers (toppers) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between topper IDs and their names.
--
-- >>> Bimap.lookup 774 toppers :: Maybe StrictText.Text
-- Just "Antlers"
toppers :: Bimap.Bimap Int StrictText.Text
toppers = Embed.decodeBimap $(FileEmbed.embedFile "data/toppers.json")
