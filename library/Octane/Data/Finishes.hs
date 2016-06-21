{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Finishes (finishes) where

import qualified Data.Bimap as Bimap
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A one-to-one mapping between paint finish IDs and their names.
--
-- >>> Bimap.lookup 266 finishes :: Maybe StrictText.Text
-- Just "Brushed Metal"
finishes :: Bimap.Bimap Int StrictText.Text
finishes = Embed.decodeBimap $(FileEmbed.embedFile "data/finishes.json")
