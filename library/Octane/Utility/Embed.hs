{-# LANGUAGE FlexibleContexts #-}

module Octane.Utility.Embed (decodeBimap, decodeMap) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.ByteString as StrictBytes
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe


-- | Decodes some bytes into a bidirection map. The bytes are assumed to be a
-- JSON object mapping values to keys. That means the resulting bimap is
-- 'Bimap.twist'ed from what you might expect.
--
-- Typically used with 'Data.FileEmbed.embedFile'.
decodeBimap :: (Aeson.FromJSON (Map.Map b a), Ord a, Ord b) => StrictBytes.ByteString -> Bimap.Bimap a b
decodeBimap bytes = bytes
    & Aeson.decodeStrict
    & Maybe.fromMaybe Map.empty
    & Map.toList
    & Bimap.fromList
    & Bimap.twist


decodeMap :: (Aeson.FromJSON (Map.Map a b)) => StrictBytes.ByteString -> Map.Map a b
decodeMap bytes = bytes
    & Aeson.decodeStrict
    & Maybe.fromMaybe Map.empty
