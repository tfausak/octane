-- | These helper functions are usually used with 'Data.FileEmbed.embedFile'.
module Octane.Utility.Embed (decodeBimap, decodeMap, decodeSet) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


-- | Decodes some bytes into a bidirection map. The bytes are assumed to be a
-- JSON object mapping values to keys. That means the resulting bimap is
-- 'Bimap.twist'ed from what you might expect.
decodeBimap
    :: (FromJSON (StrictMap b a), Ord a, Ord b)
    => StrictBytes
    -> Bimap.Bimap a b
decodeBimap bytes = bytes
    & Aeson.decodeStrict
    & Maybe.fromMaybe Map.empty
    & Map.toList
    & Bimap.fromList
    & Bimap.twist


-- | Decodes some bytes into a map. The bytes are assumed to be a JSON object
-- mapping keys to values.
decodeMap
    :: (FromJSON (StrictMap a b))
    => StrictBytes
    -> StrictMap a b
decodeMap bytes = bytes
    & Aeson.decodeStrict
    & Maybe.fromMaybe Map.empty


-- | Decodes some bytes into a set. The bytes are assumed to be a JSON array.
decodeSet
    :: (FromJSON a, Ord a)
    => StrictBytes
    -> Set a
decodeSet bytes = bytes
    & Aeson.decodeStrict
    & Maybe.fromMaybe Set.empty
