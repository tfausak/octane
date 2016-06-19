module Octane.Data.Logos where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between logo IDs and their names.
logos :: Bimap.Bimap Int StrictText.Text
logos =
    [ ("Barracudas", 244)
    , ("Blacklight", 245)
    , ("Bombers", 246)
    , ("Cyclones", 247)
    , ("Destroyers", 248)
    , ("Dragons", 249)
    , ("Express", 250)
    , ("Guardians", 251)
    , ("Knights", 252)
    , ("Kodiaks", 253)
    , ("Mammoths", 254)
    , ("Monarchs", 255)
    , ("Phantoms", 256)
    , ("Pharaohs", 257)
    , ("Pioneers", 258)
    , ("Ravagers", 259)
    , ("Rebels", 260)
    , ("Rovers", 261)
    , ("Scorpions", 262)
    , ("Skyhawks", 263)
    , ("Wolves", 264)
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
