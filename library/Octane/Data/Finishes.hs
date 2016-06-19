module Octane.Data.Finishes (finishes) where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between paint finish IDs and their names.
finishes :: Bimap.Bimap Int StrictText.Text
finishes =
    [ ("Brushed Metal", 266)
    , ("Camo", 541)
    , ("Carbon Fiber", 272) -- GlossyCarbonFiber
    , ("Corroded Metal", 268) -- Corroded
    , ("DeLorean Time Machine", 623) -- GreyCar
    , ("Glossy", 270) -- Default
    , ("Matte", 273)
    , ("Metallic", 274)
    , ("Metallic Pearl", 275)
    , ("Pearlescent", 276)
    , ("Semigloss", 277) -- Plastic
    , ("Sun-Damaged", 542) -- Cracked
    , ("Toon Glossy", 279)
    , ("Toon Matte", 281)
    , ("Toon Wood", 280)
    , ("Wood", 283)
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
