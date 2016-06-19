module Octane.Data.Bodies (bodies) where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between body IDs and their names.
bodies :: Bimap.Bimap Int StrictText.Text
bodies =
    [ ("Armadillo", 625) -- GearsCar
    , ("Backfire", 21)
    , ("Batmobile", 803) -- Darkcar
    , ("Breakout", 22) -- Force
    , ("DeLorean Time Machine", 597) -- GreyCar
    , ("Dominus", 403) -- MuscleCar
    , ("Gizmo", 26) -- Spark
    , ("Grog", 607) -- WastelandTruck
    , ("Hogsticker", 723) -- Warthog
    , ("Hotshot", 29) -- Torment
    , ("Merc", 30) -- Vanquish
    , ("Octane", 23)
    , ("Paladin", 24) -- Orion
    , ("Ripper", 600) -- Interceptor
    , ("Road Hog", 25) -- Rhino
    , ("Scarab", 404)
    , ("Sweet Tooth", 27)
    , ("Takumi", 402) -- Import
    , ("Venom", 31)
    , ("X-Devil", 28) -- Torch
    , ("Zippy", 523)
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
