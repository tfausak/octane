module Octane.Data.RocketTrails (rocketTrails) where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between rocket trail IDs and their names.
rocketTrails :: Bimap.Bimap Int StrictText.Text
rocketTrails =
    [ ("(Alpha Reward) Gold Rush", 32) -- AlphaReward
    , ("Accelerato", 524) -- MusicalNotes
    , ("Batmobile", 868) -- DarkCar
    , ("Battle-Stars", 545)
    , ("Bubbles", 33)
    , ("Burnout", 387)
    , ("Candy Corn", 578)
    , ("Confetti", 34)
    , ("Datastream", 35) -- Digital
    , ("Flamethrower Blue", 37)
    , ("Flamethrower Green", 38)
    , ("Flamethrower Pink", 39)
    , ("Flamethrower Purple", 40)
    , ("Flamethrower Red", 41)
    , ("Flamethrower", 36)
    , ("Flowers", 42)
    , ("Grass", 43)
    , ("Hydro", 69) -- Water
    , ("Ion Blue", 45) -- LightTrail
    , ("Ion Green", 46)
    , ("Ion Pink", 47)
    , ("Ion Purple", 48)
    , ("Ion Red", 49)
    , ("Ion Yellow", 50)
    , ("Money", 51)
    , ("Nitrous", 384)
    , ("Nuts & Bolts", 544)
    , ("OutaTime", 626) -- GreyCar
    , ("Plasma", 52)
    , ("Portal - Conversion Gel", 651)
    , ("Portal - Propulsion Gel", 652)
    , ("Portal - Reuplsion Gel", 653)
    , ("Rainbow", 59)
    , ("Sacred", 44) -- HolyLight
    , ("Sandstorm", 664)
    , ("Slime", 60)
    , ("Snowflakes", 61)
    , ("Sparkles", 62)
    , ("Standard Blue", 64)
    , ("Standard Pink", 65)
    , ("Standard Purple", 66)
    , ("Standard Red", 63)
    , ("Standard Yellow", 67)
    , ("Sweet Tooth", 68)
    , ("Thermal Blue", 54)
    , ("Thermal Green", 55)
    , ("Thermal Pink", 56)
    , ("Thermal Purple", 57)
    , ("Thermal Yellow", 58)
    , ("Thermal", 53) -- Propulsion
    , ("Xmas", 635) -- Presents
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
