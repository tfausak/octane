module Octane.Data.Toppers (toppers) where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between topper IDs and their names.
toppers :: Bimap.Bimap Int StrictText.Text
toppers =
    [ ("None", 0)
    , ("(Alpha Reward) Gold Cap", 224) -- AlphaHat
    , ("Antlers", 774)
    , ("Beret", 775)
    , ("Biker Cap", 776)
    , ("Birthday Cake", 789)
    , ("Blitzen", 640) -- ReindeerAntlers
    , ("Bobby Helmet", 227) -- BPH
    , ("Boombox", 521)
    , ("Bowler", 745) -- GentlemanHat
    , ("Brodie Helmet", 743) -- DoughboyHelmet
    , ("Brünnhilde", 241) -- Viking_Helmet
    , ("Captain's Hat", 741)
    , ("Cavalier", 580) -- Tricorn
    , ("Chainsaw", 795)
    , ("Chef's Hat", 756)
    , ("Cherry Top", 237) -- PoliceSiren
    , ("Christmas Tree", 639) -- RegularChristmasTree
    , ("Cockroach", 769)
    , ("Cow Skull", 685)
    , ("deadmau5", 796)
    , ("Deerstalker", 792)
    , ("Derby", 790)
    , ("Devil Horns", 229)
    , ("Fez", 230)
    , ("Fire Helmet", 231)
    , ("Foam Hat", 390) -- TourneyWinner
    , ("Fruit Hat", 744)
    , ("Graduation Cap", 778)
    , ("Halo", 232)
    , ("Hard Hat", 233)
    , ("Homburg", 742) -- DetectiveHat
    , ("Locomotive", 547) -- TrainConductor
    , ("Mariachi Hat", 234)
    , ("Mohawk", 638)
    , ("Mouse Trap", 782) -- RatTrap
    , ("Paper Boat", 770)
    , ("Pirate's Hat", 235)
    , ("Pixelated Shades", 494) -- DWI
    , ("Pizza Topper", 236)
    , ("Plunger", 772)
    , ("Police Hat", 746)
    , ("Portal - Cake", 661)
    , ("Propellerhead", 225) -- Beanie
    , ("Pumpkin", 546) -- CatOLantern
    , ("Rasta", 771)
    , ("Rhino Horns", 780)
    , ("Royal Crown", 228)
    , ("Sad Sapling", 641) -- SadChristmasTree
    , ("Santa", 642)
    , ("Season 1 - Bronze", 705)
    , ("Season 1 - Gold", 706)
    , ("Season 1 - Platinum", 707)
    , ("Season 1 - Silver", 708)
    , ("Shark Fin", 579)
    , ("Shuriken", 773)
    , ("Sombrero", 238)
    , ("Taxi Topper", 239)
    , ("Tiara", 495)
    , ("Top Hat", 240)
    , ("Traffic Cone", 747)
    , ("Unicorn", 748) -- Unihorn
    , ("Witch's Hat", 242)
    , ("Wizard Hat", 243)
    , ("Work Boot", 740)
    , ("Worms W.M.D.", 1028)
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
