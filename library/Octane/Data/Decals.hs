module Octane.Data.Decals (decals) where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between decal IDs and their names.
decals :: Bimap.Bimap Int StrictText.Text
decals =
    [ ("Backfire", backfireDecals)
    , ("Breakout", breakoutDecals)
    , ("Dominus", dominusDecals)
    , ("Gizmo", gizmoDecals)
    , ("Grog", grogDecals)
    , ("Hotshot", hotshotDecals)
    , ("Merc", mercDecals)
    , ("Octane", octaneDecals)
    , ("Paladin", paladinDecals)
    , ("Ripper", ripperDecals)
    , ("Road Hog", roadHogDecals)
    , ("Scarab", scarabDecals)
    , ("Takumi", takumiDecals)
    , ("Venom", venomDecals)
    , ("X-Devil", xDevilDecals)
    , ("Zippy", zippyDecals)
    ]
        & map (\ (body, values) -> values &
            map (\ (v, k) -> (k, v ++ " [" ++ body ++ "]")))
        & concat
        & map (\ (k, v) -> (k, StrictText.pack v))
        & Bimap.fromList
        & Bimap.insert 0 (StrictText.pack "None")


backfireDecals :: [(String, Int)]
backfireDecals =
    [ ("Flames", 288)
    , ("Lightning", 289)
    , ("Skulls", 290)
    , ("Stars", 291)
    , ("Stripes", 292)
    , ("Tech", 293)
    , ("Wings", 294)
    ]


breakoutDecals :: [(String, Int)]
breakoutDecals =
    [ ("Flames", 295)
    , ("Lightning", 296)
    , ("Skulls", 297)
    , ("Stars", 298)
    , ("Stripes", 299)
    , ("Tech", 300)
    , ("Wings", 301)
    ]


dominusDecals :: [(String, Int)]
dominusDecals =
    [ ("Flames", 504)
    , ("Scorpions", 506) -- Skorpion
    , ("Skulls", 507)
    , ("Stripes", 508)
    , ("Tats", 509) -- Tribal
    , ("Wings", 510)
    ]


gizmoDecals :: [(String, Int)]
gizmoDecals =
    [ ("Flames", 323)
    , ("Lightning", 324)
    , ("Skulls", 325)
    , ("Stars", 326)
    , ("Stripes", 327)
    , ("Tech", 328)
    , ("Wings", 329)
    ]


grogDecals :: [(String, Int)]
grogDecals =
    [ ("Bomber", 674)
    , ("Cyclops", 694)
    , ("Lepus", 675) -- Bunny
    , ("Stripes", 687)
    , ("Tagged", 695)
    , ("Tribal", 696)
    ]


hotshotDecals :: [(String, Int)]
hotshotDecals =
    [ ("Flames", 337)
    , ("Lightning", 338)
    , ("Skulls", 339)
    , ("Stars", 340)
    , ("Stripes", 341)
    , ("Tech", 342)
    , ("Wings", 343)
    ]


mercDecals :: [(String, Int)]
mercDecals =
    [ ("Dots", 344)
    , ("Flames", 345)
    , ("Lightning", 346)
    , ("Skulls", 347)
    , ("Stars", 348)
    , ("Stripes", 349)
    , ("Wings", 350)
    ]


octaneDecals :: [(String, Int)]
octaneDecals =
    [ ("Flames", 302)
    , ("Lightning", 303)
    , ("Skulls", 304)
    , ("Stars", 305)
    , ("Stripes", 306)
    , ("Tech", 307)
    , ("Wings", 308)
    ]


paladinDecals :: [(String, Int)]
paladinDecals =
    [ ("Flames", 309)
    , ("Lightning", 310)
    , ("Skulls", 311)
    , ("Stars", 312)
    , ("Stripes", 313)
    , ("Tech", 314)
    , ("Wings", 315)
    ]


ripperDecals :: [(String, Int)]
ripperDecals =
    [ ("Bomber", 701) -- Sandgirl
    , ("Flames", 669)
    , ("Ockie", 700) -- Getkraken
    , ("Shot Fox", 670) -- Kitsune
    , ("Spikes", 671)
    , ("Tribal", 672)
    ]


roadHogDecals :: [(String, Int)]
roadHogDecals =
    [ ("Flames", 316)
    , ("Lightning", 317)
    , ("Skulls", 318)
    , ("Stars", 319)
    , ("Stripes", 320)
    , ("Tech", 321) -- TechSplat
    , ("Wings", 322)
    ]


scarabDecals :: [(String, Int)]
scarabDecals =
    [ ("Bomani", 585) -- Clouds
    , ("Derby Girl", 592) -- RollerDerby
    , ("Flames", 591)
    , ("Hearts", 586)
    , ("Tiger", 587)
    , ("Tribal", 588)
    ]


takumiDecals :: [(String, Int)]
takumiDecals =
    [ ("Chaser", 503) -- Wisp
    , ("Copycat", 497) -- Gato
    , ("Crazy-8", 502) -- Technologic
    , ("Gaki", 499) -- Ohai
    , ("Reiko", 498) -- Nitrous
    , ("Stripes", 501)
    ]


venomDecals :: [(String, Int)]
venomDecals =
    [ ("Dots", 351)
    , ("Flames", 352)
    , ("Lightning", 353)
    , ("Skulls", 354)
    , ("Stripes", 355)
    , ("Tagged", 356)
    , ("Wings", 357)
    ]


xDevilDecals :: [(String, Int)]
xDevilDecals =
    [ ("Flames", 330)
    , ("Lightning", 331)
    , ("Skulls", 332)
    , ("Stars", 333)
    , ("Stripes", 334)
    , ("Tech", 335)
    , ("Wings", 336)
    ]


zippyDecals :: [(String, Int)]
zippyDecals =
    [ ("Caboodle", 589) -- HeckTec
    , ("Callous", 590) -- MeanMachine
    , ("Flames", 583)
    , ("Hearts", 581)
    , ("Leopard", 584)
    , ("Tiger", 582)
    ]
