module Octane.Data.Wheels where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between wheels IDs and their names.
wheels :: Bimap.Bimap Int StrictText.Text
wheels =
    [ ("(Alpha Reward) Goldstone", 358) -- AlphaRim
    , ("Alchemist", 374) -- Revolution
    , ("Almas", 364) -- Crypt
    , ("Armadillo", 618)
    , ("Batmobile", 874) -- Darkcar
    , ("Bender", 360) -- Ballistic
    , ("Carriage", 549)
    , ("Cog", 716) -- TODO: What is this called in game?
    , ("Cristiano", 386) -- SoccerBall
    , ("DeLorean Time Machine", 609) -- GreyCar
    , ("Dieci", 363) -- Caliber
    , ("Falco", 382) -- Vulcan
    , ("Foreman", 372) -- OffRoad
    , ("Grog", 613) -- WastelandTruck
    , ("Hogsticker", 719) -- Warthog
    , ("Invader", 380) -- Triad
    , ("Lowrider", 369)
    , ("Lucci", 361) -- Bling
    , ("Mountaineer", 375) -- SnowTire
    , ("Neptune", 370) -- Mob
    , ("Octavian", 379) -- Tarantula
    , ("OEM", 376) -- Star
    , ("Rat Rod", 366) -- Dynamo
    , ("Ripper", 690) -- Spiked
    , ("Scarab", 540) -- Scarab
    , ("Servergate", 519) -- SkullxBones
    , ("Spinner", 388) -- Spinner
    , ("Spyder", 371) -- Ninja
    , ("Stallion", 365) -- DeepDish
    , ("Stern", 368) -- Hydra
    , ("Sunburt", 383) -- Wynd
    , ("Sweet Tooth", 378)
    , ("Tempest", 377) -- Storm
    , ("Tomahawk", 362) -- Brink
    , ("Trahere", 367) -- Forge
    , ("Tunica", 359) -- Atlantis
    , ("Veloce", 373) -- Pedigree
    , ("Vortex", 381)
    , ("Zippy", 518) -- SingleSpoke
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
