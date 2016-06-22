{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Classes
    ( objectToClass
    , locationClasses
    , rotationClasses
    ) where

import Data.Function ((&))

import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed


-- | A map from object names to their class names. Note that some names have
-- been normalized. For example,
-- @Neotokyo_p.TheWorld:PersistentLevel.InMapScoreboard_TA_0@ is in this map as
-- @TheWorld:PersistentLevel.InMapScoreboard_TA@.
objectToClass :: Map.Map StrictText.Text StrictText.Text
objectToClass = Embed.decodeMap $(FileEmbed.embedFile "data/classes.json")


-- | A set of classes that have an initial location vector.
locationClasses :: Set.Set StrictText.Text
locationClasses =
    [ "TAGame.Ball_TA"
    , "TAGame.CameraSettingsActor_TA"
    , "TAGame.CarComponent_Boost_TA"
    , "TAGame.CarComponent_Dodge_TA"
    , "TAGame.CarComponent_DoubleJump_TA"
    , "TAGame.CarComponent_FlipCar_TA"
    , "TAGame.CarComponent_Jump_TA"
    , "TAGame.Car_Season_TA"
    , "TAGame.Car_TA"
    , "TAGame.GRI_TA"
    , "TAGame.GameEvent_Season_TA"
    , "TAGame.GameEvent_SoccarPrivate_TA"
    , "TAGame.GameEvent_SoccarSplitscreen_TA"
    , "TAGame.GameEvent_Soccar_TA"
    , "TAGame.PRI_TA"
    , "TAGame.Team_Soccar_TA"
    ] & map StrictText.pack & Set.fromList


-- | A set of classes that have an initial rotation vector.
rotationClasses :: Set.Set StrictText.Text
rotationClasses =
    [ "TAGame.Ball_TA"
    , "TAGame.Car_Season_TA"
    , "TAGame.Car_TA"
    ] & map StrictText.pack & Set.fromList
