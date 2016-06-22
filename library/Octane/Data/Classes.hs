module Octane.Data.Classes
    ( objectToClass
    , locationClasses
    , rotationClasses
    ) where

import Data.Function ((&))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText


-- | A map from object names to their class names. Note that some names have
-- been normalized. For example,
-- @Neotokyo_p.TheWorld:PersistentLevel.InMapScoreboard_TA_0@ is in this map as
-- @TheWorld:PersistentLevel.InMapScoreboard_TA@.
objectToClass :: Map.Map StrictText.Text StrictText.Text
objectToClass = Map.foldrWithKey
    (\ klass objects m -> objects
        & Set.map (\ object -> (object, klass))
        & Set.toList
        & Map.fromList
        & Map.union m)
    Map.empty
    classToObjects


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


classToObjects :: Map.Map StrictText.Text (Set.Set StrictText.Text)
classToObjects = let
    normal =
        [ ( "TAGame.Ball_TA",
            [ "Archetypes.Ball.Ball_Default"
            , "Archetypes.Ball.Ball_Basketball"
            , "Archetypes.Ball.Ball_Puck"
            , "Archetypes.Ball.CubeBall"
            ])
        , ( "TAGame.CameraSettingsActor_TA",
            [ "TAGame.CameraSettingsActor_TA:PRI"
            , "TAGame.Default__CameraSettingsActor_TA"
            ])
        , ( "TAGame.CarComponent_Boost_TA",
            [ "Archetypes.CarComponents.CarComponent_Boost"
            ])
        , ( "TAGame.CarComponent_Dodge_TA",
            [ "Archetypes.CarComponents.CarComponent_Dodge"
            ])
        , ( "TAGame.CarComponent_DoubleJump_TA",
            [ "Archetypes.CarComponents.CarComponent_DoubleJump"
            ])
        , ( "TAGame.CarComponent_FlipCar_TA",
            [ "Archetypes.CarComponents.CarComponent_FlipCar"
            ])
        , ( "TAGame.CarComponent_Jump_TA",
            [ "Archetypes.CarComponents.CarComponent_Jump"
            ])
        , ( "TAGame.Car_TA",
            [ "Archetypes.Car.Car_Default"
            , "Archetypes.GameEvent.GameEvent_Season:CarArchetype"
            ])
        , ( "TAGame.GameEvent_Season_TA",
            [ "Archetypes.GameEvent.GameEvent_Season"
            ])
        , ( "TAGame.GameEvent_SoccarPrivate_TA",
            [ "Archetypes.GameEvent.GameEvent_BasketballPrivate"
            , "Archetypes.GameEvent.GameEvent_HockeyPrivate"
            , "Archetypes.GameEvent.GameEvent_SoccarPrivate"
            ])
        , ( "TAGame.GameEvent_SoccarSplitscreen_TA",
            [ "Archetypes.GameEvent.GameEvent_BasketballSplitscreen"
            , "Archetypes.GameEvent.GameEvent_HockeySplitscreen"
            , "Archetypes.GameEvent.GameEvent_SoccarSplitscreen"
            ])
        , ( "TAGame.GameEvent_Soccar_TA",
            [ "Archetypes.GameEvent.GameEvent_Soccar"
            , "Archetypes.GameEvent.GameEvent_Basketball"
            ])
        , ( "TAGame.GRI_TA",
            [ "GameInfo_Basketball.GameInfo.GameInfo_Basketball:GameReplicationInfoArchetype"
            , "Gameinfo_Hockey.GameInfo.Gameinfo_Hockey:GameReplicationInfoArchetype"
            , "GameInfo_Season.GameInfo.GameInfo_Season:GameReplicationInfoArchetype"
            , "GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype"
            ])
        , ( "TAGame.PRI_TA",
            [ "TAGame.Default__PRI_TA"
            ])
        , ( "TAGame.Team_Soccar_TA",
            [ "Archetypes.Teams.Team0"
            , "Archetypes.Teams.Team1"
            ])
        , ( "TAGame.VoteActor_TA",
            [ "TAGame.Default__VoteActor_TA"
            ])
        , ( "TAGame.CrowdActor_TA",
            [ "TheWorld:PersistentLevel.CrowdActor_TA"
            ])
        , ( "TAGame.CrowdManager_TA",
            [ "TheWorld:PersistentLevel.CrowdManager_TA"
            ])
        , ( "TAGame.InMapScoreboard_TA",
            [ "TheWorld:PersistentLevel.InMapScoreboard_TA"
            ])
        , ( "TAGame.VehiclePickup_Boost_TA",
            [ "TheWorld:PersistentLevel.VehiclePickup_Boost_TA"
            ])
        ] & map (\ (klass, objects) ->
            ( StrictText.pack klass
            , objects & map StrictText.pack & Set.fromList
            ))
    in Map.fromList normal
