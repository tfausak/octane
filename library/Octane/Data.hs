-- | This module contains largely static data about Rocket League itself, such
-- as which classes objects belong to.
module Octane.Data where

import Data.Function ((&))
import Data.Monoid ((<>))

import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- * Classes

-- | A map from class names to a set of object names that are that class. This
-- is typically not useful by itself, as it is used to build the
-- 'objectToClass' map.
classToObjects :: Map.Map Text.Text (Set.Set Text.Text)
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
            [ "Archetypes.Teams.Team"
            ])
        , ( "TAGame.VoteActor_TA",
            [ "TAGame.Default__VoteActor_TA"
            ])
        ] & map (\ (klass, objects) ->
            ( Text.pack klass
            , objects & map Text.pack & Set.fromList
            ))
    special =
        [ ("TAGame.CrowdActor_TA", ".TheWorld:PersistentLevel.CrowdActor_TA_")
        , ("TAGame.CrowdManager_TA", ".TheWorld:PersistentLevel.CrowdManager_TA_")
        , ("TAGame.VehiclePickup_Boost_TA", ".TheWorld:PersistentLevel.VehiclePickup_Boost_TA_")
        ] & map (\ (klass, suffix) ->
            ( Text.pack klass
            , levels & Set.map (\ level -> level <> Text.pack suffix)
            ))
    in Map.fromList (normal ++ special)

-- | A set of classes that have an initial location vector.
locationClasses :: Set.Set Text.Text
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
    ] & map Text.pack & Set.fromList

-- | A set of classes that have an initial rotation vector.
rotationClasses :: Set.Set Text.Text
rotationClasses =
    [ "TAGame.Ball_TA"
    , "TAGame.Car_Season_TA"
    , "TAGame.Car_TA"
    ] & map Text.pack & Set.fromList

-- * Levels

-- | A set of level names. This is typically not useful by itself, as it is
-- used to build the 'classToObjects' map. If you are going to use it, be
-- aware that many of the levels are duplicated with strange capitalization.
-- For example, this set contains both @Wasteland_P@ and @Wasteland_p@.
levels :: Set.Set Text.Text
levels =
    [ "EuroStadium_Rainy_P"
    , "HoopsStadium_P"
    , "Park_Night_P"
    , "Park_Rainy_P"
    , "Stadium_p"
    , "TrainStation_Night_P"
    , "TrainStation_P"
    , "Trainstation_Night_P"
    , "UtopiaStadium_Dusk_P"
    , "UtopiaStadium_Dusk_p"
    , "UtopiaStadium_P"
    , "Utopiastadium_p"
    , "Wasteland_P"
    , "Wasteland_p"
    , "eurostad_oob_audio_map"
    , "eurostadium_p"
    , "eurostadium_rainy_audio"
    , "hoopsstadium_sfx"
    , "labs_cosmic_p"
    , "labs_doublegoal_p"
    , "labs_underpass_p"
    , "labs_utopia_p"
    , "park_night_sfx"
    , "park_p"
    , "park_rainy_sfx"
    , "park_sfx"
    , "stadium_oob_audio_map"
    , "stadium_p"
    , "stadium_winter_p"
    , "trainstation_p"
    , "utopiastadium_p"
    , "utopiastadium_sfx"
    , "wasteland_sfx"
    ] & map Text.pack & Set.fromList

-- * Objects

-- | A map from object names to their class names. Note that any trailing
-- numbers have been stripped from the object names. So
-- @Archetypes.Teams.Team0@ is in this map as @Archetypes.Teams.Team@.
objectToClass :: Map.Map Text.Text Text.Text
objectToClass = Map.foldrWithKey
    (\ klass objects m -> objects
        & Set.map (\ object -> (object, klass))
        & Set.toList
        & Map.fromList
        & Map.union m)
    Map.empty
    classToObjects

-- * Properties

-- | A set of properties that are booleans.
booleanProperties :: Set.Set Text.Text
booleanProperties =
    [ "Engine.Actor:bBlockActors"
    , "Engine.Actor:bCollideActors"
    , "Engine.Actor:bCollideWorld"
    , "Engine.Actor:bHardAttach"
    , "Engine.Actor:bHidden"
    , "Engine.Actor:bNetOwner"
    , "Engine.Actor:bProjTarget"
    , "Engine.Actor:bTearOff"
    , "Engine.GameReplicationInfo:bMatchIsOver"
    , "Engine.Pawn:bCanSwatTurn"
    , "Engine.Pawn:bRootMotionFromInterpCurve"
    , "Engine.Pawn:bSimulateGravity"
    , "Engine.PlayerReplicationInfo:bAdmin"
    , "Engine.PlayerReplicationInfo:bBot"
    , "Engine.PlayerReplicationInfo:bFromPreviousLevel"
    , "Engine.PlayerReplicationInfo:bIsSpectator"
    , "Engine.PlayerReplicationInfo:bOnlySpectator"
    , "Engine.PlayerReplicationInfo:bOutOfLives"
    , "Engine.PlayerReplicationInfo:bReadyToPlay"
    , "Engine.PlayerReplicationInfo:bWaitingPlayer"
    , "ProjectX.GRI_X:bGameStarted"
    , "TAGame.CameraSettingsActor_TA:bUsingBehindView"
    , "TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera"
    , "TAGame.CarComponent_Boost_TA:bNoBoost"
    , "TAGame.CarComponent_Boost_TA:bUnlimitedBoost"
    , "TAGame.CarComponent_FlipCar_TA:bFlipRight"
    , "TAGame.GameEvent_Soccar_TA:bBallHasBeenHit"
    , "TAGame.GameEvent_Soccar_TA:bOverTime"
    , "TAGame.GameEvent_TA:bHasLeaveMatchPenalty"
    , "TAGame.GameEvent_Team_TA:bDisableMutingOtherTeam"
    , "TAGame.PRI_TA:bIsInSplitScreen"
    , "TAGame.PRI_TA:bMatchMVP"
    , "TAGame.PRI_TA:bOnlineLoadoutSet"
    , "TAGame.PRI_TA:bReady"
    , "TAGame.PRI_TA:bUsingBehindView"
    , "TAGame.PRI_TA:bUsingFreecam"
    , "TAGame.PRI_TA:bUsingSecondaryCamera"
    , "TAGame.PRI_TA:bVoteToForfeitDisabled"
    , "TAGame.RBActor_TA:bFrozen"
    , "TAGame.RBActor_TA:bReplayActor"
    , "TAGame.Vehicle_TA:bDriving"
    , "TAGame.Vehicle_TA:bReplicatedHandbrake"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are bytes.
byteProperties :: Set.Set Text.Text
byteProperties =
    [ "Engine.PlayerReplicationInfo:Ping"
    , "TAGame.Ball_TA:HitTeamNum"
    , "TAGame.CameraSettingsActor_TA:CameraPitch"
    , "TAGame.CameraSettingsActor_TA:CameraYaw"
    , "TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount"
    , "TAGame.CarComponent_TA:ReplicatedActive"
    , "TAGame.GameEvent_Soccar_TA:ReplicatedScoredOnTeam"
    , "TAGame.GameEvent_TA:ReplicatedStateIndex"
    , "TAGame.PRI_TA:CameraPitch"
    , "TAGame.PRI_TA:CameraYaw"
    , "TAGame.Vehicle_TA:ReplicatedSteer"
    , "TAGame.Vehicle_TA:ReplicatedThrottle"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are camera settings.
camSettingsProperties :: Set.Set Text.Text
camSettingsProperties =
    [ "TAGame.CameraSettingsActor_TA:ProfileSettings"
    , "TAGame.PRI_TA:CameraSettings"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are demolitions.
demolishProperties :: Set.Set Text.Text
demolishProperties =
    [ "TAGame.Car_TA:ReplicatedDemolish"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are enumerations.
enumProperties :: Set.Set Text.Text
enumProperties =
    [ "Engine.Actor:Role"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are explosions.
explosionProperties :: Set.Set Text.Text
explosionProperties =
    [ "TAGame.Ball_TA:ReplicatedExplosionData"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are flagged integers.
flaggedIntProperties :: Set.Set Text.Text
flaggedIntProperties =
    [ "Engine.Actor:Owner"
    , "Engine.Actor:ReplicatedCollisionType"
    , "Engine.GameReplicationInfo:GameClass"
    , "Engine.Pawn:PlayerReplicationInfo"
    , "Engine.PlayerReplicationInfo:Team"
    , "Engine.TeamInfo:TeamIndex"
    , "TAGame.Ball_TA:GameEvent"
    , "TAGame.CameraSettingsActor_TA:PRI"
    , "TAGame.CarComponent_TA:Vehicle"
    , "TAGame.CrowdActor_TA:GameEvent"
    , "TAGame.CrowdActor_TA:ReplicatedOneShotSound"
    , "TAGame.CrowdManager_TA:GameEvent"
    , "TAGame.CrowdManager_TA:ReplicatedGlobalOneShotSound"
    , "TAGame.PRI_TA:PersistentCamera"
    , "TAGame.PRI_TA:ReplicatedGameEvent"
    , "TAGame.Team_TA:GameEvent"
    , "TAGame.Team_TA:LogoData"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are floats.
floatProperties :: Set.Set Text.Text
floatProperties =
    [ "Engine.Actor:DrawScale"
    , "TAGame.Ball_TA:ReplicatedAddedCarBounceScale"
    , "TAGame.Ball_TA:ReplicatedBallMaxLinearSpeedScale"
    , "TAGame.Ball_TA:ReplicatedBallScale"
    , "TAGame.Ball_TA:ReplicatedWorldBounceScale"
    , "TAGame.CarComponent_Boost_TA:BoostModifier"
    , "TAGame.CarComponent_Boost_TA:RechargeDelay"
    , "TAGame.CarComponent_Boost_TA:RechargeRate"
    , "TAGame.CarComponent_FlipCar_TA:FlipCarTime"
    , "TAGame.CrowdActor_TA:ModifiedNoise"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are game modes.
gameModeProperties :: Set.Set Text.Text
gameModeProperties =
    [ "TAGame.GameEvent_TA:GameMode"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are integers.
intProperties :: Set.Set Text.Text
intProperties =
    [ "Engine.PlayerReplicationInfo:PlayerID"
    , "Engine.PlayerReplicationInfo:Score"
    , "Engine.TeamInfo:Score"
    , "ProjectX.GRI_X:ReplicatedGameMutatorIndex"
    , "ProjectX.GRI_X:ReplicatedGamePlaylist"
    , "TAGame.CrowdActor_TA:ReplicatedCountDownNumber"
    , "TAGame.GameEvent_Soccar_TA:RoundNum"
    , "TAGame.GameEvent_Soccar_TA:SecondsRemaining"
    , "TAGame.GameEvent_TA:BotSkill"
    , "TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining"
    , "TAGame.GameEvent_TA:ReplicatedStateName"
    , "TAGame.GameEvent_Team_TA:MaxTeamSize"
    , "TAGame.PRI_TA:MatchAssists"
    , "TAGame.PRI_TA:MatchGoals"
    , "TAGame.PRI_TA:MatchSaves"
    , "TAGame.PRI_TA:MatchScore"
    , "TAGame.PRI_TA:MatchShots"
    , "TAGame.PRI_TA:Title"
    , "TAGame.PRI_TA:TotalXP"
    , "TAGame.Team_Soccar_TA:GameScore"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are online loadouts.
loadoutOnlineProperties :: Set.Set Text.Text
loadoutOnlineProperties =
    [ "TAGame.PRI_TA:ClientLoadoutOnline"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are loadouts.
loadoutProperties :: Set.Set Text.Text
loadoutProperties =
    [ "TAGame.PRI_TA:ClientLoadout"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are locations.
locationProperties :: Set.Set Text.Text
locationProperties =
    [ "Engine.Actor:RelativeLocation"
    , "TAGame.CarComponent_Dodge_TA:DodgeTorque"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are music stingers.
musicStingerProperties :: Set.Set Text.Text
musicStingerProperties =
    [ "TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are pickups.
pickupProperties :: Set.Set Text.Text
pickupProperties =
    [ "TAGame.VehiclePickup_TA:ReplicatedPickupData"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are private match settings.
privateMatchSettingsProperties :: Set.Set Text.Text
privateMatchSettingsProperties =
    [ "TAGame.GameEvent_SoccarPrivate_TA:MatchSettings"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are Q words, whatever those are.
qWordProperties :: Set.Set Text.Text
qWordProperties =
    [ "ProjectX.GRI_X:GameServerID"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are relation rotations.
relativeRotationProperties :: Set.Set Text.Text
relativeRotationProperties =
    [ "Engine.Actor:RelativeRotation"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are reservations.
reservationProperties :: Set.Set Text.Text
reservationProperties =
    [ "ProjectX.GRI_X:Reservations"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are rigid body states.
rigidBodyStateProperties :: Set.Set Text.Text
rigidBodyStateProperties =
    [ "TAGame.RBActor_TA:ReplicatedRBState"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are strings.
stringProperties :: Set.Set Text.Text
stringProperties =
    [ "Engine.GameReplicationInfo:ServerName"
    , "Engine.PlayerReplicationInfo:PlayerName"
    , "Engine.PlayerReplicationInfo:RemoteUserData"
    , "TAGame.GRI_TA:NewDedicatedServerIP"
    , "TAGame.Team_TA:CustomTeamName"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are team paints.
teamPaintProperties :: Set.Set Text.Text
teamPaintProperties =
    [ "TAGame.Car_TA:TeamPaint"
    ] & map Text.pack & Set.fromList

-- | A set of properties that are unique IDs.
uniqueIdProperties :: Set.Set Text.Text
uniqueIdProperties =
    [ "Engine.PlayerReplicationInfo:UniqueId"
    -- , "TAGame.PRI_TA:PartyLeader"
    ] & map Text.pack & Set.fromList

-- * Garage

-- | A one-to-one mapping between body IDs and their names.
bodies :: Bimap.Bimap Int Text.Text
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
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between decal IDs and their names.
decals :: Bimap.Bimap Int Text.Text
decals =
    [ ( "Backfire",
        [ ("Flames", 288)
        , ("Lightning", 289)
        , ("Skulls", 290)
        , ("Stars", 291)
        , ("Stripes", 292)
        , ("Tech", 293)
        , ("Wings", 294)
        ])
    , ( "Breakout",
        [ ("Flames", 295)
        , ("Lightning", 296)
        , ("Skulls", 297)
        , ("Stars", 298)
        , ("Stripes", 299)
        , ("Tech", 300)
        , ("Wings", 301)
        ])
    , ( "Dominus",
        [ ("Flames", 504)
        , ("Scorpions", 506) -- Skorpion
        , ("Skulls", 507)
        , ("Stripes", 508)
        , ("Tats", 509) -- Tribal
        , ("Wings", 510)
        ])
    , ( "Gizmo",
        [ ("Flames", 323)
        , ("Lightning", 324)
        , ("Skulls", 325)
        , ("Stars", 326)
        , ("Stripes", 327)
        , ("Tech", 328)
        , ("Wings", 329)
        ])
    , ( "Grog",
        [ ("Bomber", 674)
        , ("Cyclops", 694)
        , ("Lepus", 675) -- Bunny
        , ("Stripes", 687)
        , ("Tagged", 695)
        , ("Tribal", 696)
        ])
    , ( "Hotshot",
        [ ("Flames", 337)
        , ("Lightning", 338)
        , ("Skulls", 339)
        , ("Stars", 340)
        , ("Stripes", 341)
        , ("Tech", 342)
        , ("Wings", 343)
        ])
    , ( "Merc",
        [ ("Dots", 344)
        , ("Flames", 345)
        , ("Lightning", 346)
        , ("Skulls", 347)
        , ("Stars", 348)
        , ("Stripes", 349)
        , ("Wings", 350)
        ])
    , ( "Octane",
        [ ("Flames", 302)
        , ("Lightning", 303)
        , ("Skulls", 304)
        , ("Stars", 305)
        , ("Stripes", 306)
        , ("Tech", 307)
        , ("Wings", 308)
        ])
    , ( "Paladin",
        [ ("Flames", 309)
        , ("Lightning", 310)
        , ("Skulls", 311)
        , ("Stars", 312)
        , ("Stripes", 313)
        , ("Tech", 314)
        , ("Wings", 315)
        ])
    , ( "Ripper",
        [ ("Bomber", 701) -- Sandgirl
        , ("Flames", 669)
        , ("Ockie", 700) -- Getkraken
        , ("Shot Fox", 670) -- Kitsune
        , ("Spikes", 671)
        , ("Tribal", 672)
        ])
    , ( "Road Hog",
        [ ("Flames", 316)
        , ("Lightning", 317)
        , ("Skulls", 318)
        , ("Stars", 319)
        , ("Stripes", 320)
        , ("Tech", 321) -- TechSplat
        , ("Wings", 322)
        ])
    , ( "Scarab",
        [ ("Bomani", 585) -- Clouds
        , ("Derby Girl", 592) -- RollerDerby
        , ("Flames", 591)
        , ("Hearts", 586)
        , ("Tiger", 587)
        , ("Tribal", 588)
        ])
    , ( "Takumi",
        [ ("Chaser", 503) -- Wisp
        , ("Copycat", 497) -- Gato
        , ("Crazy-8", 502) -- Technologic
        , ("Gaki", 499) -- Ohai
        , ("Reiko", 498) -- Nitrous
        , ("Stripes", 501)
        ])
    , ( "Venom",
        [ ("Dots", 351)
        , ("Flames", 352)
        , ("Lightning", 353)
        , ("Skulls", 354)
        , ("Stripes", 355)
        , ("Tagged", 356)
        , ("Wings", 357)
        ])
    , ( "X-Devil",
        [ ("Flames", 330)
        , ("Lightning", 331)
        , ("Skulls", 332)
        , ("Stars", 333)
        , ("Stripes", 334)
        , ("Tech", 335)
        , ("Wings", 336)
        ])
    , ( "Zippy",
        [ ("Caboodle", 589) -- HeckTec
        , ("Callous", 590) -- MeanMachine
        , ("Flames", 583)
        , ("Hearts", 581)
        , ("Leopard", 584)
        , ("Tiger", 582)
        ])
    ]
        & map (\ (body, values) -> values &
            map (\ (v, k) -> (k, v ++ " [" ++ body ++ "]")))
        & concat
        & map (\ (k, v) -> (k, Text.pack v))
        & Bimap.fromList
        & Bimap.insert 0 (Text.pack "None")

-- | A one-to-one mapping between wheels IDs and their names.
wheels :: Bimap.Bimap Int Text.Text
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
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between rocket trail IDs and their names.
rocketTrails :: Bimap.Bimap Int Text.Text
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
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between antenna IDs and their names.
antennas :: Bimap.Bimap Int Text.Text
antennas =
    [ ("None", 0)
    , ("(Beta Reward) Gold Nugget", 6)
    , ("8-Ball", 1)
    , ("Alien", 753)
    , ("Balloon Dog", 752)
    , ("Batman", 804)
    , ("Blue Chequered Flag", 284)
    , ("Bomb Pole", 614)
    , ("Calavera", 574) -- DOTDSkull
    , ("Camo Flag", 285)
    , ("Candle", 757)
    , ("Candy Cane", 649)
    , ("Chick Magnet", 392)
    , ("Cupcake", 393)
    , ("Dave's Bread", 391)
    , ("deadmau5", 781) -- Mau5
    , ("Disco Ball", 800)
    , ("Disconnect", 394)
    , ("Dollar Sign", 3)
    , ("Donut", 395)
    , ("Foam Finger", 398)
    , ("Fuzzy Brute", 593) -- FurballFrakenstein
    , ("Fuzzy Vamp", 595) -- FurballVampire
    , ("Genie Lamp", 798)
    , ("Gingerbread Man", 5)
    , ("Heart", 7)
    , ("Holiday Gift", 650) -- ChristmasGift
    , ("Horseshoe", 8)
    , ("Hula Girl", 762)
    , ("Jolly Roger", 217) -- Pirate
    , ("Lightning Bolt", 9)
    , ("Moai", 522)
    , ("Parrot", 750)
    , ("Peace", 11)
    , ("Piñata", 400)
    , ("Planetoid", 12) -- Saturn
    , ("Radioactive", 615) -- RadiationSymbol
    , ("Rainbow", 490)
    , ("Reddit Snoo", 629)
    , ("Retro Ball - Urban", 575)
    , ("Retro Ball - Utopia", 576)
    , ("Retro Ball - Wasteland", 577)
    , ("Rocket", 755)
    , ("Rose", 785)
    , ("Rubber Duckie", 779)
    , ("Safety Flag", 286) -- OrangeNylon
    , ("Skull", 13)
    , ("Smiley", 14)
    , ("Snowman", 15)
    , ("Soccer Ball", 16)
    , ("Star", 17)
    , ("Sunflower", 786)
    , ("Superman", 805)
    , ("Sweet Tooth", 18)
    , ("Tennis Ball", 19)
    , ("The Game Awards - Statue", 655) -- VGA_Statue
    , ("UFO", 20)
    , ("Venus Flytrap", 788)
    , ("Waffle", 401)
    , ("White Flag", 491)
    , ("Wonder Woman", 806)
    -- TODO: What are these called in game?
    , ("Community", 527)
    , ("ESL_Champ", 537)
    , ("Eyeball", 396)
    , ("Moderator", 528)
    , ("PanicButton", 829)
    , ("Psyonix", 287)
    , ("TourneyChamp", 539)
    , ("Translator", 530)
    -- Community Flags
    , ("9GAG", 799)
    , ("AbleGamers", 814)
    , ("Achievement Hunter", 610)
    , ("Angry Army", 548) -- AgryJoe
    , ("Anne Munition", 531)
    , ("Day[9]TV", 654)
    , ("Destructoid", 724)
    , ("EMD1", 532) -- EatMyDiction
    , ("ESL", 536)
    , ("Funhaus", 611)
    , ("Gamespot", 725)
    , ("Gassy Mexican", 511)
    , ("Gfinity", 732)
    , ("GoldGlove", 728)
    , ("IGN", 802)
    , ("itmeJP", 533)
    , ("LIRIK", 513)
    , ("lolRenaynay", 628)
    , ("MLG", 538)
    , ("NeoGAF", 616)
    , ("NVIDIA", 808)
    , ("Operation Sports", 726)
    , ("PC Gamer", 667)
    , ("Polaris", 608)
    , ("Razer", 573)
    , ("Reddit", 630)
    , ("RL Garage", 645)
    , ("Rocket Beans TV", 869)
    , ("Rooster Teeth", 612)
    , ("Saudi Gamer", 534)
    , ("Serious Gaming", 572)
    , ("Something Awful", 699)
    , ("Spooky1611", 514)
    , ("Team Fat", 220)
    , ("Team PowA", 515)
    , ("The Attack", 535)
    , ("The Game Awards - Logo", 693) -- VGX
    , ("Twitch", 221)
    , ("YouTube Gaming", 516)
    -- Country Flags
    , ("Afghanistan", 407)
    , ("Albania", 408)
    , ("Algeria", 124)
    , ("American Samoa", 409)
    , ("Andorra", 830)
    , ("Angola", 410)
    , ("Anguilla", 831)
    , ("Antigua & Barbuda", 832)
    , ("Argentina", 125)
    , ("Armenia", 411)
    , ("Aruba", 833)
    , ("Australia", 126)
    , ("Austria", 127)
    , ("Azerbaijan", 412)
    , ("Bahamas", 834)
    , ("Bahrain", 550)
    , ("Bangladesh", 413)
    , ("Barbados", 835)
    , ("Belarus", 414)
    , ("Belgium", 128)
    , ("Belize", 551)
    , ("Benin", 415)
    , ("Bermuda", 836)
    , ("Bhutan", 552)
    , ("Bolivia", 416)
    , ("Bosnia and Herzegovina", 129)
    , ("Botswana", 553)
    , ("Brazil", 130)
    , ("British Virgin Islands", 837)
    , ("Bulgaria", 131)
    , ("Burkina Faso", 417)
    , ("Burma", 418)
    , ("Burundi", 419)
    , ("Cambodia", 420)
    , ("Cameroon", 132)
    , ("Canada", 133)
    , ("Cape Verde Islands", 134)
    , ("Cayman Islands", 838)
    , ("Central African Republic", 540)
    , ("Chad", 422)
    , ("Chile", 135)
    , ("China", 136)
    , ("Chinese Taipei", 198)
    , ("Colombia", 137)
    , ("Comoros", 554)
    , ("Congo", 138)
    , ("Congo DR", 423)
    , ("Cook Islands", 839)
    , ("Costa Rica", 139)
    , ("Croatia", 140)
    , ("Cuba", 424)
    , ("Curacao", 840)
    , ("Cyprus", 141)
    , ("Czech Republic", 142)
    , ("Côte d'Ivoire", 165)
    , ("Denmark", 143)
    , ("Djibouti", 555)
    , ("Dominica", 841)
    , ("Dominican Republic", 425)
    , ("East Timor", 556)
    , ("Ecuador", 144)
    , ("Egypt", 426)
    , ("El Salvador", 145)
    , ("England", 146)
    , ("Equatorial Guinea", 147)
    , ("Eritrea", 427)
    , ("Estonia", 428)
    , ("Ethiopia", 429)
    , ("Falkland Islands", 842)
    , ("Faroe Islands", 843)
    , ("Fiji", 557)
    , ("Finland", 148)
    , ("France", 149)
    , ("Gabon", 430)
    , ("Gambia", 558)
    , ("Georgia", 431)
    , ("Germany", 150)
    , ("Ghana", 151)
    , ("Gibraltar", 844)
    , ("Greece", 152)
    , ("Grenada", 863)
    , ("Guam", 432)
    , ("Guatemala", 153)
    , ("Guinea", 154)
    , ("Guinea Bissau", 559)
    , ("Guyana", 560)
    , ("Haiti", 433)
    , ("Honduras", 155)
    , ("Hong Kong", 156)
    , ("Hungary", 157)
    , ("Iceland", 158)
    , ("India", 159)
    , ("Indonesia", 160)
    , ("Iran", 161)
    , ("Iraq", 434)
    , ("Ireland", 162)
    , ("Isle Of Man", 845)
    , ("Israel", 163)
    , ("Italy", 164)
    , ("Jamaica", 166)
    , ("Japan", 167)
    , ("Jordan", 435)
    , ("Kazakhstan", 436)
    , ("Kenya", 168)
    , ("Kiribati", 846)
    , ("Kosovo", 561)
    , ("Kuwait", 437)
    , ("Kyrgyzstan", 438)
    , ("Laos", 439)
    , ("Latvia", 440)
    , ("Lebanon", 441)
    , ("Lesotho", 562)
    , ("Liberia", 442)
    , ("Libya", 443)
    , ("Lithuania", 444)
    , ("Luxembourg", 169)
    , ("Macau", 563)
    , ("Macedonia", 445)
    , ("Madagascar", 446)
    , ("Malawi", 447)
    , ("Malaysia", 170)
    , ("Maldives", 847)
    , ("Mali", 448)
    , ("Malta", 171)
    , ("Marshall Islands", 848)
    , ("Mauritania", 449)
    , ("Mauritius", 564)
    , ("Mexico", 172)
    , ("Micronesia", 849)
    , ("Moldova", 450)
    , ("Mongolia", 451)
    , ("Montenegro", 452)
    , ("Montserrat", 864)
    , ("Morocco", 453)
    , ("Mozambique", 454)
    , ("Namibia", 457)
    , ("Nepal", 458)
    , ("Netherlands", 173)
    , ("New Caledonia", 850)
    , ("New Zealand", 174)
    , ("Nicaragua", 175)
    , ("Nigeria", 176)
    , ("North Korea", 460)
    , ("North Mariana Islands", 456)
    , ("Northern Ireland", 177)
    , ("Norway", 178)
    , ("Oman", 461)
    , ("Palestine", 462)
    , ("Palau", 851)
    , ("Palestine", 462)
    , ("Panama", 180)
    , ("Papua New Guinea", 463)
    , ("Paraguay", 464)
    , ("Peru", 181)
    , ("Philippines", 182)
    , ("Poland", 183)
    , ("Portugal", 184)
    , ("Puerto Rico", 465)
    , ("Puntland", 565)
    , ("Qatar", 466)
    , ("Republic of Niger", 459)
    , ("Romania", 185)
    , ("Russia", 186)
    , ("Rwanda", 467)
    , ("Réunion", 566)
    , ("Saint Kitts & Nevis", 852)
    , ("Saint Lucia", 853)
    , ("Saint Vincent", 854)
    , ("Samoa", 855)
    , ("San Marino", 856)
    , ("Sao Tome", 857)
    , ("Saudi Arabia", 468)
    , ("Scotland", 187)
    , ("Senegal", 188)
    , ("Serbia", 189)
    , ("Seychelles", 858)
    , ("Sierra Leone", 469)
    , ("Singapore", 190)
    , ("Slovakia", 191)
    , ("Slovenia", 192)
    , ("Solomon Islands", 567)
    , ("Somalia", 470)
    , ("Somaliland", 568)
    , ("South Africa", 193)
    , ("South Korea", 194)
    , ("South Sudan", 471)
    , ("Spain", 195)
    , ("Sri Lanka", 389)
    , ("Sudan", 472)
    , ("Suriname", 569)
    , ("Swaziland", 570)
    , ("Sweden", 196)
    , ("Switzerland", 197)
    , ("Syria", 473)
    , ("Tahiti", 859)
    , ("Tajikistan", 474)
    , ("Tanzania", 475)
    , ("Thailand", 199)
    , ("Togo", 476)
    , ("Tonga", 730)
    , ("Trinidad and Tobago", 477)
    , ("Tunisia", 200)
    , ("Turkey", 201)
    , ("Turkmenistan", 478)
    , ("Turks & Caicos", 860)
    , ("Uganda", 479)
    , ("Ukraine", 204)
    , ("United Arab Emirates", 202)
    , ("United Kingdom", 203)
    , ("United States", 206)
    , ("Uruguay", 502)
    , ("US Virgin Islands", 480)
    , ("Uzbekistan", 481)
    , ("Vanuatu", 861)
    , ("Vatican City", 862)
    , ("Venezuela", 207)
    , ("Vietnam", 208)
    , ("Wales", 209)
    , ("Western Sahara", 571)
    , ("Yemen", 210)
    , ("Zambia", 482)
    , ("Zimbabwe", 483)
    -- NBA Flags
    , ("NBA", 1263)
    , ("Atlanta Hawks", 1245)
    , ("Boston Celtics", 1246)
    , ("Brooklyn Nets", 1247)
    , ("Charlotte Hornets", 1248)
    , ("Chicago Bulls", 1249)
    , ("Cleveland Cavaliers", 1250)
    , ("Dallas Mavericks", 1251)
    , ("Denver Nuggets", 1252)
    , ("Detroit Pistons", 1253)
    , ("Golden State Warriors", 1254)
    , ("Houston Rockets", 1255)
    , ("Indian Pacers", 1256)
    , ("Los Angeles Clippers", 1257)
    , ("Los Angeles Lakers", 1258)
    , ("Memphis Grizzlies", 1259)
    , ("Miami Heat", 1260)
    , ("Milwaukee Bucks", 1261)
    , ("Minnesota Timberwolves", 1262)
    , ("New Orleans Pelicans", 1264)
    , ("New Tork Knicks", 1265)
    , ("Oklahoma City Thunder", 1266)
    , ("Orlando Magic", 1267)
    , ("Philadelphia 76ers", 1268)
    , ("Phoenix Suns", 1269)
    , ("Portland Trail Blazers", 1270)
    , ("Sacramento Kings", 1271)
    , ("San Antonio Spurs", 1272)
    , ("Toronto Raptors", 1273)
    , ("Utah Jazz", 1274)
    , ("Washington Wizards", 1275)
    -- Video Games
    , ("Blacklight", 385)
    , ("Blacklight: Retribution", 211)
    , ("Chivalry - Agatha Knights", 525)
    , ("Chivalry - Mason Order", 526)
    , ("Edge Of Space", 213)
    , ("Euro Truck Simulator Rig", 813) -- ETS2
    , ("Fallout - Vault Boy", 673)
    , ("Fenix Rage", 216)
    , ("Goat Simulator - G2", 927)
    , ("Goat Simulator - Goatenna", 867)
    , ("Oddworld - Abe", 678)
    , ("Oddworld - Molluck", 679)
    , ("Oddworld - Necrum", 632)
    , ("Oddworld - RuptureFarms", 633)
    , ("Portal - Aperture Laboratories", 656)
    , ("Portal - Cake Sticker", 657)
    , ("Portal - Companion Cube", 681)
    , ("Portal - PotatOS", 698)
    , ("Portal - Wheatley", 686) -- Portal_PC
    , ("Shadowgate", 218)
    , ("Strike Vector EX", 219)
    , ("Unreal", 222)
    , ("Unreal Frag Center", 485)
    , ("Unreal Tournament", 489) -- UTBlue
    , ("Unreal Tournament (Classic)", 487) -- UT
    , ("Unreal Tournament - Blue", 484) -- Epic_BlueFlag
    , ("Unreal Tournament - Flak Shell", 397)
    , ("Unreal Tournament - Red", 486) -- Epic_RedFlag
    , ("Unreal Tournament 2004", 488)
    , ("Warframe", 223)
    , ("Warframe - Chroma", 682)
    , ("Warframe - Excalibur", 684)
    , ("Warframe - Loki", 683)
    , ("Witcher Medallion", 749)
    , ("Worms W.M.D", 1008)
    , ("Worms W.M.D. Grenade", 1000)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between topper IDs and their names.
toppers :: Bimap.Bimap Int Text.Text
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
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between paint finish IDs and their names.
finishes :: Bimap.Bimap Int Text.Text
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
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- * Misc

-- | A one-to-one mapping between bot IDs and their names.
bots :: Bimap.Bimap Int Text.Text
bots =
    [ ("Astro", 70)
    , ("Bane", 71)
    , ("Beast", 72)
    , ("Blade", 73)
    , ("C-Block", 74)
    , ("Captain", 75)
    , ("Caveman", 76)
    , ("Cougar", 77)
    , ("Devil", 78)
    , ("Diesel", 79)
    , ("Dotty", 80)
    , ("Dude", 81)
    , ("Duke", 82)
    , ("Dynamite", 83)
    , ("Flame", 84)
    , ("Flash", 85)
    , ("Fossil", 86)
    , ("Fury", 87)
    , ("Hawk", 88)
    , ("Hunter", 89)
    , ("Imp", 90)
    , ("Jet", 91)
    , ("JM", 92)
    , ("Kidd", 93)
    , ("Laser", 94)
    , ("Lightning", 95)
    , ("Lucky", 96)
    , ("Middy", 97)
    , ("Mountain", 98)
    , ("Nitro", 99)
    , ("Outlaw", 100)
    , ("Phoenix", 101)
    , ("Raja", 102)
    , ("Razor", 103)
    , ("Retro", 104)
    , ("Roundhouse", 105)
    , ("Sabre", 106)
    , ("Samson", 107)
    , ("Savage", 108)
    , ("Shadow", 109)
    , ("Shield", 110)
    , ("Siren", 111)
    , ("Sky", 112)
    , ("Squall", 113)
    , ("Tank", 114)
    , ("Thunder", 115)
    , ("Titan", 116)
    , ("Turbo", 117)
    , ("Tusk", 118)
    , ("Viper", 119)
    , ("Warrior", 120)
    , ("Wolf", 121)
    , ("Yuri", 122)
    , ("Zap", 123)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between logo IDs and their names.
logos :: Bimap.Bimap Int Text.Text
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
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList
