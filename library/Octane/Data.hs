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
        , ( "TAGame.Car_Season_TA",
            [ "Archetypes.GameEvent.GameEvent_Season:CarArchetype"
            ])
        , ( "TAGame.Car_TA",
            [ "Archetypes.Car.Car_Default"
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
        , ( "TAGame.Team_TA",
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
    , "TAGame.Team_TA"
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

-- TODO
todo :: Int
todo = -1

-- | A one-to-one mapping between body IDs and their names.
bodies :: Bimap.Bimap Int Text.Text
bodies =
    [ ("Backfire", 21)
    , ("Batmobile", 803)
    , ("Breakout", 22)
    , ("DeLorean Time Machine", 597)
    , ("Dominus", 403)
    , ("Gizmo", 26)
    , ("Grog", 607)
    , ("Hotshot", 29)
    , ("Merc", 30)
    , ("Octane", 23)
    , ("Paladin", 24)
    , ("Ripper", 600)
    , ("Road Hog", 25)
    , ("Scarab", 404)
    , ("Takumi", 402)
    , ("Venom", 31)
    , ("X-Devil", 28)
    , ("Zippy", 523)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between decal IDs and their names.
decals :: Bimap.Bimap Int Text.Text
decals =
    [ ( "Backfire",
        [ ("Flames", 295)
        , ("Lightning", 324)
        , ("Skulls", 339)
        , ("Stars", 291)
        , ("Stripes", 313)
        , ("Tech", 321)
        , ("Wings", 336)
        ])
    , ( "Breakout",
        [ ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Tech", todo)
        , ("Wings", todo)
        ])
    , ( "Dominus",
        [ ("Flames", 504)
        , ("Scorpions", 506)
        , ("Skulls", todo)
        , ("Stripes", todo)
        , ("Tats", todo)
        , ("Wings", todo)
        ])
    , ( "Gizmo",
        [ ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Tech", todo)
        , ("Wings", todo)
        ])
    , ( "Grog",
        [ ("Bomber", 674)
        , ("Cyclops", todo)
        , ("Lepus", todo)
        , ("Stripes", todo)
        , ("Tagged", todo)
        , ("Tribal", 696)
        ])
    , ( "Hotshot",
        [ ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Tech", todo)
        , ("Wings", todo)
        ])
    , ( "Merc",
        [ ("Dots", todo)
        , ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Wings", todo)
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
        [ ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Tech", todo)
        , ("Wings", todo)
        ])
    , ( "Ripper",
        [ ("Bomber", 701)
        , ("Flames", todo)
        , ("Ockie", todo)
        , ("Shot Fox", todo)
        , ("Spikes", todo)
        , ("Tribal", 672)
        ])
    , ( "Road Hog",
        [ ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Tech", todo)
        , ("Wings", todo)
        ])
    , ( "Scarab",
        [ ("Bomani", 585)
        , ("Derby Girl", todo)
        , ("Flames", todo)
        , ("Hearts", todo)
        , ("Tiger", todo)
        , ("Tribal", 588)
        ])
    , ( "Takumi",
        [ ("Chaser", 503)
        , ("Copycat", todo)
        , ("Crazy-8", todo)
        , ("Gaki", todo)
        , ("Reiko", todo)
        , ("Stripes", 501)
        ])
    , ( "Venom",
        [ ("Dots", todo)
        , ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stripes", todo)
        , ("Tagged", todo)
        , ("Wings", todo)
        ])
    , ( "X-Devil",
        [ ("Flames", todo)
        , ("Lightning", todo)
        , ("Skulls", todo)
        , ("Stars", todo)
        , ("Stripes", todo)
        , ("Tech", todo)
        , ("Wings", todo)
        ])
    , ( "Zippy",
        [ ("Caboodle", 589)
        , ("Callous", todo)
        , ("Flames", todo)
        , ("Hearts", todo)
        , ("Leopard", todo)
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
    [ ("Alchemist", 374)
    , ("Almas", 364)
    , ("Batmobile", 874)
    , ("Bender", 369)
    , ("Carriage", 549)
    , ("Cristiano", 386)
    , ("DeLorean Time Machine", 609)
    , ("Dieci", 363)
    , ("Falco", 382)
    , ("Foreman", 372)
    , ("Grog", 613)
    , ("Invader", 380)
    , ("Lowrider", 369)
    , ("Lucci", 361)
    , ("Mountaineer", 375)
    , ("Neptune", 370)
    , ("Octavian", 379)
    , ("OEM", 376)
    , ("Rat Rod", 366)
    , ("Ripper", 690)
    , ("Scarab", 540)
    , ("Servergate", 519)
    , ("Spinner", 388)
    , ("Spyder", 371)
    , ("Stallion", 365)
    , ("Stern", 368)
    , ("Sunburt", 383)
    , ("Tempest", 377)
    , ("Tomahawk", 362)
    , ("Trahere", 367)
    , ("Tunica", todo)
    , ("Veloce", todo)
    , ("Vortex", todo)
    , ("Zippy", todo)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between rocket trail IDs and their names.
rocketTrails :: Bimap.Bimap Int Text.Text
rocketTrails =
    [ ("Accelerato", 524)
    , ("Batmobile", 868)
    , ("Battle-Stars", 545)
    , ("Bubbles", 33)
    , ("Burnout", 387)
    , ("Candy Corn", 578)
    , ("Confetti", 34)
    , ("Datastream", 35)
    , ("Flamethrower", 36)
    , ("Flamethrower Blue", 37)
    , ("Flamethrower Green", 38)
    , ("Flamethrower Pink", 39)
    , ("Flamethrower Purple", 40)
    , ("Flamethrower Red", 41)
    , ("Flowers", 42)
    , ("Grass", 43)
    , ("Hydro", 69)
    , ("Ion Blue", 45)
    , ("Ion Green", 46)
    , ("Ion Pink", 47)
    , ("Ion Purple", 48)
    , ("Ion Red", 49)
    , ("Ion Yellow", 50)
    , ("Money", 51)
    , ("Nitrous", 384)
    , ("Nuts & Bolts", 544)
    , ("OutaTime", 626)
    , ("Plasma", 52)
    , ("Portal - Conversion Gel", todo)
    , ("Portal - Propulsion Gel", todo)
    , ("Portal - Reuplsion Gel", todo)
    , ("Rainbow", todo)
    , ("Sacred", 44)
    , ("Sandstorm", todo)
    , ("Slime", todo)
    , ("Snowflakes", todo)
    , ("Sparkles", todo)
    , ("Standard Blue", todo)
    , ("Standard Pink", todo)
    , ("Standard Purple", todo)
    , ("Standard Red", todo)
    , ("Standard Red", 63)
    , ("Standard Yellow", todo)
    , ("Thermal", todo)
    , ("Thermal Blue", todo)
    , ("Thermal Green", todo)
    , ("Thermal Pink", todo)
    , ("Thermal Purple", todo)
    , ("Thermal Yellow", todo)
    , ("Xmas", todo)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between antenna IDs and their names.
antennas :: Bimap.Bimap Int Text.Text
antennas =
    [ ("None", 0)
    , ("8-Ball", 1)
    , ("Alien", 753)
    , ("Balloon Dog", 752)
    , ("Batman", 804)
    , ("Blue Chequered Flag", 284)
    , ("Bomb Pole", 614)
    , ("Calavera", 574)
    , ("Camo Flag", 285)
    , ("Candle", 757)
    , ("Candy Cane", 649)
    , ("Chick Magnet", 392)
    , ("Cupcake", 393)
    , ("Dave's Bread", 391)
    , ("deadmau5", 781)
    , ("Disco Ball", 800)
    , ("Disconnect", 394)
    , ("Dollar Sign", 3)
    , ("Donut", 395)
    , ("Foam Finger", 398)
    , ("Fuzzy Brute", 593)
    , ("Fuzzy Vamp", 595)
    , ("Genie Lamp", 798)
    , ("Gingerbread Man", 5)
    , ("Heart", 7)
    , ("Holiday Gift", 650)
    , ("Horseshoe", todo)
    , ("Hula Girl", todo)
    , ("Jolly Roger", todo)
    , ("Lightning Bolt", todo)
    , ("Moai", todo)
    , ("Parrot", todo)
    , ("Peace", todo)
    , ("Piñata", todo)
    , ("Planetoid", todo)
    , ("Radioactive", todo)
    , ("Reddit Snoo", todo)
    , ("Retro Ball - Urban", todo)
    , ("Retro Ball - Utopia", todo)
    , ("Retro Ball - Wasteland", todo)
    , ("Rocket", todo)
    , ("Rose", todo)
    , ("Rubber Duckie", todo)
    , ("Safety Flag", todo)
    , ("Skull", todo)
    , ("Smiley", todo)
    , ("Snowman", todo)
    , ("Soccer Ball", todo)
    , ("Star", todo)
    , ("Sunflower", todo)
    , ("Superman", todo)
    , ("Tennis Ball", todo)
    , ("The Game Awards - Statue", todo)
    , ("UFO", todo)
    , ("Venus Flytrap", todo)
    , ("Waffle", todo)
    , ("White Flag", todo)
    , ("Wonder Woman", 806)
    -- Community Flags
    , ("9GAG", todo)
    , ("AbleGamers", todo)
    , ("Achievement Hunter", todo)
    , ("Angry Army", todo)
    , ("Anne Munition", todo)
    , ("Day[9]TV", todo)
    , ("Destructoid", todo)
    , ("EMD1", todo)
    , ("ESL", todo)
    , ("Funhaus", todo)
    , ("Gamespot", todo)
    , ("Gassy Mexican", todo)
    , ("Gfinity", todo)
    , ("GoldGlove", todo)
    , ("IGN", todo)
    , ("itmeJP", todo)
    , ("LIRIK", todo)
    , ("lolRenaynay", todo)
    , ("MLG", todo)
    , ("NeoGAF", todo)
    , ("NVIDIA", todo)
    , ("Operation Sports", todo)
    , ("PC Gamer", todo)
    , ("Polaris", todo)
    , ("Razer", todo)
    , ("Reddit", todo)
    , ("RL Garage", todo)
    , ("Rocket Beans TV", todo)
    , ("Rooster Teeth", todo)
    , ("Saudi Gamer", todo)
    , ("Serious Gaming", todo)
    , ("Something Awful", todo)
    , ("Spooky1611", todo)
    , ("Team Fat", todo)
    , ("Team PowA", todo)
    , ("The Attack", todo)
    , ("The Game Awards - Logo", todo)
    , ("Twitch", todo)
    , ("YouTube Gaming", todo)
    -- Country Flags
    , ("Afghanistan", todo)
    , ("Albania", todo)
    , ("Algeria", todo)
    , ("American Samoa", todo)
    , ("Andorra", todo)
    , ("Angola", todo)
    , ("Anguilla", todo)
    , ("Antigua & Barbuda", todo)
    , ("Argentina", todo)
    , ("Armenia", todo)
    , ("Aruba", todo)
    , ("Australia", todo)
    , ("Austria", todo)
    , ("Azerbaijan", todo)
    , ("Bahamas", todo)
    , ("Bahrain", todo)
    , ("Bangladesh", todo)
    , ("Barbados", todo)
    , ("Belarus", todo)
    , ("Belgium", todo)
    , ("Belize", todo)
    , ("Benin", todo)
    , ("Bermuda", todo)
    , ("Bhutan", todo)
    , ("Bolivia", todo)
    , ("Bosnia and Herzegovina", todo)
    , ("Botswana", todo)
    , ("Brazil", todo)
    , ("British Virgin Islands", todo)
    , ("Bulgaria", todo)
    , ("Burkina Faso", todo)
    , ("Burma", todo)
    , ("Burundi", todo)
    , ("Cambodia", todo)
    , ("Cameroon", todo)
    , ("Canada", todo)
    , ("Cape Verde Islands", todo)
    , ("Cayman Islands", todo)
    , ("Central African Republic", todo)
    , ("Chad", todo)
    , ("Chile", todo)
    , ("China", todo)
    , ("Chinese Taipei", todo)
    , ("Colombia", todo)
    , ("Comoros", todo)
    , ("Congo", todo)
    , ("Congo DR", todo)
    , ("Cook Islands", todo)
    , ("Costa Rica", todo)
    , ("Croatia", todo)
    , ("Cuba", todo)
    , ("Curacao", todo)
    , ("Cyprus", todo)
    , ("Czech Republic", todo)
    , ("Côte d'Ivoire", todo)
    , ("Denmark", todo)
    , ("Djibouti", todo)
    , ("Dominica", todo)
    , ("Dominican Republic", todo)
    , ("East Timor", todo)
    , ("Ecuador", todo)
    , ("Egypt", todo)
    , ("El Salvador", todo)
    , ("England", todo)
    , ("Equatorial Guinea", todo)
    , ("Eritrea", todo)
    , ("Estonia", todo)
    , ("Ethiopia", todo)
    , ("Falkland Islands", todo)
    , ("Faroe Islands", todo)
    , ("Fiji", todo)
    , ("Finland", todo)
    , ("France", todo)
    , ("Gabon", todo)
    , ("Gambia", todo)
    , ("Georgia", todo)
    , ("Germany", todo)
    , ("Ghana", todo)
    , ("Gibraltar", todo)
    , ("Greece", todo)
    , ("Grenada", todo)
    , ("Guam", todo)
    , ("Guatemala", todo)
    , ("Guinea", todo)
    , ("Guinea Bissau", todo)
    , ("Guyana", todo)
    , ("Haiti", todo)
    , ("Honduras", todo)
    , ("Hong Kong", todo)
    , ("Hungary", todo)
    , ("Iceland", todo)
    , ("India", todo)
    , ("Indonesia", todo)
    , ("Iran", todo)
    , ("Iraq", todo)
    , ("Ireland", todo)
    , ("Isle Of Man", todo)
    , ("Israel", todo)
    , ("Italy", todo)
    , ("Jamaica", todo)
    , ("Japan", todo)
    , ("Jordan", todo)
    , ("Kazakhstan", todo)
    , ("Kenya", todo)
    , ("Kiribati", todo)
    , ("Kosovo", todo)
    , ("Kuwait", todo)
    , ("Kyrgyzstan", todo)
    , ("Laos", todo)
    , ("Latvia", todo)
    , ("Lebanon", todo)
    , ("Lesotho", todo)
    , ("Liberia", todo)
    , ("Libya", todo)
    , ("Lithuania", todo)
    , ("Luxembourg", todo)
    , ("Macau", todo)
    , ("Macedonia", todo)
    , ("Madagascar", todo)
    , ("Malawi", todo)
    , ("Malaysia", todo)
    , ("Maldives", todo)
    , ("Mali", todo)
    , ("Malta", todo)
    , ("Marshall Islands", todo)
    , ("Mauritania", todo)
    , ("Mauritius", todo)
    , ("Mexico", todo)
    , ("Micronesia", todo)
    , ("Moldova", todo)
    , ("Mongolia", todo)
    , ("Montenegro", todo)
    , ("Montserrat", todo)
    , ("Morocco", todo)
    , ("Mozambique", todo)
    , ("Namibia", todo)
    , ("Nepal", todo)
    , ("Netherlands", todo)
    , ("New Caledonia", todo)
    , ("New Zealand", todo)
    , ("Nicaragua", todo)
    , ("Nigeria", todo)
    , ("North Korea", todo)
    , ("North Mariana Islands", todo)
    , ("Northern Ireland", todo)
    , ("Norway", todo)
    , ("Oman", todo)
    , ("Palestine", todo)
    , ("Palau", todo)
    , ("Palestine", todo)
    , ("Panama", todo)
    , ("Papua New Guinea", todo)
    , ("Paraguay", todo)
    , ("Peru", todo)
    , ("Philippines", todo)
    , ("Poland", todo)
    , ("Portugal", todo)
    , ("Puerto Rico", todo)
    , ("Puntland", todo)
    , ("Qatar", todo)
    , ("Republic of Niger", todo)
    , ("Romania", todo)
    , ("Russia", todo)
    , ("Rwanda", todo)
    , ("Réunion", todo)
    , ("Saint Kitts & Nevis", todo)
    , ("Saint Lucia", todo)
    , ("Saint Vincent", todo)
    , ("Samoa", todo)
    , ("San Marino", todo)
    , ("Sao Tome", todo)
    , ("Saudi Arabia", todo)
    , ("Scotland", todo)
    , ("Senegal", todo)
    , ("Serbia", todo)
    , ("Setchelles", todo)
    , ("Sierra Leone", todo)
    , ("Singapore", todo)
    , ("Slovakia", todo)
    , ("Slovenia", todo)
    , ("Solomon Islands", todo)
    , ("Somalia", todo)
    , ("Somaliland", todo)
    , ("South Africa", todo)
    , ("South Korea", todo)
    , ("South Sudan", todo)
    , ("Spain", todo)
    , ("Sri Lanka", todo)
    , ("Sudan", todo)
    , ("Suriname", todo)
    , ("Swaziland", todo)
    , ("Sweden", todo)
    , ("Switzerland", todo)
    , ("Syria", todo)
    , ("Tahiti", todo)
    , ("Tajikistan", todo)
    , ("Tanzania", todo)
    , ("Thailand", todo)
    , ("Togo", todo)
    , ("Tonga", todo)
    , ("Trinidad and Tobago", todo)
    , ("Tunisia", todo)
    , ("Turkey", todo)
    , ("Turkmenistan", todo)
    , ("Turks & Caicos", todo)
    , ("Uganda", todo)
    , ("Ukraine", todo)
    , ("United Arab Emirates", todo)
    , ("United Kingdom", todo)
    , ("United States", todo)
    , ("Uruguay", todo)
    , ("US Virgin Islands", todo)
    , ("Uzbekistan", todo)
    , ("Vanuatu", todo)
    , ("Vatican City", todo)
    , ("Venezuela", todo)
    , ("Vietnam", todo)
    , ("Wales", todo)
    , ("Western Sahara", todo)
    , ("Yemen", todo)
    , ("Zambia", todo)
    , ("Zimbabwe", todo)
    -- NBA Flags
    , ("NBA", todo)
    , ("Atlanta Hawks", todo)
    , ("Boston Celtics", todo)
    , ("Brooklyn Nets", todo)
    , ("Charlotte Hornets", todo)
    , ("Chicago Bulls", todo)
    , ("Cleveland Cavaliers", todo)
    , ("Dallas Mavericks", todo)
    , ("Denver Nuggets", todo)
    , ("Detroit Pistons", todo)
    , ("Golden State Warriors", todo)
    , ("Houston Rockets", todo)
    , ("Indian Pacers", todo)
    , ("Los Angeles Clippers", todo)
    , ("Los Angeles Lakers", todo)
    , ("Memphis Grizzlies", todo)
    , ("Miami Heat", todo)
    , ("Milwaukee Bucks", todo)
    , ("Minnesota Timberwolves", todo)
    , ("New Orleans Pelicans", todo)
    , ("New Tork Knicks", todo)
    , ("Oklahoma City Thunder", todo)
    , ("Orlando Magic", todo)
    , ("Philadelphia 76ers", todo)
    , ("Phoenix Suns", todo)
    , ("Portland Trail Blazers", todo)
    , ("Sacramento Kings", todo)
    , ("San Antonio Spurs", todo)
    , ("Toronto Raptors", todo)
    , ("Utah Jazz", todo)
    , ("Washington Wizards", todo)
    -- Video Games
    , ("Blacklight", todo)
    , ("Blacklight: Retribution", todo)
    , ("Chivalry - Agatha Knights", todo)
    , ("Chivalry - Mason Order", todo)
    , ("Edge Of Space", todo)
    , ("Euro Truck Simulator Rig", todo)
    , ("Fallout - Vault Boy", todo)
    , ("Fenix Rage", todo)
    , ("Goat Simulator - G2", todo)
    , ("Goat Simulator - Goatenna", todo)
    , ("Oddworld - Abe", todo)
    , ("Oddworld - Molluck", todo)
    , ("Oddworld - Necrum", todo)
    , ("Oddworld - RuptureFarms", todo)
    , ("Portal - Aperture Laboratories", todo)
    , ("Portal - Cake Sticker", todo)
    , ("Portal - Companion Cube", todo)
    , ("Portal - PotatOS", todo)
    , ("Portal - Wheatley", todo)
    , ("Shadowgate", todo)
    , ("Strike Vector EX", todo)
    , ("Unreal", todo)
    , ("Unreal Frag Center", todo)
    , ("Unreal Tournament", todo)
    , ("Unreal Tournament (Classic)", todo)
    , ("Unreal Tournament - Blue", todo)
    , ("Unreal Tournament - Flak Shell", todo)
    , ("Unreal Tournament - Red", todo)
    , ("Unreal Tournament 2004", todo)
    , ("Warframe", todo)
    , ("Warframe - Chroma", todo)
    , ("Warframe - Excalibur", todo)
    , ("Warframe - Loki", todo)
    , ("Witcher Medallion", todo)
    , ("Worms W.M.D", todo)
    , ("Worms W.M.D. Grenade", todo)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between topper IDs and their names.
toppers :: Bimap.Bimap Int Text.Text
toppers =
    [ ("None", 0)
    , ("Antlers", 774)
    , ("Beret", 775)
    , ("Biker Cap", 776)
    , ("Birthday Cake", 789)
    , ("Blitzen", 640)
    , ("Bobby Helmet", 227)
    , ("Boombox", 521)
    , ("Bowler", 745)
    , ("Brodie Helmet", 743)
    , ("Brünnhilde", 241)
    , ("Captain's Hat", 741)
    , ("Cavalier", 580)
    , ("Chainsaw", 795)
    , ("Chef's Hat", 756)
    , ("Cherry Top", 237)
    , ("Christmas Tree", 639)
    , ("Cockroach", 769)
    , ("Cow Skull", 685)
    , ("deadmau5", 796)
    , ("Deerstalker", 792)
    , ("Derby", 790)
    , ("Devil Horns", 229)
    , ("Fez", 230)
    , ("Fire Helmet", 231)
    , ("Foam Hat", 390)
    , ("Fruit Hat", 744)
    , ("Graduation Cap", todo)
    , ("Halo", todo)
    , ("Hard Hat", todo)
    , ("Homburg", todo)
    , ("Locomotive", todo)
    , ("Mariachi Hat", todo)
    , ("Mohawk", 638)
    , ("Mouse Trap", todo)
    , ("Paper Boat", todo)
    , ("Pirate's Hat", todo)
    , ("Pixelated Shades", todo)
    , ("Pizza Topper", todo)
    , ("Plunger", todo)
    , ("Police Hat", todo)
    , ("Portal - Cake", todo)
    , ("Propellerhead", todo)
    , ("Pumpkin", todo)
    , ("Rasta", todo)
    , ("Rhino Horns", todo)
    , ("Royal Crown", todo)
    , ("Sad Sapling", todo)
    , ("Santa", todo)
    , ("Season 1 - Gold", todo)
    , ("Shark Fin", todo)
    , ("Shuriken", todo)
    , ("Sombrero", todo)
    , ("Taxi Topper", todo)
    , ("Tiara", todo)
    , ("Top Hat", todo)
    , ("Traffic Cone", todo)
    , ("Unicorn", todo)
    , ("Witch's Hat", todo)
    , ("Wizard Hat", todo)
    , ("Work Boot", todo)
    , ("Worms W.M.D.", todo)
    ] & map (\ (v, k) -> (k, Text.pack v)) & Bimap.fromList
