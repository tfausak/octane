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

-- | A one-to-one mapping between body IDs and their names.
bodies :: Bimap.Bimap Int Text.Text
bodies =
    [ (21, "Backfire")
    , (22, "Breakout")
    , (23, "Octane")
    , (24, "Paladin")
    , (25, "Road Hog")
    , (26, "Gizmo")
    -- 27 is conspicuously missing...
    , (28, "X-Devil")
    , (29, "Hotshot")
    , (30, "Merc")
    , (31, "Venom")
    , (402, "Takumi")
    , (403, "Dominus")
    , (404, "Scarab")
    , (523, "Zippy")
    , (597, "DeLorean Time Machine")
    , (600, "Ripper")
    , (607, "Grog")
    , (803, "Batmobile")
    ] & map (\ (k, v) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between decal IDs and their names.
decals :: Bimap.Bimap Int Text.Text
decals = let
    common =
        [ (0, "None")
        , (295, "Flames")
        , (305, "Stars")
        , (313, "Stripes")
        , (321, "Tech")
        , (324, "Lightning")
        , (336, "Wings")
        , (339, "Skulls")
        , (344, "Dots")
        , (356, "Tagged")
        ]
    dominus =
        [ (504, "Flames")
        , (506, "Scorpions")
        -- TODO: This is incomplete.
        ] & map (\ (k, v) -> (k, v ++ " [Dominus]"))
    grog =
        [ (674, "Bomber")
        , (696, "Tribal")
        -- TODO: This is incomplete.
        ] & map (\ (k, v) -> (k, v ++ " [Grog]"))
    ripper =
        [ (672, "Tribal")
        , (701, "Bomber")
        -- TODO: This is incomplete.
        ] & map (\ (k, v) -> (k, v ++ " [Ripper]"))
    scarab =
        [ (585, "Bomani")
        , (588, "Tribal")
        -- TODO: This is incomplete.
        ] & map (\ (k, v) -> (k, v ++ " [Scarab]"))
    takumi =
        [ (501, "Stripes")
        , (503, "Chaser")
        -- TODO: This is incomplete.
        ] & map (\ (k, v) -> (k, v ++ " [Takumi]"))
    zippy =
        [ (582, "Tiger")
        , (589, "Caboodle")
        -- TODO: This is incomplete.
        ] & map (\ (k, v) -> (k, v ++ " [Zippy]"))
    in [common, dominus, grog, ripper, scarab, takumi, zippy]
        & concat
        & map (\ (k, v) -> (k, Text.pack v))
        & Bimap.fromList

-- | A one-to-one mapping between wheels IDs and their names.
wheels :: Bimap.Bimap Int Text.Text
wheels =
    [ (361, "Lucci")
    , (363, "Dieci")
    , (364, "Almas")
    , (366, "Rat Rod")
    , (369, "Bender")
    , (369, "Lowrider")
    , (370, "Neptune")
    , (372, "Foreman")
    , (374, "Alchemist")
    , (375, "Mountaineer")
    , (376, "OEM")
    , (379, "Octavian")
    , (380, "Invader")
    , (382, "Falco")
    , (386, "Cristiano")
    , (388, "Spinner")
    , (519, "Servergate")
    , (540, "Scarab")
    , (549, "Carriage")
    , (609, "DeLorean Time Machine")
    , (613, "Grog")
    , (690, "Ripper")
    , (874, "Batmobile")
    -- TODO: This is incomplete.
    ] & map (\ (k, v) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between rocket trail IDs and their names.
rocketTrails :: Bimap.Bimap Int Text.Text
rocketTrails =
    [ (33, "Bubbles")
    , (34, "Confetti")
    , (35, "Datastream")
    , (36, "Flamethrower")
    , (37, "Flamethrower Blue")
    , (38, "Flamethrower Green")
    , (39, "Flamethrower Pink")
    , (40, "Flamethrower Purple")
    , (41, "Flamethrower Red")
    , (42, "Flowers")
    , (43, "Grass")
    , (45, "Ion Blue")
    , (46, "Ion Green")
    , (47, "Ion Pink")
    , (48, "Ion Purple")
    , (63, "Standard Red")
    , (69, "Hydro")
    , (387, "Burnout")
    , (524, "Accelerato")
    , (545, "Battle-Stars")
    , (578, "Candy Corn")
    , (626, "OutaTime")
    , (868, "Batmobile")
    -- TODO: This is incomplete.
    ] & map (\ (k, v) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between antenna IDs and their names.
antennas :: Bimap.Bimap Int Text.Text
antennas =
    [ (0, "None")
    , (1, "8-Ball")
    , (3, "Dollar Sign")
    , (284, "Blue Chequered Flag")
    , (285, "Camo Flag")
    , (391, "Dave's Bread")
    , (392, "Chick Magnet")
    , (393, "Cupcake")
    , (394, "Disconnect")
    , (395, "Donut")
    , (398, "Foam Finger")
    , (574, "Calavera")
    , (614, "Bomb Pole")
    , (649, "Candy Cane")
    , (752, "Balloon Dog")
    , (753, "Alien")
    , (757, "Candle")
    , (781, "deadmau5")
    , (800, "Disco Ball")
    , (804, "Batman")
    -- TODO: This is incomplete.
    ] & map (\ (k, v) -> (k, Text.pack v)) & Bimap.fromList

-- | A one-to-one mapping between topper IDs and their names.
toppers :: Bimap.Bimap Int Text.Text
toppers =
    [ (0, "None")
    , (227, "Bobby Helmet")
    , (237, "Cherry Top")
    , (241, "Brünnhilde")
    , (521, "Boombox")
    , (580, "Cavalier")
    , (639, "Christmas Tree")
    , (640, "Blitzen")
    , (685, "Cow Skull")
    , (741, "Captain's Hat")
    , (743, "Brodie Helmet")
    , (745, "Bowler")
    , (756, "Chef's Hat")
    , (769, "Cockroach")
    , (774, "Antlers")
    , (775, "Beret")
    , (776, "Biker Cap")
    , (789, "Birthday Cake")
    , (792, "Deerstalker")
    , (795, "Chainsaw")
    , (796, "deadmau5")
    -- TODO: This is incomplete.
    ] & map (\ (k, v) -> (k, Text.pack v)) & Bimap.fromList
