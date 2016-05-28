module Octane.Parser.ClassPropertyMap where

import Data.Function ((&))
import Debug.Trace

import qualified Control.Newtype as Newtype
import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Octane.Type as Type

-- | The class property map is a map from class IDs in the stream to a map from
-- | property IDs in the stream to property names.
getClassPropertyMap :: Type.Replay -> IntMap.IntMap (IntMap.IntMap Text.Text)
getClassPropertyMap replay = let
    basicClassPropertyMap = getBasicClassPropertyMap replay
    classMap = getClassMap replay
    in replay
        & getClassIds
        & map (\ classId -> let
            ownProperties = case IntMap.lookup classId basicClassPropertyMap of
                Nothing -> IntMap.empty
                Just x -> x
            parentProperties = case IntMap.lookup classId classMap of
                Nothing -> IntMap.empty
                Just parentClassIds -> parentClassIds
                    & map (\ parentClassId ->
                        case IntMap.lookup parentClassId basicClassPropertyMap of
                            Nothing -> IntMap.empty
                            Just x -> x)
                    & IntMap.unions
            properties = IntMap.union ownProperties parentProperties
            in (classId, properties))
        & IntMap.fromList

-- | The class cache is a list of 3-tuples where the first element is a class
-- | ID, the second is its cache ID, and the third is its parent's cache ID.
getClassCache :: Type.Replay -> [(Int, Int, Int)]
getClassCache replay = replay
    & Type.replayCacheItems
    & Newtype.unpack
    & map (\ x ->
        ( x & Type.cacheItemClassId & Newtype.unpack & fromIntegral
        , x & Type.cacheItemCacheId & Newtype.unpack & fromIntegral
        , x & Type.cacheItemParentCacheId & Newtype.unpack & fromIntegral
        ))

-- | The class IDs in a replay. Comes from the class cache.
getClassIds :: Type.Replay -> [Int]
getClassIds replay = replay
    & getClassCache
    & map (\ (x, _, _) -> x)

-- | Gets the parent class ID for the given parent cache ID. This is necessary
-- | because there is not always a class with the given cache ID in the cache.
-- | When that happens, the parent cache ID is decremented and tried again.
getParentClassId :: Int -> [(Int, Int, Int)] -> Maybe Int
getParentClassId parentCacheId xs =
    case dropWhile (\ (_, cacheId, _) -> cacheId /= parentCacheId) xs of
        [] -> if parentCacheId <= 0
            then Nothing
            else getParentClassId (parentCacheId - 1) xs
        (parentClassId, _, _) : _ -> Just parentClassId

-- | The basic class map is a naive mapping from class ID to its parent class
-- | ID. It's naive because it only maps the class ID to its immediate parent.
-- | It does not chase the inheritance all the way down.
getBasicClassMap :: Type.Replay -> IntMap.IntMap Int
getBasicClassMap replay = replay
    & getClassCache
    & reverse
    & List.tails
    & Maybe.mapMaybe (\ xs -> case xs of
        [] -> Nothing
        (classId, _, parentCacheId) : ys -> do
            parentClassId <- getParentClassId parentCacheId ys
            return (classId, parentClassId))
    & IntMap.fromList

-- | Given a naive mapping from class ID to its parent class ID, return all of
-- | the parent IDs for a given class.
getParentClassIds :: Int -> IntMap.IntMap Int -> [Int]
getParentClassIds classId basicClassMap =
    case IntMap.lookup classId basicClassMap of
        Nothing -> []
        Just parentClassId -> parentClassId : getParentClassIds parentClassId basicClassMap

-- | The class map is a mapping from a class ID to all of its parent class IDs.
getClassMap :: Type.Replay -> IntMap.IntMap [Int]
getClassMap replay = let
    basicClassMap = getBasicClassMap replay
    in replay
        & getClassIds
        & map (\ classId ->
            ( classId
            , getParentClassIds classId basicClassMap
            ))
        & IntMap.fromList

-- | The property map is a mapping from property IDs to property names.
getPropertyMap :: Type.Replay -> IntMap.IntMap Text.Text
getPropertyMap replay = replay
    & Type.replayObjects
    & Newtype.unpack
    & map Newtype.unpack
    & zip [0 ..]
    & IntMap.fromList

-- | The basic class property map is a naive mapping from class IDs to a
-- | mapping from property IDs to property names. It's naive because it does
-- | not include the properties from the class's parents.
getBasicClassPropertyMap :: Type.Replay -> IntMap.IntMap (IntMap.IntMap Text.Text)
getBasicClassPropertyMap replay = let
    propertyMap = getPropertyMap replay
    in replay
        & Type.replayCacheItems
        & Newtype.unpack
        & map (\ x -> let
            classId = x & Type.cacheItemClassId & Newtype.unpack & fromIntegral
            properties = x
                & Type.cacheItemCacheProperties
                & Newtype.unpack
                & Maybe.mapMaybe (\ y -> let
                    streamId = y & Type.cachePropertyStreamId & Newtype.unpack & fromIntegral
                    propertyId = y & Type.cachePropertyObjectId & Newtype.unpack & fromIntegral
                    in case IntMap.lookup propertyId propertyMap of
                        Nothing -> Nothing
                        Just name -> Just (streamId, name))
                & IntMap.fromList
            in (classId, properties))
        & IntMap.fromList

-- | The actor map is a mapping from class names to their IDs.
getActorMap :: Type.Replay -> Map.Map Text.Text Int
getActorMap replay = replay
    & Type.replayActors
    & Newtype.unpack
    & map (\ x -> let
        className = x & Type.actorName & Newtype.unpack
        classId = x & Type.actorStreamId & Newtype.unpack & fromIntegral
        in (className, classId))
    & Map.fromList

-- | Gets the class ID and name for a given property ID.
getClass
    :: IntMap.IntMap Text.Text -- ^ Property ID to property name
    -> Map.Map Text.Text Text.Text -- ^ Property name to class name
    -> Map.Map Text.Text Int -- ^ Class name to class ID
    -> Int -- ^ property ID
    -> Maybe (Int, Text.Text) -- ^ Maybe class ID and class name
getClass propertyIdsToNames propertyNamesToClassNames classNamesToIds propertyId =
    case IntMap.lookup propertyId propertyIdsToNames of
        Nothing -> trace ("could not find property name for property id " ++ show propertyId) Nothing
        Just rawPropertyName -> let
            -- There are a large number of properties that end in numbers that
            -- should all be treated the same. Instead of explicitly mapping
            -- each of them, we can remove the numbers and treat them the same.
            propertyName = rawPropertyName & Text.dropWhileEnd Char.isDigit
            in case Map.lookup propertyName propertyNamesToClassNames of
                Nothing -> trace ("could not find class name for property name " ++ show propertyName) $ trace (classNamesToIds & Map.toList & map (\ (k, v) -> " " ++ show v ++ " => " ++ show k) & unlines) $ Nothing
                Just className -> case Map.lookup className classNamesToIds of
                    Nothing -> trace ("could not find class id for class name " ++ show className) Nothing
                    Just classId -> Just (classId, className)

-- | The archetype maps is a mapping from object names to their class names.
archetypeMap :: Map.Map Text.Text Text.Text
archetypeMap =
    -- We start by mapping class names to object names of that class. This
    -- allows us to avoid repeating the class name a bunch.
    [ ( "TAGame.Ball_TA"
      , [ "Archetypes.Ball.Ball_Default"
        , "Archetypes.Ball.Ball_Basketball"
        , "Archetypes.Ball.Ball_Puck"
        ])
    , ( "TAGame.CameraSettingsActor_TA"
      , [ "TAGame.CameraSettingsActor_TA:PRI"
        , "TAGame.Default__CameraSettingsActor_TA"
        ])
    , ( "TAGame.CarComponent_Boost_TA"
      , [ "Archetypes.CarComponents.CarComponent_Boost"
        ])
    , ( "TAGame.CarComponent_Dodge_TA"
      , [ "Archetypes.CarComponents.CarComponent_Dodge"
        ])
    , ( "TAGame.CarComponent_DoubleJump_TA"
      , [ "Archetypes.CarComponents.CarComponent_DoubleJump"
        ])
    , ( "TAGame.CarComponent_FlipCar_TA"
      , [ "Archetypes.CarComponents.CarComponent_FlipCar"
        ])
    , ( "TAGame.CarComponent_Jump_TA"
      , [ "Archetypes.CarComponents.CarComponent_Jump"
        ])
    , ( "TAGame.Car_Season_TA"
      , [ "Archetypes.GameEvent.GameEvent_Season:CarArchetype"
        ])
    , ( "TAGame.Car_TA"
      , [ "Archetypes.Car.Car_Default"
        ])
    , ( "TAGame.GameEvent_Season_TA"
      , [ "Archetypes.GameEvent.GameEvent_Season"
        ])
    , ( "TAGame.GameEvent_SoccarPrivate_TA"
      , [ "Archetypes.GameEvent.GameEvent_SoccarPrivate"
        ])
    , ( "TAGame.GameEvent_SoccarSplitscreen_TA"
      , [ "Archetypes.GameEvent.GameEvent_SoccarSplitscreen"
        ])
    , ( "TAGame.GameEvent_Soccar_TA"
      , [ "Archetypes.GameEvent.GameEvent_Soccar"
        , "Archetypes.GameEvent.GameEvent_Basketball"
        ])
    , ( "TAGame.GRI_TA"
      , [ "GameInfo_Basketball.GameInfo.GameInfo_Basketball:GameReplicationInfoArchetype"
        , "GameInfo_Season.GameInfo.GameInfo_Season:GameReplicationInfoArchetype"
        , "GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype"
        ])
    , ( "TAGame.PRI_TA"
      , [ "TAGame.Default__PRI_TA"
        ])
    , ( "TAGame.Team_TA"
      , [ "Archetypes.Teams.Team"
        ])
    -- These ones are special. They have specific names for each level.
    , ( "TAGame.VehiclePickup_Boost_TA"
      , levels & Set.toList & map (\ level -> level ++ ".TheWorld:PersistentLevel.VehiclePickup_Boost_TA_")
      )
    , ( "TAGame.CrowdActor_TA"
      , levels & Set.toList & map (\ level -> level ++ ".TheWorld:PersistentLevel.CrowdActor_TA_")
      )
    , ( "TAGame.CrowdManager_TA"
      , levels & Set.toList & map (\ level -> level ++ ".TheWorld:PersistentLevel.CrowdManager_TA_")
      )
    ]
        & concatMap (\ (v, ks) -> ks & map (\ k -> (k, v)))
        & map (\ (k, v) -> (Text.pack k, Text.pack v))
        & Map.fromList

-- | These are the levels that we know about.
levels :: Set.Set String
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
    , "eurostad_oob_audio_map"
    , "eurostadium_p"
    , "eurostadium_rainy_audio"
    , "hoopsstadium_sfx"
    , "labs_doublegoal_p"
    , "labs_utopia_p"
    , "park_night_sfx"
    , "park_p"
    , "park_rainy_sfx"
    , "park_sfx"
    , "stadium_oob_audio_map"
    , "stadium_winter_p"
    , "trainstation_p"
    , "utopiastadium_sfx"
    , "wasteland_sfx"
    ] & Set.fromList

-- | These classes have an initial location vector.
classesWithLocation :: Set.Set Text.Text
classesWithLocation =
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

-- | These classes have an initial rotation vector.
classesWithRotation :: Set.Set Text.Text
classesWithRotation =
    [ "TAGame.Ball_TA"
    , "TAGame.Car_Season_TA"
    , "TAGame.Car_TA"
    ] & map Text.pack & Set.fromList
