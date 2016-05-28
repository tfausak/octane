module Octane.Parser.ClassPropertyMap where

import Data.Function ((&))
import Debug.Trace

import qualified Control.Newtype as Newtype
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
        Just propertyName -> case Map.lookup propertyName propertyNamesToClassNames of
            Nothing -> trace ("could not find class name for property name " ++ show propertyName) Nothing
            Just className -> case Map.lookup className classNamesToIds of
                Nothing -> trace ("could not find class id for class name " ++ show className) Nothing
                Just classId -> Just (classId, className)

-- | The archetype maps is a mapping from object IDs to their class IDs.
archetypeMap :: Map.Map Text.Text Text.Text
archetypeMap =
    [ ( "TAGame.CarComponent_Boost_TA"
      , [ "Archetypes.CarComponents.CarComponent_Boost"
        ])
    , ( "TAGame.CarComponent_Jump_TA"
      , [ "Archetypes.CarComponents.CarComponent_Jump"
        ])
    , ( "TAGame.Ball_TA"
      , [ "Archetypes.Ball.Ball_Default"
        ])
    , ( "TAGame.CarComponent_DoubleJump_TA"
      , [ "Archetypes.CarComponents.CarComponent_DoubleJump"
        ])
    , ( "TAGame.CarComponent_FlipCar_TA"
      , [ "Archetypes.CarComponents.CarComponent_FlipCar"
        ])
    , ( "TAGame.CameraSettingsActor_TA"
      , [ "TAGame.CameraSettingsActor_TA:PRI"
        ])
    , ( "TAGame.PRI_TA"
      , [ "TAGame.Default__PRI_TA"
        ])
    , ( "TAGame.GameEvent_Soccar_TA"
      , [ "Archetypes.GameEvent.GameEvent_Soccar"
        ])
    , ( "TAGame.CarComponent_Dodge_TA"
      , [ "Archetypes.CarComponents.CarComponent_Dodge"
        ])
    , ( "TAGame.Car_TA"
      , [ "Archetypes.Car.Car_Default"
        ])
    , ( "Engine.GameReplicationInfo"
      , [ "GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype"
        ])
    , ( "TAGame.Team_TA"
      , [ "Archetypes.Teams.Team0"
        , "Archetypes.Teams.Team1"
        ])
    ]
        & concatMap (\ (v, ks) -> ks & map (\ k -> (k, v)))
        & map (\ (k, v) -> (Text.pack k, Text.pack v))
        & Map.fromList

-- | These classes have an initial location vector.
classesWithLocation :: Set.Set Text.Text
classesWithLocation =
    [ "TAGame.CarComponent_Boost_TA"
    , "TAGame.CarComponent_Jump_TA"
    , "TAGame.Ball_TA"
    , "TAGame.CarComponent_DoubleJump_TA"
    , "TAGame.CarComponent_FlipCar_TA"
    , "TAGame.PRI_TA"
    , "TAGame.GameEvent_Soccar_TA"
    , "TAGame.CarComponent_Dodge_TA"
    , "TAGame.Car_TA"
    , "Engine.GameReplicationInfo"
    , "TAGame.Team_TA"
    ] & map Text.pack & Set.fromList

-- | These classes have an initial rotation vector.
classesWithRotation :: Set.Set Text.Text
classesWithRotation =
    [ "TAGame.Ball_TA"
    , "TAGame.Car_TA"
    ] & map Text.pack & Set.fromList
