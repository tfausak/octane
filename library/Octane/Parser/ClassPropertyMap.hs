-- | This module is responsible for building the class property map, which maps
-- | class IDs to a map of property IDs to property names. This map is the
-- | cornerstone of the replay stream parser.
module Octane.Parser.ClassPropertyMap where

import Data.Function ((&))

import qualified Control.Newtype as Newtype
import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
        Nothing -> Nothing
        Just rawPropertyName -> let
            -- There are a large number of properties that end in numbers that
            -- should all be treated the same. Instead of explicitly mapping
            -- each of them, we can remove the numbers and treat them the same.
            propertyName = rawPropertyName & Text.dropWhileEnd Char.isDigit
            in case Map.lookup propertyName propertyNamesToClassNames of
                Nothing -> Nothing
                Just className -> case Map.lookup className classNamesToIds of
                    Nothing -> Nothing
                    Just classId -> Just (classId, className)
