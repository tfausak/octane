module Octane.Parser.ClassPropertyMap (getClassPropertyMap) where

import Data.Function ((&))

import qualified Control.Newtype as Newtype
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
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

-- | The basic class map is a naive mapping from class ID to its parent class
-- | ID. It's naive because it only maps the class ID to its immediate parent.
-- | It does not chase the inheritance all the way down.
getBasicClassMap :: Type.Replay -> IntMap.IntMap Int
getBasicClassMap replay = replay
    & getClassCache
    & reverse
    & List.tails
    & Maybe.mapMaybe (\ xs ->
        case xs of
            [] -> Nothing
            (classId, _, parentCacheId) : ys ->
                case dropWhile (\ (_, cacheId, _) -> cacheId /= parentCacheId) ys of
                    [] -> Nothing
                    (parentClassId, _, _) : _ -> Just (classId, parentClassId))
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
