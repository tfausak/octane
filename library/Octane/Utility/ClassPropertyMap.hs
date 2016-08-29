{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}

-- | This module is responsible for building the class property map, which maps
-- class IDs to a map of property IDs to property names. This map is the
-- cornerstone of the replay stream parser.
module Octane.Utility.ClassPropertyMap
    ( getClassPropertyMap
    , getPropertyMap
    , getActorMap
    , getClass
    ) where

import Data.Function ((&))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as StrictText
import qualified Octane.Type.ReplayWithoutFrames as ReplayWithoutFrames
import qualified Octane.Type.Word32 as Word32
import qualified "regex-compat" Text.Regex as Regex


-- | The class property map is a map from class IDs in the stream to a map from
-- property IDs in the stream to property names.
getClassPropertyMap :: ReplayWithoutFrames.ReplayWithoutFrames -> IntMap.IntMap (IntMap.IntMap StrictText.Text)
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
-- ID, the second is its cache ID, and the third is its parent's cache ID.
getClassCache :: ReplayWithoutFrames.ReplayWithoutFrames -> [(Int, Int, Int)]
getClassCache replay = replay
    & #cache
    & #unpack
    & map (\ x ->
        ( x & #classId & Word32.fromWord32
        , x & #cacheId & Word32.fromWord32
        , x & #parentCacheId & Word32.fromWord32
        ))


-- | The class IDs in a replay. Comes from the class cache.
getClassIds :: ReplayWithoutFrames.ReplayWithoutFrames -> [Int]
getClassIds replay = replay
    & getClassCache
    & map (\ (x, _, _) -> x)


-- | Gets the parent class ID for the given parent cache ID. This is necessary
-- because there is not always a class with the given cache ID in the cache.
-- When that happens, the parent cache ID is decremented and tried again.
getParentClassId :: Int -> [(Int, Int, Int)] -> Maybe Int
getParentClassId parentCacheId xs =
    case dropWhile (\ (_, cacheId, _) -> cacheId /= parentCacheId) xs of
        [] -> if parentCacheId <= 0
            then Nothing
            else getParentClassId (parentCacheId - 1) xs
        (parentClassId, _, _) : _ -> Just parentClassId


-- | The basic class map is a naive mapping from class ID to its parent class
-- ID. It's naive because it only maps the class ID to its immediate parent.
-- It does not chase the inheritance all the way down.
getBasicClassMap :: ReplayWithoutFrames.ReplayWithoutFrames -> IntMap.IntMap Int
getBasicClassMap replay = replay
    & getClassCache
    & reverse
    & List.tails
    & Maybe.mapMaybe (\ xs -> case xs of
        [] -> Nothing
        (classId, _, parentCacheId) : ys -> do
            parentClassId <- getParentClassId parentCacheId ys
            pure (classId, parentClassId))
    & IntMap.fromList


-- | Given a naive mapping from class ID to its parent class ID, pure all of
-- the parent IDs for a given class.
getParentClassIds :: Int -> IntMap.IntMap Int -> [Int]
getParentClassIds classId basicClassMap =
    case IntMap.lookup classId basicClassMap of
        Nothing -> []
        Just parentClassId -> parentClassId : getParentClassIds parentClassId basicClassMap


-- | The class map is a mapping from a class ID to all of its parent class IDs.
getClassMap :: ReplayWithoutFrames.ReplayWithoutFrames -> IntMap.IntMap [Int]
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
getPropertyMap :: ReplayWithoutFrames.ReplayWithoutFrames -> IntMap.IntMap StrictText.Text
getPropertyMap replay = replay
    & #objects
    & #unpack
    & map #unpack
    & zip [0 ..]
    & IntMap.fromList


-- | The basic class property map is a naive mapping from class IDs to a
-- mapping from property IDs to property names. It's naive because it does
-- not include the properties from the class's parents.
getBasicClassPropertyMap :: ReplayWithoutFrames.ReplayWithoutFrames -> IntMap.IntMap (IntMap.IntMap StrictText.Text)
getBasicClassPropertyMap replay = let
    propertyMap = getPropertyMap replay
    in replay
        & #cache
        & #unpack
        & map (\ x -> let
            classId = x & #classId & Word32.fromWord32
            properties = x
                & #properties
                & #unpack
                & Maybe.mapMaybe (\ y -> let
                    streamId = y & #streamId & Word32.fromWord32
                    propertyId = y & #objectId & Word32.fromWord32
                    in case IntMap.lookup propertyId propertyMap of
                        Nothing -> Nothing
                        Just name -> Just (streamId, name))
                & IntMap.fromList
            in (classId, properties))
        & IntMap.fromList


-- | The actor map is a mapping from class names to their IDs.
getActorMap :: ReplayWithoutFrames.ReplayWithoutFrames -> Map.Map StrictText.Text Int
getActorMap replay = replay
    & #classes
    & #unpack
    & map (\ x -> let
        className = x & #name & #unpack
        classId = x & #streamId & Word32.fromWord32
        in (className, classId))
    & Map.fromList


-- | Gets the class ID and name for a given property ID.
getClass
    :: (Monad m)
    => IntMap.IntMap StrictText.Text -- ^ Property ID to property name
    -> Map.Map StrictText.Text StrictText.Text -- ^ Property name to class name
    -> Map.Map StrictText.Text Int -- ^ Class name to class ID
    -> Int -- ^ property ID
    -> m (Int, StrictText.Text) -- ^ Maybe class ID and class name
getClass propertyIdsToNames propertyNamesToClassNames classNamesToIds propertyId = do
    rawPropertyName <- getPropertyName propertyIdsToNames propertyId
    let propertyName = normalizeName rawPropertyName
    className <- getClassName propertyNamesToClassNames propertyName
    classId <- getClassId classNamesToIds className
    pure (classId, className)


getPropertyName :: (Monad m) => IntMap.IntMap StrictText.Text -> Int -> m StrictText.Text
getPropertyName propertyNames propertyId = do
    case IntMap.lookup propertyId propertyNames of
        Nothing -> do
            fail ("Could not find name for property " ++ show propertyId)
        Just propertyName -> do
            pure propertyName


normalizeName :: StrictText.Text -> StrictText.Text
normalizeName name = name
    & StrictText.unpack
    & replace "_[0-9]+$" ""
    & replace "^[A-Z_a-z]+[.]TheWorld:" "TheWorld:"
    & StrictText.pack


replace :: String -> String -> String -> String
replace needle replacement haystack =
    Regex.subRegex (Regex.mkRegex needle) haystack replacement


getClassName :: (Monad m) => Map.Map StrictText.Text StrictText.Text -> StrictText.Text -> m StrictText.Text
getClassName classNames propertyName = do
    case Map.lookup propertyName classNames of
        Nothing -> do
            fail ("Could not find class for property " ++ show propertyName)
        Just className -> do
            pure className


getClassId :: (Monad m) => Map.Map StrictText.Text Int -> StrictText.Text -> m Int
getClassId classIds className = do
    case Map.lookup className classIds of
        Nothing -> do
            fail ("Could not find ID for class " ++ show className)
        Just classId -> do
            pure classId
