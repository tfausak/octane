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

import qualified Data.Bimap as Bimap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as StrictText
import qualified Octane.Data as Data
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.List as List
import qualified Octane.Type.ReplayWithoutFrames as Replay
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32
import qualified "regex-compat" Text.Regex as Regex

-- | The class property map is a map from class IDs in the stream to a map from
-- property IDs in the stream to property names.
getClassPropertyMap :: Replay.ReplayWithoutFrames
                    -> IntMap.IntMap (IntMap.IntMap StrictText.Text)
getClassPropertyMap replay =
  let basicClassPropertyMap = getBasicClassPropertyMap replay
      classMap = getClassMap replay
  in getClassIds (#classes replay) (#cache replay) &
     map
       (\classId ->
          let ownProperties =
                case IntMap.lookup classId basicClassPropertyMap of
                  Nothing -> IntMap.empty
                  Just x -> x
              parentProperties =
                case IntMap.lookup classId classMap of
                  Nothing -> IntMap.empty
                  Just parentClassIds ->
                    parentClassIds &
                    map
                      (\parentClassId ->
                         case IntMap.lookup parentClassId basicClassPropertyMap of
                           Nothing -> IntMap.empty
                           Just x -> x) &
                    IntMap.unions
              properties = IntMap.union ownProperties parentProperties
          in (classId, properties)) &
     IntMap.fromList

-- | The class cache is a list of 4-tuples where the first element is a class
-- ID, the second is its name, the third is its cache ID, the fourth is its
-- parent's cache ID.
getClassCache
  :: List.List ClassItem.ClassItem
  -> List.List CacheItem.CacheItem
  -> [(Int, StrictText.Text, Int, Int)]
getClassCache classes cache = do
  let classNames = classes & getActorMap & Bimap.toMapR
  cache & #unpack &
    map
      (\cacheItem -> do
         let classId = cacheItem & #classId & Word32.fromWord32
         let className =
               case Map.lookup classId classNames of
                 Nothing ->
                   error
                     ("could not find class name for id " ++
                      show classId ++ " in " ++ show classNames)
                 Just x -> x
         let cacheId = cacheItem & #cacheId & Word32.fromWord32
         let parentCacheId = cacheItem & #parentCacheId & Word32.fromWord32
         (classId, className, cacheId, parentCacheId))

-- | The class IDs in a replay. Comes from the class cache.
getClassIds :: List.List ClassItem.ClassItem
            -> List.List CacheItem.CacheItem
            -> [Int]
getClassIds classes cache =
  getClassCache classes cache & map (\(x, _, _, _) -> x)

-- | Gets the parent class ID for the given parent cache ID. This is necessary
-- because there is not always a class with the given cache ID in the cache.
-- When that happens, the parent cache ID is decremented and tried again.
getParentClassId :: Maybe StrictText.Text
                 -> Int
                 -> [(Int, StrictText.Text, Int, Int)]
                 -> Maybe Int
getParentClassId maybeClassName parentCacheId xs =
  case maybeClassName of
    Nothing -> getParentClassIdById parentCacheId xs
    Just className -> getParentClassIdByName className parentCacheId xs

getParentClassIdById :: Int -> [(Int, StrictText.Text, Int, Int)] -> Maybe Int
getParentClassIdById parentCacheId xs =
  case dropWhile (\(_, _, cacheId, _) -> cacheId /= parentCacheId) xs of
    [] ->
      if parentCacheId <= 0
        then Nothing
        else getParentClassIdById (parentCacheId - 1) xs
    (parentClassId, _, _, _):_ -> Just parentClassId

getParentClassIdByName :: StrictText.Text
                       -> Int
                       -> [(Int, StrictText.Text, Int, Int)]
                       -> Maybe Int
getParentClassIdByName className parentCacheId xs =
  case Map.lookup className Data.parentClasses of
    Just parentClassName ->
      xs & filter (\(_, name, _, _) -> name == parentClassName) &
      filter (\(_, _, cacheId, _) -> cacheId == parentCacheId) &
      map (\(classId, _, _, _) -> classId) &
      Maybe.listToMaybe &
      Maybe.maybe (getParentClassIdById parentCacheId xs) Just
    Nothing -> getParentClassIdById parentCacheId xs

-- | The basic class map is a naive mapping from class ID to its parent class
-- ID. It's naive because it only maps the class ID to its immediate parent.
-- It does not chase the inheritance all the way down.
getBasicClassMap
  :: List.List ClassItem.ClassItem
  -> List.List CacheItem.CacheItem
  -> IntMap.IntMap Int
getBasicClassMap classes cache =
  getClassCache classes cache & reverse & List.tails &
  Maybe.mapMaybe
    (\xs ->
       case xs of
         [] -> Nothing
         (classId, className, _, parentCacheId):ys -> do
           parentClassId <- getParentClassId (Just className) parentCacheId ys
           pure (classId, parentClassId)) &
  IntMap.fromList

-- | Given a naive mapping from class ID to its parent class ID, pure all of
-- the parent IDs for a given class.
getParentClassIds :: Int -> IntMap.IntMap Int -> [Int]
getParentClassIds classId basicClassMap =
  case IntMap.lookup classId basicClassMap of
    Nothing -> []
    Just parentClassId ->
      parentClassId : getParentClassIds parentClassId basicClassMap

-- | The class map is a mapping from a class ID to all of its parent class IDs.
getClassMap :: Replay.ReplayWithoutFrames -> IntMap.IntMap [Int]
getClassMap replay =
  let basicClassMap = getBasicClassMap (#classes replay) (#cache replay)
  in getClassIds (#classes replay) (#cache replay) &
     map (\classId -> (classId, getParentClassIds classId basicClassMap)) &
     IntMap.fromList

-- | The property map is a mapping from property IDs to property names.
getPropertyMap :: List.List Text.Text -> IntMap.IntMap StrictText.Text
getPropertyMap objects =
  objects & #unpack & map #unpack & zip [0 ..] & IntMap.fromList

-- | The basic class property map is a naive mapping from class IDs to a
-- mapping from property IDs to property names. It's naive because it does
-- not include the properties from the class's parents.
getBasicClassPropertyMap :: Replay.ReplayWithoutFrames
                         -> IntMap.IntMap (IntMap.IntMap StrictText.Text)
getBasicClassPropertyMap replay =
  let propertyMap = replay & #objects & getPropertyMap
  in replay & #cache & #unpack &
     map
       (\x ->
          let classId = x & #classId & Word32.fromWord32
              properties =
                x & #properties & #unpack &
                Maybe.mapMaybe
                  (\y ->
                     let streamId = y & #streamId & Word32.fromWord32
                         propertyId = y & #objectId & Word32.fromWord32
                     in case IntMap.lookup propertyId propertyMap of
                          Nothing -> Nothing
                          Just name -> Just (streamId, name)) &
                IntMap.fromList
          in (classId, properties)) &
     IntMap.fromList

-- | The actor map is a mapping from class names to their IDs.
getActorMap :: List.List ClassItem.ClassItem -> Bimap.Bimap StrictText.Text Int
getActorMap classes =
  classes & #unpack &
  map
    (\x ->
       let className = x & #name & #unpack
           classId = x & #streamId & Word32.fromWord32
       in (className, classId)) &
  Bimap.fromList

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

getPropertyName
  :: (Monad m)
  => IntMap.IntMap StrictText.Text -> Int -> m StrictText.Text
getPropertyName propertyNames propertyId = do
  case IntMap.lookup propertyId propertyNames of
    Nothing -> do
      fail ("Could not find name for property " ++ show propertyId)
    Just propertyName -> do
      pure propertyName

normalizeName :: StrictText.Text -> StrictText.Text
normalizeName name =
  name & StrictText.unpack & replace "_[0-9]+$" "" &
  replace "^[A-Z_a-z]+[.]TheWorld:" "TheWorld:" &
  StrictText.pack

replace :: String -> String -> String -> String
replace needle replacement haystack =
  Regex.subRegex (Regex.mkRegex needle) haystack replacement

getClassName
  :: (Monad m)
  => Map.Map StrictText.Text StrictText.Text
  -> StrictText.Text
  -> m StrictText.Text
getClassName classNames propertyName = do
  case Map.lookup propertyName classNames of
    Nothing -> do
      fail ("Could not find class for property " ++ show propertyName)
    Just className -> do
      pure className

getClassId
  :: (Monad m)
  => Map.Map StrictText.Text Int -> StrictText.Text -> m Int
getClassId classIds className = do
  case Map.lookup className classIds of
    Nothing -> do
      fail ("Could not find ID for class " ++ show className)
    Just classId -> do
      pure classId
