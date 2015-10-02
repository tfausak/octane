module Octane.Types.CacheItem where

import Octane.Types.CacheProperty (CacheProperty)
import Octane.Types.Int32LE (Int32LE)
import Octane.Types.List (List)

import qualified Data.Binary as B

data CacheItem = NewCacheItem
    { cacheItemTag :: Int32LE -- this is the actor id
    -- TODO: These indexes should be offset by the cache item's position in the
    --   list. So if the cache item is at index 3 and the start index is 20,
    --   the start index is actually 23.
    , cacheItemStart :: Int32LE
    , cacheItemEnd :: Int32LE
    , cacheItemCacheProperties :: List CacheProperty
    } deriving (Show)

instance B.Binary CacheItem where
    get = NewCacheItem
        <$> B.get
        <*> B.get
        <*> B.get
        <*> B.get

    put cacheItem = do
        B.put (cacheItemTag cacheItem)
        B.put (cacheItemStart cacheItem)
        B.put (cacheItemEnd cacheItem)
        B.put (cacheItemCacheProperties cacheItem)
