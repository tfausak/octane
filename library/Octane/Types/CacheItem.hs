module Octane.Types.CacheItem where

import Octane.Core
import Octane.Types.CacheProperty
import Octane.Types.Int32LE
import Octane.Types.List

data CacheItem = NewCacheItem {
    cacheItemTag :: Int32LE,
    cacheItemStart :: Int32LE,
    cacheItemEnd :: Int32LE,
    cacheItemCacheProperties :: List CacheProperty
} deriving (Show)

instance Binary CacheItem where
    get = do
        tag <- get
        start <- get
        end <- get
        cacheProperties <- get
        return NewCacheItem {
            cacheItemTag = tag,
            cacheItemStart = start,
            cacheItemEnd = end,
            cacheItemCacheProperties = cacheProperties
        }

    put cacheItem = do
        cacheItem & cacheItemTag & put
        cacheItem & cacheItemStart & put
        cacheItem & cacheItemEnd & put
        cacheItem & cacheItemCacheProperties & put
