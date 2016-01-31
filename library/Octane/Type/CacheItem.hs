module Octane.Type.CacheItem (CacheItem(..)) where

import Octane.Core
import Octane.Type.CacheProperty
import Octane.Type.List
import Octane.Type.Primitive.Int32LE

data CacheItem = NewCacheItem
    { cacheItemTag :: Int32LE
    , cacheItemStart :: Int32LE
    , cacheItemEnd :: Int32LE
    , cacheItemCacheProperties :: List CacheProperty
    } deriving (Show)

instance Binary CacheItem where
    get = do
        tag <- get
        start <- get
        end <- get
        cacheProperties <- get
        return NewCacheItem
            { cacheItemTag = tag
            , cacheItemStart = start
            , cacheItemEnd = end
            , cacheItemCacheProperties = cacheProperties
            }

    put cacheItem = do
        cacheItem & cacheItemTag & put
        cacheItem & cacheItemStart & put
        cacheItem & cacheItemEnd & put
        cacheItem & cacheItemCacheProperties & put
