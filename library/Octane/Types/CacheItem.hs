module Octane.Types.CacheItem where

import qualified Data.Binary as Binary
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

instance Binary.Binary CacheItem where
    get = do
        tag <- Binary.get
        start <- Binary.get
        end <- Binary.get
        cacheProperties <- Binary.get
        return NewCacheItem {
            cacheItemTag = tag,
            cacheItemStart = start,
            cacheItemEnd = end,
            cacheItemCacheProperties = cacheProperties
        }

    put cacheItem = do
        cacheItem & cacheItemTag & Binary.put
        cacheItem & cacheItemStart & Binary.put
        cacheItem & cacheItemEnd & Binary.put
        cacheItem & cacheItemCacheProperties & Binary.put
