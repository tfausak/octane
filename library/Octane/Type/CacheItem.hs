{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheItem (CacheItem(..)) where

import Octane.Core
import Octane.Type.CacheProperty
import Octane.Type.Primitive.Int32LE
import Octane.Type.Primitive.List

data CacheItem = NewCacheItem
    { cacheItemTag :: Int32LE
    , cacheItemStart :: Int32LE
    , cacheItemEnd :: Int32LE
    , cacheItemCacheProperties :: List CacheProperty
    } deriving (Eq, Generic, NFData, Show)

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
