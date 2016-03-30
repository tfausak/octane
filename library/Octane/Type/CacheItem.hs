{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheItem (CacheItem(..)) where

import Octane.Internal.Core
import Octane.Type.CacheProperty
import Octane.Type.Primitive.List
import Octane.Type.Primitive.Word32LE

data CacheItem = CacheItem
    { cacheItemTag :: Word32LE
    , cacheItemStart :: Word32LE
    , cacheItemEnd :: Word32LE
    , cacheItemCacheProperties :: List CacheProperty
    } deriving (Eq, Generic, NFData, Show)

instance Binary CacheItem where
    get = do
        tag <- get
        start <- get
        end <- get
        cacheProperties <- get
        return CacheItem
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

instance ToJSON CacheItem where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 9 }
