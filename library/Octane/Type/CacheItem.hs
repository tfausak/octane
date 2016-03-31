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
    get = CacheItem
        <$> get
        <*> get
        <*> get
        <*> get

    put cacheItem = do
        cacheItem & cacheItemTag & put
        cacheItem & cacheItemStart & put
        cacheItem & cacheItemEnd & put
        cacheItem & cacheItemCacheProperties & put

instance ToJSON CacheItem where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 9 }
