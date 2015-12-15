{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.CacheItem where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Binary as Binary
import Data.Function ((&))
import Octane.Parser.Types.CacheProperty
import Octane.Parser.Types.Int32LE
import Octane.Parser.Types.List

data CacheItem = NewCacheItem {
    cacheItemTag :: Int32LE,
    cacheItemStart :: Int32LE,
    cacheItemEnd :: Int32LE,
    cacheItemCacheProperties :: List CacheProperty
} deriving (Show)

instance Aeson.ToJSON CacheItem where
    toJSON cacheItem = Aeson.object [
        "id" .= cacheItemTag cacheItem,
        "start" .= cacheItemStart cacheItem,
        "end" .= cacheItemEnd cacheItem,
        "properties" .= cacheItemCacheProperties cacheItem
        ]

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
