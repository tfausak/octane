module Octane.Types.CacheProperty (CacheProperty(..)) where

import Octane.Core
import Octane.Types.Int32LE

data CacheProperty = NewCacheProperty
    { cachePropertyIndex :: Int32LE
    , cachePropertyTag :: Int32LE
    } deriving (Show)

instance Binary CacheProperty where
    get = do
        index <- get
        tag <- get
        return NewCacheProperty
            { cachePropertyIndex = index
            , cachePropertyTag = tag
            }

    put cacheProperty = do
        cacheProperty & cachePropertyIndex & put
        cacheProperty & cachePropertyTag & put
