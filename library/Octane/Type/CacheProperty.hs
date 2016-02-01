module Octane.Type.CacheProperty (CacheProperty(..)) where

import Octane.Core
import Octane.Type.Primitive.Int32LE

data CacheProperty = NewCacheProperty
    { cachePropertyIndex :: Int32LE
    , cachePropertyTag :: Int32LE
    } deriving (Eq, Show)

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
