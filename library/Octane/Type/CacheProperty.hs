{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheProperty (CacheProperty(..)) where

import Octane.Core
import Octane.Type.Primitive.Int32LE

data CacheProperty = CacheProperty
    { cachePropertyIndex :: Int32LE
    , cachePropertyTag :: Int32LE
    } deriving (Eq, Generic, NFData, Show)

instance Binary CacheProperty where
    get = do
        index <- get
        tag <- get
        return CacheProperty
            { cachePropertyIndex = index
            , cachePropertyTag = tag
            }

    put cacheProperty = do
        cacheProperty & cachePropertyIndex & put
        cacheProperty & cachePropertyTag & put
