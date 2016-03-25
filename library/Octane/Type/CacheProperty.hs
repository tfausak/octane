{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheProperty (CacheProperty(..)) where

import Octane.Core
import Octane.Type.Primitive.Word32LE

data CacheProperty = CacheProperty
    { cachePropertyIndex :: Word32LE
    , cachePropertyTag :: Word32LE
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

instance ToJSON CacheProperty where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 13 }
