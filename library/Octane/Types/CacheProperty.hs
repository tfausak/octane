module Octane.Types.CacheProperty where

import Octane.Types.Int32LE (Int32LE)

import qualified Data.Binary as B

data CacheProperty = NewCacheProperty
    { cachePropertyIndex :: Int32LE
    , cachePropertyTag :: Int32LE
    } deriving (Show)

instance B.Binary CacheProperty where
    get = NewCacheProperty
        <$> B.get
        <*> B.get

    put cacheProperty = do
        B.put (cachePropertyIndex cacheProperty)
        B.put (cachePropertyTag cacheProperty)
