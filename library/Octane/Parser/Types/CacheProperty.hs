{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.CacheProperty where

import qualified Data.Binary as Binary
import Data.Function ((&))
import Octane.Parser.Types.Int32LE

data CacheProperty = NewCacheProperty {
    cachePropertyIndex :: Int32LE,
    cachePropertyTag :: Int32LE
} deriving (Show)

instance Binary.Binary CacheProperty where
    get = do
        index <- Binary.get
        tag <- Binary.get
        return NewCacheProperty {
            cachePropertyIndex = index,
            cachePropertyTag = tag
        }

    put cacheProperty = do
        cacheProperty & cachePropertyIndex & Binary.put
        cacheProperty & cachePropertyTag & Binary.put
