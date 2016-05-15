{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheItem (CacheItem(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.CacheProperty as CacheProperty
import qualified Octane.Type.Primitive.List as List
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | An item in the class net cache map.
data CacheItem = CacheItem
    { cacheItemClassId :: Word32LE.Word32LE
    , cacheItemParentCacheId :: Word32LE.Word32LE
    , cacheItemCacheId :: Word32LE.Word32LE
    , cacheItemCacheProperties :: List.List CacheProperty.CacheProperty
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary CacheItem where
    get = CacheItem <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
    put cacheItem = do
        cacheItem & cacheItemClassId & Binary.put
        cacheItem & cacheItemParentCacheId & Binary.put
        cacheItem & cacheItemCacheId & Binary.put
        cacheItem & cacheItemCacheProperties & Binary.put

instance DeepSeq.NFData CacheItem

instance Aeson.ToJSON CacheItem where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop 9
            }
