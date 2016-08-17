module Octane.Type.CacheItem (CacheItem(..)) where

import Basics

import qualified Octane.Type.CacheProperty as CacheProperty
import qualified Octane.Type.List as List
import qualified Octane.Type.Word32 as Word32


-- | An item in the class net cache map.
data CacheItem = CacheItem
    { cacheItemClassId :: Word32.Word32
    -- ^ The class ID.
    , cacheItemParentCacheId :: Word32.Word32
    -- ^ The cache ID of the parent class.
    , cacheItemCacheId :: Word32.Word32
    -- ^ The cache ID of the class.
    , cacheItemProperties :: List.List CacheProperty.CacheProperty
    -- ^ The properties that belong to this class.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''CacheItem)

-- | Fields are stored one after the other in order.
instance Binary CacheItem where
    get = CacheItem
        <$> get
        <*> get
        <*> get
        <*> get

    put cacheItem = do
        cacheItem & #classId & put
        cacheItem & #parentCacheId & put
        cacheItem & #cacheId & put
        cacheItem & #properties & put

instance NFData CacheItem where
