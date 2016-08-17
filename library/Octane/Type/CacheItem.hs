module Octane.Type.CacheItem (CacheItem(..)) where

import Basics

import qualified Data.Binary as Binary
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
        <$> Binary.get
        <*> Binary.get
        <*> Binary.get
        <*> Binary.get

    put cacheItem = do
        cacheItem & #classId & Binary.put
        cacheItem & #parentCacheId & Binary.put
        cacheItem & #cacheId & Binary.put
        cacheItem & #properties & Binary.put

instance NFData CacheItem where
