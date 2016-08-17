module Octane.Type.CacheProperty (CacheProperty(..)) where

import Basics

import qualified Octane.Type.Word32 as Word32


-- | A property on an item in the class net cache map.
data CacheProperty = CacheProperty
    { cachePropertyObjectId :: Word32.Word32
    -- ^ The object's ID.
    , cachePropertyStreamId :: Word32.Word32
    -- ^ The object's ID in the network stream.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''CacheProperty)

-- | Fields are stored one after the other in order.
instance Binary CacheProperty where
    get = CacheProperty
        <$> get
        <*> get

    put cacheProperty = do
        cacheProperty & #objectId & put
        cacheProperty & #streamId & put

instance NFData CacheProperty where
