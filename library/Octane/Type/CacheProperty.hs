module Octane.Type.CacheProperty (CacheProperty(..)) where

import Basics

import qualified Data.Binary as Binary
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
instance Binary.Binary CacheProperty where
    get = CacheProperty
        <$> Binary.get
        <*> Binary.get

    put cacheProperty = do
        cacheProperty & #objectId & Binary.put
        cacheProperty & #streamId & Binary.put

instance NFData CacheProperty where
