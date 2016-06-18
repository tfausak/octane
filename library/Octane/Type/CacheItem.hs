{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.CacheItem (CacheItem(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.CacheProperty as CacheProperty
import qualified Octane.Type.List as List
import qualified Octane.Type.Word32 as Word32


-- | An item in the class net cache map.
data CacheItem = CacheItem
    { classId :: Word32.Word32
    , parentCacheId :: Word32.Word32
    , cacheId :: Word32.Word32
    , properties :: List.List CacheProperty.CacheProperty
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary CacheItem where
    get = CacheItem <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
    put cacheItem = do
        cacheItem & classId & Binary.put
        cacheItem & parentCacheId & Binary.put
        cacheItem & cacheId & Binary.put
        cacheItem & properties & Binary.put

instance DeepSeq.NFData CacheItem where
