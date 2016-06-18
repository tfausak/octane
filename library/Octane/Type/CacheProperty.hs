{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.CacheProperty (CacheProperty(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word32 as Word32


-- | A property on an item in the class net cache map.
data CacheProperty = CacheProperty
    { cachePropertyObjectId :: Word32.Word32
    , cachePropertyStreamId :: Word32.Word32
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary CacheProperty where
    get = CacheProperty <$> Binary.get <*> Binary.get
    put cacheProperty = do
        cacheProperty & cachePropertyObjectId & Binary.put
        cacheProperty & cachePropertyStreamId & Binary.put

instance DeepSeq.NFData CacheProperty where
