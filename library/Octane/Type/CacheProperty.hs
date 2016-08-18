{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.CacheProperty (CacheProperty(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word32 as Word32


-- | A property on an item in the class net cache map.
data CacheProperty = CacheProperty
    { cachePropertyObjectId :: Word32.Word32
    -- ^ The object's ID.
    , cachePropertyStreamId :: Word32.Word32
    -- ^ The object's ID in the network stream.
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''CacheProperty)

-- | Fields are stored one after the other in order.
--
-- >>> Binary.decode "\x01\x00\x00\x00\x02\x00\x00\x00" :: CacheProperty
-- CacheProperty {cachePropertyObjectId = 0x00000001, cachePropertyStreamId = 0x00000002}
--
-- >>> Binary.encode (CacheProperty 1 2)
-- "\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL"
instance Binary.Binary CacheProperty where
    get = CacheProperty
        <$> Binary.get
        <*> Binary.get

    put cacheProperty = do
        cacheProperty & #objectId & Binary.put
        cacheProperty & #streamId & Binary.put

instance DeepSeq.NFData CacheProperty where
