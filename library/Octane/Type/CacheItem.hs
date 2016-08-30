{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.CacheItem
  ( CacheItem(..)
  ) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
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
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''CacheItem)

-- | Fields are stored one after the other in order.
instance Binary.Binary CacheItem where
  get = CacheItem <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
  put cacheItem = do
    cacheItem & #classId & Binary.put
    cacheItem & #parentCacheId & Binary.put
    cacheItem & #cacheId & Binary.put
    cacheItem & #properties & Binary.put

instance DeepSeq.NFData CacheItem
