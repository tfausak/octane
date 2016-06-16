{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheProperty (CacheProperty(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Json as Json
import qualified Octane.Type.Primitive.Int32 as Int32

-- | A property on an item in the class net cache map.
data CacheProperty = CacheProperty
    { cachePropertyObjectId :: !Int32.Int32
    , cachePropertyStreamId :: !Int32.Int32
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary CacheProperty where
    get = CacheProperty <$> Binary.get <*> Binary.get
    put cacheProperty = do
        cacheProperty & cachePropertyObjectId & Binary.put
        cacheProperty & cachePropertyStreamId & Binary.put

instance DeepSeq.NFData CacheProperty

instance Aeson.ToJSON CacheProperty where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "CacheProperty")
