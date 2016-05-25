{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.CacheProperty (CacheProperty(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | A property on an item in the class net cache map.
data CacheProperty = CacheProperty
    { cachePropertyObjectId :: !Word32LE.Word32LE
    , cachePropertyStreamId :: !Word32LE.Word32LE
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary CacheProperty where
    get = CacheProperty <$> Binary.get <*> Binary.get
    put cacheProperty = do
        cacheProperty & cachePropertyObjectId & Binary.put
        cacheProperty & cachePropertyStreamId & Binary.put

instance DeepSeq.NFData CacheProperty

instance Aeson.ToJSON CacheProperty where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop 13
            }
