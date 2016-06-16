{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Float32 (Float32(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as IEEE754
import Data.Function ((&))
import qualified GHC.Generics as Generics

-- | A 32-bit little-endian float.
newtype Float32 = Float32
    { unpackFloat32 :: Float
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Float32 where
    get = do
        float <- IEEE754.getFloat32le
        float & Float32 & return
    put float = do
        float & unpackFloat32 & IEEE754.putFloat32le

instance DeepSeq.NFData Float32

instance Aeson.ToJSON Float32 where
    toJSON float = float & unpackFloat32 & Aeson.toJSON
