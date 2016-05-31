{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Float32LE (Float32LE(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as IEEE754
import Data.Function ((&))
import qualified GHC.Generics as Generics

-- | A 32-bit little-endian float.
newtype Float32LE = Float32LE
    { unpackFloat32LE :: Float
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Float32LE where
    get = do
        float <- IEEE754.getFloat32le
        float & Float32LE & return
    put float = do
        float & unpackFloat32LE & IEEE754.putFloat32le

instance DeepSeq.NFData Float32LE

instance Aeson.ToJSON Float32LE
