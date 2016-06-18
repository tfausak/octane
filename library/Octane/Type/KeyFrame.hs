{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.KeyFrame (KeyFrame(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Word32 as Word32


data KeyFrame = KeyFrame
    { time :: Float32.Float32
    , frame :: Word32.Word32
    , position :: Word32.Word32
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary KeyFrame where
    get = KeyFrame <$> Binary.get <*> Binary.get <*> Binary.get
    put keyFrame = do
        keyFrame & time & Binary.put
        keyFrame & frame & Binary.put
        keyFrame & position & Binary.put

instance DeepSeq.NFData KeyFrame where
