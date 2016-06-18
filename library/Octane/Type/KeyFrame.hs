{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.KeyFrame (KeyFrame(..)) where

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

instance DeepSeq.NFData KeyFrame where
