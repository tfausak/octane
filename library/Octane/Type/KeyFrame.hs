{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.KeyFrame (KeyFrame(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Float32LE as Float32LE
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | A key frame. Each key frame has the time since the beginning of the match,
-- | the frame it corresponds to, and that frame's bit position in the network
-- | stream.
data KeyFrame = KeyFrame
    { keyFrameTime :: Float32LE.Float32LE
    , keyFrameFrame :: Word32LE.Word32LE
    , keyFramePosition :: Word32LE.Word32LE
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary KeyFrame where
    get = KeyFrame <$> Binary.get <*> Binary.get <*> Binary.get
    put keyFrame = do
        keyFrame & keyFrameTime & Binary.put
        keyFrame & keyFrameFrame & Binary.put
        keyFrame & keyFramePosition & Binary.put

instance DeepSeq.NFData KeyFrame

instance Aeson.ToJSON KeyFrame where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop 8
            }
