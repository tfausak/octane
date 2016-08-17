module Octane.Type.KeyFrame (KeyFrame(..)) where

import Basics

import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Word32 as Word32


-- | A key frame.
data KeyFrame = KeyFrame
    { keyFrameTime :: Float32.Float32
    -- ^ When this key frame occurred.
    , keyFrameFrame :: Word32.Word32
    -- ^ Which frame this key frame corresponds to.
    , keyFramePosition :: Word32.Word32
    -- ^ The bit position of the start of this key frame in the network stream.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''KeyFrame)

-- | Stored with the fields one after the other in order.
instance Binary KeyFrame where
    get = KeyFrame
        <$> get
        <*> get
        <*> get

    put keyFrame = do
        keyFrame & #time & put
        keyFrame & #frame & put
        keyFrame & #position & put

instance NFData KeyFrame where
