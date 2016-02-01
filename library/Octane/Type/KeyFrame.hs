{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.KeyFrame (KeyFrame(..)) where

import Octane.Core
import Octane.Type.Primitive.Float32LE
import Octane.Type.Primitive.Int32LE

-- | A key frame. Each key frame has the time since the beginning of the match,
-- | the frame it corresponds to, and that frame's bit position in the network
-- | stream.
data KeyFrame = KeyFrame
    { keyFrameTime :: Float32LE
    , keyFrameFrame :: Int32LE
    , keyFramePosition :: Int32LE
    } deriving (Eq, Generic, NFData, Show)

instance Binary KeyFrame where
    get = do
        time <- get
        frame <- get
        position <- get
        return KeyFrame
            { keyFrameTime = time
            , keyFrameFrame = frame
            , keyFramePosition = position
            }

    put keyFrame = do
        keyFrame & keyFrameTime & put
        keyFrame & keyFrameFrame & put
        keyFrame & keyFramePosition & put
