module Octane.Type.KeyFrame (KeyFrame(..)) where

import Octane.Core
import Octane.Type.Primitive.Float32LE
import Octane.Type.Primitive.Int32LE

data KeyFrame = NewKeyFrame
    { keyFrameTime :: Float32LE
    , keyFrameFrame :: Int32LE
    , keyFramePosition :: Int32LE
    } deriving (Show)

instance Binary KeyFrame where
    get = do
        time <- get
        frame <- get
        position <- get
        return NewKeyFrame
            { keyFrameTime = time
            , keyFrameFrame = frame
            , keyFramePosition = position
            }

    put keyFrame = do
        keyFrame & keyFrameTime & put
        keyFrame & keyFrameFrame & put
        keyFrame & keyFramePosition & put
