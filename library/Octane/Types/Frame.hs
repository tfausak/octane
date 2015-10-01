module Octane.Types.Frame where

import Octane.Types.Float32LE (Float32LE)

import qualified Data.Binary as B

data Frame = NewFrame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    } deriving (Show)

instance B.Binary Frame where
    get = do
        time <- B.get
        delta <- B.get
        return NewFrame
            { frameTime = time
            , frameDelta = delta
            }

    put frame = do
        B.put (frameTime frame)
        B.put (frameDelta frame)
