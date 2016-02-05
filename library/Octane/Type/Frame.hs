{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Frame (Frame(..)) where

import Octane.Core
import Octane.Type.Primitive.Float32LE

data Frame = Frame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    } deriving (Eq, Generic, NFData, Show)

instance Binary Frame where
    get = do
        time <- get
        delta <- get
        return Frame
            { frameTime = time
            , frameDelta = delta
            }

    put frame = do
        frame & frameTime & put
        frame & frameDelta & put
