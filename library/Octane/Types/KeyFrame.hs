module Octane.Types.KeyFrame where

import Octane.Types.Float32LE (Float32LE)
import Octane.Types.Int32LE (Int32LE)

import qualified Data.Binary as B

data KeyFrame = NewKeyFrame
    { keyFrameTime :: Float32LE
    , keyFrameFrame :: Int32LE
    , keyFramePosition :: Int32LE
    } deriving (Show)

instance B.Binary KeyFrame where
    get = NewKeyFrame
        <$> B.get
        <*> B.get
        <*> B.get

    put keyFrame = do
        B.put (keyFrameTime keyFrame)
        B.put (keyFrameFrame keyFrame)
        B.put (keyFramePosition keyFrame)
