{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.KeyFrame (KeyFrame(..)) where

import Octane.Internal.Core
import Octane.Type.Primitive.Float32LE
import Octane.Type.Primitive.Word32LE

-- | A key frame. Each key frame has the time since the beginning of the match,
-- | the frame it corresponds to, and that frame's bit position in the network
-- | stream.
data KeyFrame = KeyFrame
    { keyFrameTime :: Float32LE
    , keyFrameFrame :: Word32LE
    , keyFramePosition :: Word32LE
    } deriving (Eq, Generic, NFData, Show)

instance Binary KeyFrame where
    get = KeyFrame
        <$> get
        <*> get
        <*> get

    put keyFrame = do
        keyFrame & keyFrameTime & put
        keyFrame & keyFrameFrame & put
        keyFrame & keyFramePosition & put

instance ToJSON KeyFrame where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 8 }
