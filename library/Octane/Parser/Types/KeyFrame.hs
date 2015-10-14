{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.KeyFrame where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Binary as Binary
import Flow ((|>))
import Octane.Parser.Types.Float32LE
import Octane.Parser.Types.Int32LE

data KeyFrame = NewKeyFrame {
    keyFrameTime :: Float32LE,
    keyFrameFrame :: Int32LE,
    keyFramePosition :: Int32LE
} deriving (Show)

instance Aeson.ToJSON KeyFrame where
    toJSON keyFrame = Aeson.object [
        "time" .= keyFrameTime keyFrame,
        "frame" .= keyFrameFrame keyFrame,
        "position" .= keyFramePosition keyFrame
        ]

instance Binary.Binary KeyFrame where
    get = do
        time <- Binary.get
        frame <- Binary.get
        position <- Binary.get
        return NewKeyFrame {
            keyFrameTime = time,
            keyFrameFrame = frame,
            keyFramePosition = position
        }

    put keyFrame = do
        keyFrame |> keyFrameTime |> Binary.put
        keyFrame |> keyFrameFrame |> Binary.put
        keyFrame |> keyFramePosition |> Binary.put