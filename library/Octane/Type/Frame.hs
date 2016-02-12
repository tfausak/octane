{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Frame (Frame(..)) where

import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Bits.Put as Bits
import qualified Data.Word as Word
import Octane.Core
import Octane.Type.Primitive.Float32LE

data Frame = Frame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    , frameReplications :: [()]
    } deriving (Eq, Generic, NFData, Show)

instance BinaryBit Frame where
    getBits _ = do
        time <- Bits.getWord32be 32
        delta <- Bits.getWord32be 32
        hasReplication <- Bits.getBool
        if hasReplication
        then undefined -- TODO
        else return Frame
            { frameTime = wordToFloat time
            , frameDelta = wordToFloat delta
            , frameReplications = []
            }
    putBits _ frame = do
        frame & frameTime & floatToWord & Bits.putWord32be 32
        frame & frameDelta & floatToWord & Bits.putWord32be 32
        if frame & frameReplications & null
        then Bits.putBool False
        else undefined -- TODO

wordToFloat :: Word.Word32 -> Float32LE
wordToFloat _ = Float32LE 0 -- TODO

floatToWord :: Float32LE -> Word.Word32
floatToWord _ = 0 -- TODO
