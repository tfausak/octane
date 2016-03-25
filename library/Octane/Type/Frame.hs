{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Frame (Frame(..)) where

import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Bits.Put as Bits
import qualified Data.Word as Word
import Octane.Core
import Octane.Type.Primitive.Float32LE
import Octane.Type.Replication

data Frame = Frame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    , frameReplications :: [Replication]
    } deriving (Eq, Generic, NFData, Show)

instance BinaryBit Frame where
    getBits _ = do
        time <- Bits.getWord32be 32
        delta <- Bits.getWord32be 32
        replications <- getReplications
        return Frame
            { frameTime = wordToFloat time
            , frameDelta = wordToFloat delta
            , frameReplications = replications
            }

    putBits _ frame = do
        frame & frameTime & floatToWord & Bits.putWord32be 32
        frame & frameDelta & floatToWord & Bits.putWord32be 32
        frame & frameReplications & putReplications

instance ToJSON Frame where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

wordToFloat :: Word.Word32 -> Float32LE
wordToFloat _ = Float32LE 0 -- TODO

floatToWord :: Float32LE -> Word.Word32
floatToWord _ = 0 -- TODO

getReplications :: BitGet [Replication]
getReplications = do
    hasReplication <- Bits.getBool
    if hasReplication
    then do
        replication <- getBits undefined
        replications <- getReplications
        return (replication : replications)
    else return []

putReplications :: [Replication] -> BitPut ()
putReplications replications = case replications of
    [] -> Bits.putBool False
    replication : rest -> do
        Bits.putBool True
        putBits undefined replication
        putReplications rest
