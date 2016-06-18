{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.KeyFrameBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "KeyFrame"
    [ bench "decode basic" (nf decodeKeyFrame "\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (KeyFrame
        (Float32 0.0)
        (Word32 0)
        (Word32 0)))
    ]

decodeKeyFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, KeyFrame)
decodeKeyFrame = Binary.decodeOrFail
