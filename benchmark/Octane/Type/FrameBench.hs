{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.FrameBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Frame"
    [ bench "decode basic" (nf decodeFrame "\
        \\0\0\0\0\
        \\0\0\0\0\
        \")
    , bench "encode basic" (nf Binary.encode (Frame
        (Float32LE 0.0)
        (Float32LE 0.0)))
    ]

decodeFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Frame)
decodeFrame = Binary.decodeOrFail
