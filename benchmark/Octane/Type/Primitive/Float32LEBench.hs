{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Float32LEBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Float32LE"
    [ bgroup "decode"
        [ bench "basic" (whnf decodeFloat32LE "\0\0\0\0")
        ]
    , bgroup "encode"
        [ bench "basic" (whnf Binary.encode (NewFloat32LE 0.0))
        ]
    ]

decodeFloat32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Float32LE)
decodeFloat32LE = Binary.decodeOrFail
