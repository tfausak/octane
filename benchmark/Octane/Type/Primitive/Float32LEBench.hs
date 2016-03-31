{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Float32LEBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Float32LE"
    [ bench "decode basic" (nf decodeFloat32LE "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Float32LE 0.0))
    ]

decodeFloat32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Float32LE)
decodeFloat32LE = Binary.decodeOrFail
