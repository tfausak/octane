{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Float32Bench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Float32"
    [ bench "decode basic" (nf decodeFloat32 "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Float32 0.0))
    ]

decodeFloat32 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Float32)
decodeFloat32 = Binary.decodeOrFail
