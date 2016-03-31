{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Word64LEBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Word64LE"
    [ bench "decode basic" (nf decodeWord64LE "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Word64LE 0))
    ]

decodeWord64LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Word64LE)
decodeWord64LE = Binary.decodeOrFail
