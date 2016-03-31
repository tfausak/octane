{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Word32LEBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Word32LE"
    [ bench "decode basic" (nf decodeWord32LE "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Word32LE 0))
    ]

decodeWord32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Word32LE)
decodeWord32LE = Binary.decodeOrFail
