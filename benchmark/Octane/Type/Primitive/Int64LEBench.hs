{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int64LEBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Int64LE"
    [ bench "decode basic" (nf decodeInt64LE "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (NewInt64LE 0))
    ]

decodeInt64LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int64LE)
decodeInt64LE = Binary.decodeOrFail
