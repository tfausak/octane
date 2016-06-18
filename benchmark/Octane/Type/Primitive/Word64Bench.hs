{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Word64Bench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Word64"
    [ bench "decode basic" (nf decodeWord64 "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Word64 0))
    ]

decodeWord64 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Word64)
decodeWord64 = Binary.decodeOrFail
