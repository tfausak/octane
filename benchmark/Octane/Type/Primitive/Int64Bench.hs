{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int64Bench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Int64"
    [ bench "decode basic" (nf decodeInt64 "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Int64 0))
    ]

decodeInt64 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int64)
decodeInt64 = Binary.decodeOrFail
