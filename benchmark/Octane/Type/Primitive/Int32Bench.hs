{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int32Bench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Int32"
    [ bench "decode basic" (nf decodeInt32 "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Int32 0))
    ]

decodeInt32 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int32)
decodeInt32 = Binary.decodeOrFail
