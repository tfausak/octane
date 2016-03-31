{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.StreamBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Stream"
    [ bench "decode basic" (nf decodeStream "\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Stream ""))
    ]

decodeStream :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Stream)
decodeStream = Binary.decodeOrFail
