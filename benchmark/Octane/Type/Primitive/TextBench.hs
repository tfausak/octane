{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.TextBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "Text"
    [ bench "decode basic" (nf decodeText "\1\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Text ""))
    ]

decodeText :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Text)
decodeText = Binary.decodeOrFail
