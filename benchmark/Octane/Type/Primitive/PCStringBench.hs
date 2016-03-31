{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.PCStringBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "PCString"
    [ bench "decode basic" (nf decodePCString "\1\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (PCString ""))
    ]

decodePCString :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, PCString)
decodePCString = Binary.decodeOrFail
