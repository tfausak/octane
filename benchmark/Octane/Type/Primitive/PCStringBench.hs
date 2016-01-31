{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.PCStringBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "PCString"
    [ bench "decode basic" (whnf decodePCString "\1\0\0\0\0")
    , bench "encode basic" (whnf Binary.encode (NewPCString ""))
    ]

decodePCString :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, PCString)
decodePCString = Binary.decodeOrFail
