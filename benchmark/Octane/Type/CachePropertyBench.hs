{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CachePropertyBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "CacheProperty"
    [ bench "decode basic" (nf decodeCacheProperty "\
        \\0\0\0\0\
        \\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (CacheProperty
        (Int32 0)
        (Int32 0)))
    ]

decodeCacheProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheProperty)
decodeCacheProperty = Binary.decodeOrFail
