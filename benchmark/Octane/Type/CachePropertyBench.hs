{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CachePropertyBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "CacheProperty"
    [ bench "decode basic" (whnf decodeCacheProperty "\0\0\0\0\0\0\0\0")
    , bench "encode basic" (whnf Binary.encode (NewCacheProperty (NewInt32LE 0) (NewInt32LE 0)))
    ]

decodeCacheProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheProperty)
decodeCacheProperty = Binary.decodeOrFail
