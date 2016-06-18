{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CacheItemBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

benchmarks :: Benchmark
benchmarks = bgroup "CacheItem"
    [ bench "decode basic" (nf decodeCacheItem "\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (CacheItem
        (Word32 0)
        (Word32 0)
        (Word32 0)
        (List [])))
    ]

decodeCacheItem :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheItem)
decodeCacheItem = Binary.decodeOrFail
