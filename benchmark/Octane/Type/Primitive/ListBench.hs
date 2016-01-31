{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.ListBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "List"
    [ bgroup "decode"
        [ bench "basic" (whnf decodeBooleanList "\0\0\0\0")
        ]
    , bgroup "encode"
        [ bench "basic" (whnf Binary.encode (NewList [] :: List Boolean))
        ]
    ]

decodeList :: (Binary.Binary a) => BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, List a)
decodeList = Binary.decodeOrFail

decodeBooleanList :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, List Boolean)
decodeBooleanList = decodeList
