{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int32LEBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Int32LE"
    [ bgroup "decode"
        [ bench "basic" (whnf decodeInt32LE "\0\0\0\0")
        ]
    , bgroup "encode"
        [ bench "basic" (whnf Binary.encode (NewInt32LE 0))
        ]
    ]

decodeInt32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int32LE)
decodeInt32LE = Binary.decodeOrFail
