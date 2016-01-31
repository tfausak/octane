{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.BooleanBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Boolean"
    [ bgroup "decode"
        [ bench "basic" (whnf decodeBoolean "\0")
        ]
    , bgroup "encode"
        [ bench "basic" (whnf Binary.encode (NewBoolean False))
        ]
    ]

decodeBoolean :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Boolean)
decodeBoolean = Binary.decodeOrFail
