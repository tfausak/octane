{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.KeyFrameBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "KeyFrame"
    [ bench "decode basic" (whnf decodeKeyFrame "\0\0\0\0\0\0\0\0\0\0\0\0")
    , bench "encode basic" (whnf Binary.encode (NewKeyFrame (NewFloat32LE 0.0) (NewInt32LE 0) (NewInt32LE 0)))
    ]

decodeKeyFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, KeyFrame)
decodeKeyFrame = Binary.decodeOrFail
