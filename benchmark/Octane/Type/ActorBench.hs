{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.ActorBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Actor"
    [ bench "decode basic" (nf decodeActor "\
        \\1\0\0\0\0\
        \\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Actor
        (PCString "")
        (Int32LE 0)))
    ]

decodeActor :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Actor)
decodeActor = Binary.decodeOrFail
