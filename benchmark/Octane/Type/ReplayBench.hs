{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.ReplayBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Replay"
    [ bench "decode basic" (nf decodeReplay "\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\1\0\0\0\0\
        \\5\0\0\0None\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \\0\0\0\0\
        \")
    , bench "encode basic" (nf Binary.encode (Replay
        (Int32LE 0)
        (Int32LE 0)
        (Int32LE 0)
        (Int32LE 0)
        (PCString "")
        (Dictionary Map.empty)
        (Int32LE 0)
        (Int32LE 0)
        (List [])
        (List [])
        (Stream (Int32LE 0) "")
        (List [])
        (List [])
        (List [])
        (List [])
        (List [])
        (List [])
        (List [])))
    ]

decodeReplay :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Replay)
decodeReplay = Binary.decodeOrFail
