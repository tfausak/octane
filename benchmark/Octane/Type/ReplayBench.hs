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
    , bench "encode basic" (nf Binary.encode (NewReplay
        (NewInt32LE 0)
        (NewInt32LE 0)
        (NewInt32LE 0)
        (NewInt32LE 0)
        (NewPCString "")
        (NewDictionary Map.empty)
        (NewInt32LE 0)
        (NewInt32LE 0)
        (NewList [])
        (NewList [])
        ""
        (NewList [])
        (NewList [])
        (NewList [])
        (NewList [])
        (NewList [])
        (NewList [])
        (NewList [])))
    ]

decodeReplay :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Replay)
decodeReplay = Binary.decodeOrFail
