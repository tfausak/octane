{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.MarkBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Mark"
    [ bench "decode basic" (nf decodeMark "\
        \\1\0\0\0\0\
        \\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (Mark
        (PCString "")
        (Word32LE 0)))
    ]

decodeMark :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Mark)
decodeMark = Binary.decodeOrFail
