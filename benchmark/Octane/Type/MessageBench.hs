{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.MessageBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Message"
    [ bench "decode basic" (nf decodeMessage "\0\0\0\0\1\0\0\0\0\1\0\0\0\0")
    , bench "encode basic" (nf Binary.encode (NewMessage (NewInt32LE 0) (NewPCString "") (NewPCString "")))
    ]

decodeMessage :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Message)
decodeMessage = Binary.decodeOrFail
