{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.DictionaryBench (benchmarks) where

import Criterion
import Octane

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

benchmarks :: Benchmark
benchmarks = bgroup "Dictionary"
    [ bench "decode basic" (nf decodeBooleanDictionary "\5\0\0\0None\0")
    , bench "encode basic" (nf Binary.encode (Dictionary Map.empty :: Dictionary Boolean))
    ]

decodeDictionary :: (Binary.Binary a) => BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Dictionary a)
decodeDictionary = Binary.decodeOrFail

decodeBooleanDictionary :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Dictionary Boolean)
decodeBooleanDictionary = decodeDictionary
