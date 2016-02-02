{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.PropertyBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Property"
    [ bgroup "Array"
        [ bench "decode basic" (nf decodeProperty "\
            \\14\0\0\0ArrayProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (ArrayProperty
            (Int64LE 0)
            (List [])))
        ]
    , bgroup "Bool"
        [ bench "decode basic" (nf decodeProperty "\
            \\13\0\0\0BoolProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\0")
        , bench "encode basic" (nf Binary.encode (BoolProperty
            (Int64LE 0)
            (Boolean False)))
        ]
    , bgroup "Byte"
        [ bench "decode basic" (nf decodeProperty "\
            \\13\0\0\0ByteProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\1\0\0\0\0\
            \\1\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (ByteProperty
            (Int64LE 0)
            (PCString "", PCString "")))
        ]
    , bgroup "Float"
        [ bench "decode basic" (nf decodeProperty "\
            \\14\0\0\0FloatProperty\0\
            \\4\0\0\0\0\0\0\0\
            \\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (FloatProperty
            (Int64LE 4)
            (Float32LE 0.0)))
        ]
    , bgroup "Int"
        [ bench "decode basic" (nf decodeProperty "\
            \\12\0\0\0IntProperty\0\
            \\4\0\0\0\0\0\0\0\
            \\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (IntProperty
            (Int64LE 4)
            (Int32LE 0)))
        ]
    , bgroup "Name"
        [ bench "decode basic" (nf decodeProperty "\
            \\13\0\0\0NameProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\1\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (NameProperty
            (Int64LE 0)
            (PCString "")))
        ]
    , bgroup "QWord"
        [ bench "decode basic" (nf decodeProperty "\
            \\14\0\0\0QWordProperty\0\
            \\8\0\0\0\0\0\0\0\
            \\0\0\0\0\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (QWordProperty
            (Int64LE 8)
            (Int64LE 0)))
        ]
    , bgroup "String"
        [ bench "decode basic" (nf decodeProperty "\
            \\12\0\0\0StrProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\1\0\0\0\0")
        , bench "encode basic" (nf Binary.encode (StrProperty
            (Int64LE 0)
            (PCString "")))
        ]
    ]

decodeProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Property)
decodeProperty = Binary.decodeOrFail
