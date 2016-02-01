{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.PropertyBench (benchmarks) where

import Criterion
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Octane

benchmarks :: Benchmark
benchmarks = bgroup "Property"
    [ bgroup "Array"
        [ bench "decode basic" (nf decodeProperty "\
            \\14\0\0\0ArrayProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (ArrayProperty
            (NewInt64LE 0)
            (NewList [])))
        ]
    , bgroup "Bool"
        [ bench "decode basic" (nf decodeProperty "\
            \\13\0\0\0BoolProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\0\
            \")
        , bench "encode basic" (nf Binary.encode (BoolProperty
            (NewInt64LE 0)
            (NewBoolean False)))
        ]
    , bgroup "Byte"
        [ bench "decode basic" (nf decodeProperty "\
            \\13\0\0\0ByteProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\1\0\0\0\0\
            \\1\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (ByteProperty
            (NewInt64LE 0)
            (NewPCString "", NewPCString "")))
        ]
    , bgroup "Float"
        [ bench "decode basic" (nf decodeProperty "\
            \\14\0\0\0FloatProperty\0\
            \\4\0\0\0\0\0\0\0\
            \\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (FloatProperty
            (NewInt64LE 4)
            (NewFloat32LE 0.0)))
        ]
    , bgroup "Int"
        [ bench "decode basic" (nf decodeProperty "\
            \\12\0\0\0IntProperty\0\
            \\4\0\0\0\0\0\0\0\
            \\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (IntProperty
            (NewInt64LE 4)
            (NewInt32LE 0)))
        ]
    , bgroup "Name"
        [ bench "decode basic" (nf decodeProperty "\
            \\13\0\0\0NameProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\1\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (NameProperty
            (NewInt64LE 0)
            (NewPCString "")))
        ]
    , bgroup "QWord"
        [ bench "decode basic" (nf decodeProperty "\
            \\14\0\0\0QWordProperty\0\
            \\8\0\0\0\0\0\0\0\
            \\0\0\0\0\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (QWordProperty
            (NewInt64LE 8)
            (NewInt64LE 0)))
        ]
    , bgroup "String"
        [ bench "decode basic" (nf decodeProperty "\
            \\12\0\0\0StrProperty\0\
            \\0\0\0\0\0\0\0\0\
            \\1\0\0\0\0\
            \")
        , bench "encode basic" (nf Binary.encode (StrProperty
            (NewInt64LE 0)
            (NewPCString "")))
        ]
    ]

decodeProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Property)
decodeProperty = Binary.decodeOrFail
