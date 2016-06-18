module Octane.Type.PrimitiveBench (benchmarks) where

import Criterion

import qualified Octane.Type.Primitive.BooleanBench
import qualified Octane.Type.Primitive.DictionaryBench
import qualified Octane.Type.Primitive.Float32Bench
import qualified Octane.Type.Primitive.ListBench
import qualified Octane.Type.Primitive.TextBench
import qualified Octane.Type.Primitive.StreamBench
import qualified Octane.Type.Primitive.Int32Bench
import qualified Octane.Type.Primitive.Word64Bench

benchmarks :: Benchmark
benchmarks = bgroup "Primitive"
    [ Octane.Type.Primitive.BooleanBench.benchmarks
    , Octane.Type.Primitive.DictionaryBench.benchmarks
    , Octane.Type.Primitive.Float32Bench.benchmarks
    , Octane.Type.Primitive.ListBench.benchmarks
    , Octane.Type.Primitive.TextBench.benchmarks
    , Octane.Type.Primitive.StreamBench.benchmarks
    , Octane.Type.Primitive.Int32Bench.benchmarks
    , Octane.Type.Primitive.Word64Bench.benchmarks
    ]
