module Octane.Type.PrimitiveBench (benchmarks) where

import Criterion
import qualified Octane.Type.Primitive.BooleanBench
import qualified Octane.Type.Primitive.Float32LEBench

benchmarks :: Benchmark
benchmarks = bgroup "Primitive"
    [ Octane.Type.Primitive.BooleanBench.benchmarks
    , Octane.Type.Primitive.Float32LEBench.benchmarks
    ]
