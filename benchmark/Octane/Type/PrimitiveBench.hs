module Octane.Type.PrimitiveBench (benchmarks) where

import Criterion
import qualified Octane.Type.Primitive.BooleanBench

benchmarks :: Benchmark
benchmarks = bgroup "Primitive"
    [ Octane.Type.Primitive.BooleanBench.benchmarks
    ]
