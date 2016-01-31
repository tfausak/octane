module Octane.TypeBench (benchmarks) where

import Criterion
import qualified Octane.Type.PrimitiveBench

benchmarks :: Benchmark
benchmarks = bgroup "Type"
    [ Octane.Type.PrimitiveBench.benchmarks
    ]
