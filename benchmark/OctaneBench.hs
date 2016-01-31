module OctaneBench (benchmarks) where

import Criterion
import qualified Octane.TypeBench

benchmarks :: Benchmark
benchmarks = bgroup "Octane"
    [ Octane.TypeBench.benchmarks
    ]
