module OctaneBench (benchmarks) where

import Criterion
import qualified Octane.MainBench
import qualified Octane.TypeBench

benchmarks :: Benchmark
benchmarks = bgroup "Octane"
    [ Octane.MainBench.benchmarks
    , Octane.TypeBench.benchmarks
    ]
