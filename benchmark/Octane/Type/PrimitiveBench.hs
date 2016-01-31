module Octane.Type.PrimitiveBench (benchmarks) where

import Criterion
import qualified Octane.Type.Primitive.BooleanBench
import qualified Octane.Type.Primitive.Float32LEBench
import qualified Octane.Type.Primitive.Int32LEBench
import qualified Octane.Type.Primitive.Int64LEBench
import qualified Octane.Type.Primitive.ListBench
import qualified Octane.Type.Primitive.PCStringBench

benchmarks :: Benchmark
benchmarks = bgroup "Primitive"
    [ Octane.Type.Primitive.BooleanBench.benchmarks
    , Octane.Type.Primitive.Float32LEBench.benchmarks
    , Octane.Type.Primitive.Int32LEBench.benchmarks
    , Octane.Type.Primitive.Int64LEBench.benchmarks
    , Octane.Type.Primitive.ListBench.benchmarks
    , Octane.Type.Primitive.PCStringBench.benchmarks
    ]
