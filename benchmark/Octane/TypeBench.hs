module Octane.TypeBench (benchmarks) where

import Criterion
import qualified Octane.Type.ActorBench
import qualified Octane.Type.CacheItemBench
import qualified Octane.Type.CachePropertyBench
import qualified Octane.Type.FrameBench
import qualified Octane.Type.KeyFrameBench
import qualified Octane.Type.MarkBench
import qualified Octane.Type.MessageBench
import qualified Octane.Type.PrimitiveBench
import qualified Octane.Type.PropertyBench
import qualified Octane.Type.ReplayBench
import qualified Octane.Type.StreamBench

benchmarks :: Benchmark
benchmarks = bgroup "Type"
    [ Octane.Type.ActorBench.benchmarks
    , Octane.Type.CacheItemBench.benchmarks
    , Octane.Type.CachePropertyBench.benchmarks
    , Octane.Type.FrameBench.benchmarks
    , Octane.Type.KeyFrameBench.benchmarks
    , Octane.Type.MarkBench.benchmarks
    , Octane.Type.MessageBench.benchmarks
    , Octane.Type.PrimitiveBench.benchmarks
    , Octane.Type.PropertyBench.benchmarks
    , Octane.Type.ReplayBench.benchmarks
    , Octane.Type.StreamBench.benchmarks
    ]
