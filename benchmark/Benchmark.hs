import qualified Criterion.Main
import qualified OctaneBench

main :: IO ()
main = Criterion.Main.defaultMain [OctaneBench.benchmarks]
