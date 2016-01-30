import qualified OctaneSpec as OctaneSpec
import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "octane" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel OctaneSpec.spec
