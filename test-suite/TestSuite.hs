import Test.Tasty.Hspec

import qualified OctaneSpec
import qualified Test.Tasty

main :: IO ()
main = do
    test <- testSpec "octane" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel OctaneSpec.spec
