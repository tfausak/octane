module OctaneSpec (spec) where

import qualified Octane.MainSpec
import qualified Octane.TypeSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Octane" $ do
    Octane.MainSpec.spec
    Octane.TypeSpec.spec
