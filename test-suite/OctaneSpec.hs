module OctaneSpec (spec) where

import qualified Octane.MainSpec as MainSpec
import qualified Octane.TypesSpec as TypesSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Octane" $ do
    MainSpec.spec
    TypesSpec.spec
