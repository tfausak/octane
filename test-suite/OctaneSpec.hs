module OctaneSpec (spec) where

import qualified Octane.MainSpec as MainSpec
import qualified Octane.TypeSpec as TypeSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Octane" $ do
    MainSpec.spec
    TypeSpec.spec
