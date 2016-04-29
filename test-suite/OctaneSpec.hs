module OctaneSpec (spec) where

import Test.Tasty.Hspec

import qualified Octane.MainSpec
import qualified Octane.TypeSpec
import qualified Octane.VersionSpec

spec :: Spec
spec = describe "Octane" $ do
    Octane.MainSpec.spec
    Octane.TypeSpec.spec
    Octane.VersionSpec.spec
