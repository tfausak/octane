module Octane.Type.PrimitiveSpec (spec) where

import qualified Octane.Type.Primitive.BooleanSpec as BooleanSpec
import qualified Octane.Type.Primitive.Float32LESpec as Float32LESpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Primitive" $ do
    BooleanSpec.spec
    Float32LESpec.spec
