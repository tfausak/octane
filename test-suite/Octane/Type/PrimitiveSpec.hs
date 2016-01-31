module Octane.Type.PrimitiveSpec (spec) where

import qualified Octane.Type.Primitive.BooleanSpec as BooleanSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Primitive" $ do
    BooleanSpec.spec
