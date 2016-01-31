module Octane.Type.PrimitiveSpec (spec) where

import qualified Octane.Type.Primitive.BooleanSpec as BooleanSpec
import qualified Octane.Type.Primitive.Float32LESpec as Float32LESpec
import qualified Octane.Type.Primitive.Int32LESpec as Int32LESpec
import qualified Octane.Type.Primitive.Int64LESpec as Int64LESpec
import qualified Octane.Type.Primitive.ListSpec as ListSpec
import qualified Octane.Type.Primitive.PCStringSpec as PCStringSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Primitive" $ do
    BooleanSpec.spec
    Float32LESpec.spec
    Int32LESpec.spec
    Int64LESpec.spec
    ListSpec.spec
    PCStringSpec.spec
