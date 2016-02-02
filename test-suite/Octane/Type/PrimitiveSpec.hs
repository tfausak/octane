module Octane.Type.PrimitiveSpec (spec) where

import qualified Octane.Type.Primitive.BooleanSpec
import qualified Octane.Type.Primitive.DictionarySpec
import qualified Octane.Type.Primitive.Float32LESpec
import qualified Octane.Type.Primitive.Int32LESpec
import qualified Octane.Type.Primitive.Int64LESpec
import qualified Octane.Type.Primitive.ListSpec
import qualified Octane.Type.Primitive.PCStringSpec
import qualified Octane.Type.Primitive.StreamSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Primitive" $ do
    Octane.Type.Primitive.BooleanSpec.spec
    Octane.Type.Primitive.DictionarySpec.spec
    Octane.Type.Primitive.Float32LESpec.spec
    Octane.Type.Primitive.Int32LESpec.spec
    Octane.Type.Primitive.Int64LESpec.spec
    Octane.Type.Primitive.ListSpec.spec
    Octane.Type.Primitive.PCStringSpec.spec
    Octane.Type.Primitive.StreamSpec.spec
