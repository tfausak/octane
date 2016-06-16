module Octane.Type.PrimitiveSpec (spec) where

import Test.Tasty.Hspec

import qualified Octane.Type.Primitive.BooleanSpec
import qualified Octane.Type.Primitive.DictionarySpec
import qualified Octane.Type.Primitive.Float32Spec
import qualified Octane.Type.Primitive.ListSpec
import qualified Octane.Type.Primitive.TextSpec
import qualified Octane.Type.Primitive.StreamSpec
import qualified Octane.Type.Primitive.Int32Spec
import qualified Octane.Type.Primitive.Int64Spec

spec :: Spec
spec = describe "Primitive" $ do
    Octane.Type.Primitive.BooleanSpec.spec
    Octane.Type.Primitive.DictionarySpec.spec
    Octane.Type.Primitive.Float32Spec.spec
    Octane.Type.Primitive.ListSpec.spec
    Octane.Type.Primitive.TextSpec.spec
    Octane.Type.Primitive.StreamSpec.spec
    Octane.Type.Primitive.Int32Spec.spec
    Octane.Type.Primitive.Int64Spec.spec
