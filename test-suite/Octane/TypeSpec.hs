module Octane.TypeSpec (spec) where

import Test.Tasty.Hspec

import qualified Octane.Type.ActorSpec
import qualified Octane.Type.CacheItemSpec
import qualified Octane.Type.CachePropertySpec
import qualified Octane.Type.KeyFrameSpec
import qualified Octane.Type.MarkSpec
import qualified Octane.Type.MessageSpec
import qualified Octane.Type.PrimitiveSpec
import qualified Octane.Type.PropertySpec
import qualified Octane.Type.ReplaySpec

spec :: Spec
spec = describe "Type" $ do
    Octane.Type.ActorSpec.spec
    Octane.Type.CacheItemSpec.spec
    Octane.Type.CachePropertySpec.spec
    Octane.Type.KeyFrameSpec.spec
    Octane.Type.MarkSpec.spec
    Octane.Type.MessageSpec.spec
    Octane.Type.PrimitiveSpec.spec
    Octane.Type.PropertySpec.spec
    Octane.Type.ReplaySpec.spec
