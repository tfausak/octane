module Octane.TypeSpec (spec) where

import qualified Octane.Type.ActorSpec as ActorSpec
import qualified Octane.Type.ActorMapSpec as ActorMapSpec
import qualified Octane.Type.BooleanSpec as BooleanSpec
import qualified Octane.Type.CacheItemSpec as CacheItemSpec
import qualified Octane.Type.CachePropertySpec as CachePropertySpec
import qualified Octane.Type.Float32LESpec as Float32LESpec
import qualified Octane.Type.Int32LESpec as Int32LESpec
import qualified Octane.Type.Int64LESpec as Int64LESpec
import qualified Octane.Type.KeyFrameSpec as KeyFrameSpec
import qualified Octane.Type.ListSpec as ListSpec
import qualified Octane.Type.MarkSpec as MarkSpec
import qualified Octane.Type.MessageSpec as MessageSpec
import qualified Octane.Type.ObjectMapSpec as ObjectMapSpec
import qualified Octane.Type.PCStringSpec as PCStringSpec
import qualified Octane.Type.PropertySpec as PropertySpec
import qualified Octane.Type.ReplaySpec as ReplaySpec
import qualified Octane.Type.TableSpec as TableSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Type" $ do
    ActorSpec.spec
    ActorMapSpec.spec
    BooleanSpec.spec
    CacheItemSpec.spec
    CachePropertySpec.spec
    Float32LESpec.spec
    Int32LESpec.spec
    Int64LESpec.spec
    KeyFrameSpec.spec
    ListSpec.spec
    MarkSpec.spec
    MessageSpec.spec
    ObjectMapSpec.spec
    PCStringSpec.spec
    PropertySpec.spec
    ReplaySpec.spec
    TableSpec.spec
