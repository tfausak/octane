module Octane.TypesSpec (spec) where

import qualified Octane.Types.ActorSpec as ActorSpec
import qualified Octane.Types.ActorMapSpec as ActorMapSpec
import qualified Octane.Types.BooleanSpec as BooleanSpec
import qualified Octane.Types.CacheItemSpec as CacheItemSpec
import qualified Octane.Types.CachePropertySpec as CachePropertySpec
import qualified Octane.Types.Float32LESpec as Float32LESpec
import qualified Octane.Types.Int32LESpec as Int32LESpec
import qualified Octane.Types.Int64LESpec as Int64LESpec
import qualified Octane.Types.KeyFrameSpec as KeyFrameSpec
import qualified Octane.Types.ListSpec as ListSpec
import qualified Octane.Types.MarkSpec as MarkSpec
import qualified Octane.Types.MessageSpec as MessageSpec
import qualified Octane.Types.ObjectMapSpec as ObjectMapSpec
import qualified Octane.Types.PCStringSpec as PCStringSpec
import qualified Octane.Types.PropertySpec as PropertySpec
import qualified Octane.Types.ReplaySpec as ReplaySpec
import qualified Octane.Types.TableSpec as TableSpec
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Types" $ do
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
