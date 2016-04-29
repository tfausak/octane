module Octane.VersionSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Version as Version

spec :: Spec
spec = describe "Version" $ do
    it "has some parts" $ do
        shouldBe (null (Version.versionBranch version)) False
