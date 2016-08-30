module Main
  ( main
  ) where

import qualified OctaneSpec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hspec as Hspec

main :: IO ()
main = do
  test <- Hspec.testSpec "octane" spec
  Tasty.defaultMain test

spec :: Hspec.Spec
spec = Hspec.parallel OctaneSpec.spec
