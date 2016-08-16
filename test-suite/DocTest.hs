module Main (main) where

import Prelude

import qualified Test.DocTest as DocTest


main :: IO ()
main = DocTest.doctest ["library"]
