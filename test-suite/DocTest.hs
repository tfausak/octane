module Main (main) where

import qualified Test.DocTest as DocTest


main :: IO ()
main = DocTest.doctest ["library"]
