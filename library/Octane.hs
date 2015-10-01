{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Octane.Types

import System.Environment (getArgs)

import qualified Data.Binary as B

main :: IO ()
main = do
    files <- getArgs
    replays <- mapM B.decodeFile files
    let newFiles = map (\ x -> x ++ "-no-outro.replay") files
    let newReplays = map (\ x -> x { replayOutro = "" }) replays
    mapM_ (uncurry B.encodeFile) (zip newFiles newReplays)
