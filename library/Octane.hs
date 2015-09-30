module Octane where

import Octane.Types.Replay (Replay)

import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B

main :: IO ()
main = do
    files <- getArgs
    results <- mapM B.decodeFileOrFail files
    mapM_ printResult results

printResult :: Either (B.ByteOffset, String) Replay -> IO ()
printResult result = case result of
    Left (offset, message) -> putStrLn (show offset ++ ": " ++ message)
    Right replay -> print replay
