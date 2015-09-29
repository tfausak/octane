module Octane where

import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified System.Environment as E

main :: IO ()
main = do
    files <- E.getArgs
    results <- mapM B.decodeFileOrFail files
    mapM_ printResult results

printResult :: Either (G.ByteOffset, String) Replay -> IO ()
printResult result = case result of
    Left (offset, message) -> putStrLn (show offset ++ ": " ++ message)
    Right replay -> print replay

data Replay = NewReplay
    {
    } deriving (Eq, Ord, Read, Show)

instance B.Binary Replay where
    get = undefined
    put = undefined
