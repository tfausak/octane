module Octane where

import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified System.Environment as E

-- * High-level interface

main :: IO ()
main = do
    files <- E.getArgs
    results <- mapM B.decodeFileOrFail files
    mapM_ printResult results

printResult :: Either (G.ByteOffset, String) Replay -> IO ()
printResult result = case result of
    Left (offset, message) -> putStrLn (show offset ++ ": " ++ message)
    Right replay -> print replay

-- * Types

data Replay = NewReplay
    {
    } deriving (Eq, Ord, Read, Show)

instance B.Binary Replay where
    get = getReplay
    put = putReplay

-- * Readers

getReplay :: G.Get Replay
getReplay = do
    return NewReplay
        {
        }

-- * Writers

-- TODO: This library should be able to write replays, but it's not currently a
--   high priority.
putReplay :: Replay -> P.Put
putReplay _replay = do
    undefined
