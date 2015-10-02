module Octane where

import Octane.Types

import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Bits as BB
import qualified Data.Binary.Bits.Get as BB
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
    files <- getArgs
    contents <- mapM BS.readFile files
    results <- mapM B.decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, BS.ByteString, Either (B.ByteOffset, String) Replay) -> IO ()
debug (file, contents, result) = do
    putStrLn file
    putStrLn ("input:\t" ++ show (BS.length contents) ++ " bytes")
    case result of
        Left (offset, message) -> do
            putStrLn ("error at byte " ++ show offset ++ ": " ++ message)
        Right replay -> do
            let output = B.encode replay
            putStrLn ("output:\t" ++ show (BSL.length output) ++ " bytes")

            let frames = BSL.fromStrict (replayFrames replay)
            if BSL.null frames
            then putStrLn "No frames!"
            else do
                let parser = BB.runBitGet (BB.getBits undefined)
                let frame = B.runGet parser frames
                print (frame :: Frame)
