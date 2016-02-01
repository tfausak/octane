module Octane.Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Octane.Core
import Octane.Type

main :: IO ()
main = do
    files <- getArgs
    contents <- mapM BS.readFile files
    results <- mapM decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, ByteString, Either (ByteOffset, String) Replay) -> IO ()
debug (file, contents, result) = case result of
    Left (offset, message) -> hPutStrLn stderr
        (file ++ " @ byte " ++ show offset ++ " - " ++ message)
    Right replay -> do
        let inputSize = contents & BS.length & fromIntegral
        let outputSize = replay & encode & BSL.length
        when (inputSize /= outputSize) $ do
            hPutStrLn stderr ("input size (" ++ show inputSize ++ ") not equal to output size (" ++ show outputSize ++ ")!")
        print replay
