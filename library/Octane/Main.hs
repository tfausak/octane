module Octane.Main (main) where

import qualified Control.Monad as Monad
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Function ((&))
import qualified Octane.Parser as Parser
import qualified Octane.Type as Type
import qualified System.Environment as Environment
import qualified System.IO as IO

main :: IO ()
main = do
    files <- Environment.getArgs
    contents <- mapM BS.readFile files
    results <- mapM Binary.decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, BS.ByteString, Either (Binary.ByteOffset, String) Type.Replay)
      -> IO ()
debug (file,contents,result) =
    case result of
        Left (offset,message) ->
            IO.hPutStrLn
                IO.stderr
                (file ++ " @ byte " ++ show offset ++ " - " ++ message)
        Right replay -> do
            let inputSize = contents & BS.length & fromIntegral
            let outputSize = replay & Binary.encode & BSL.length
            Monad.when (inputSize /= outputSize) $
                do IO.hPutStrLn
                       IO.stderr
                       ("input size (" ++
                        show inputSize ++
                        ") not equal to output size (" ++
                        show outputSize ++ ")!")
            let config =
                    Aeson.defConfig
                    { Aeson.confCompare = compare
                    }
            replay & Aeson.encodePretty' config & BSL8.putStrLn
            let frames = Parser.parseFrames replay
            let frame = take 1 frames
            print frame
