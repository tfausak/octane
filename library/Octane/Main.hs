module Octane.Main (main) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Octane.Parser as Parser
import qualified Octane.Type as Type
import qualified System.Environment as Environment

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
        Left (offset,message) -> error
            ( file
            ++ " @ byte "
            ++ show offset
            ++ ": "
            ++ message
            )
        Right replay -> do
            let inputSize = contents & BS.length & fromIntegral
            let outputSize = replay
                    & Binary.encode
                    & BSL.length
                    & DeepSeq.deepseq replay
            Monad.when (inputSize /= outputSize) (error
                ( "input size "
                ++ show inputSize
                ++ " not equal to output size "
                ++ show outputSize
                ))

            let frames = Parser.parseFrames replay
            let expectedFrames = replay
                    & Type.replayProperties
                    & Newtype.unpack
                    & Map.lookup ("NumFrames" & Text.pack & Newtype.pack)
                    & Maybe.fromMaybe (Type.IntProperty (Newtype.pack 4) (Newtype.pack 0))
            let actualFrames = frames
                    & length
                    & fromIntegral
                    & Newtype.pack
                    & Type.IntProperty (Newtype.pack 4)
                    & DeepSeq.deepseq frames
            Monad.when (expectedFrames /= actualFrames) (error
                ( "expected "
                ++ show expectedFrames
                ++ " frames but found "
                ++ show actualFrames
                ++ " frames"
                ))

            putStr "{\"meta\":"
            replay & Aeson.encode & BSL8.putStrLn
            putStr ",\"frames\":"
            frames & Aeson.encode & BSL8.putStrLn
            putStrLn "}"
