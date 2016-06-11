module Octane.Main (main) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Octane.FullReplay as FullReplay
import qualified Prelude
import qualified System.Environment as Environment


main :: Prelude.IO ()
main = do
    [file] <- Environment.getArgs
    fullReplay <- FullReplay.unsafeParseReplayFile file
    DeepSeq.deepseq fullReplay (Prelude.pure ())
    ByteString.putStrLn (Aeson.encode fullReplay)
