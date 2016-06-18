module Octane.Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Octane.Type.Replay as Replay
import qualified System.Environment as Environment


-- | Octane's command-line entrypoint. It has three modes:
--
-- 1.  If no arguments are given, it will read from 'System.IO.stdin' and write
--     to a JSON object to 'System.IO.stdout'.
--
-- 2.  If one argument is given, it will assume that argument is a file, read
--     from it, and write a JSON object to 'System.IO.stdout'.
--
-- 3.  If multiple arguments are given, it will assume that those arguments are
--     files, read from them, and write a JSON array of objects to
--     'System.IO.stdout'.
main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> mainWithoutFiles
        [file] -> mainWithFile file
        files -> mainWithFiles files


mainWithoutFiles :: IO ()
mainWithoutFiles = do
    LazyBytes.interact (\ input -> do
        let rawReplay = Binary.decode input
        let replay = Replay.fromRawReplay rawReplay
        Aeson.encode replay)


mainWithFile :: FilePath -> IO ()
mainWithFile file = do
    rawReplay <- Binary.decodeFile file
    let replay = Replay.fromRawReplay rawReplay
    let output = Aeson.encode replay
    LazyBytes.putStr output


mainWithFiles :: [FilePath] -> IO ()
mainWithFiles files = do
    rawReplays <- mapM Binary.decodeFile files
    let replays = map Replay.fromRawReplay rawReplays
    let output = Aeson.encode replays
    LazyBytes.putStr output
