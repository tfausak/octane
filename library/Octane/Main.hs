module Octane.Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Octane.Type.Replay as Replay
import qualified System.Environment as Environment


-- | Octane's command-line entrypoint. It has three modes:
--
-- 1.  If no arguments are given, it will read from 'System.IO.stdin' and write
--     to a JSON object to 'System.IO.stdout'.
--
--     > octane < a.replay > replay.json
--
-- 2.  If one argument is given, it will assume that argument is a path to a
--     replay. Both local and remote (HTTP or HTTPS) paths are supported.
--     Octane will read from the path and write a JSON object to
--     'System.IO.stdout'.
--
--     > octane a.replay > replay.json
--
--     > octane https://media.rocketleaguereplays.com/uploads/replay_files/8D0940554D285C3F45109F85C79396A2.replay > replay.json
--
-- 3.  If multiple arguments are given, it will assume that those arguments are
--     paths to replays, read from them, and write a JSON array of objects to
--     'System.IO.stdout'.
--
--     > octane first.replay second.replay > replays.json
main :: IO ()
main = do
    manager <- Client.newManager TLS.tlsManagerSettings
    TLS.setGlobalManager manager

    args <- Environment.getArgs
    case args of
        [] -> main0
        [x] -> main1 x
        xs -> main2 xs


main0 :: IO ()
main0 = do
    LazyBytes.interact (\ input -> do
        let replay = Binary.decode input
        Aeson.encode (replay :: Replay.Replay))


main1 :: String -> IO ()
main1 x = do
    replay <- decode x
    let output = Aeson.encode replay
    LazyBytes.putStr output


main2 :: [String] -> IO ()
main2 xs = do
    replays <- mapM decode xs
    let output = Aeson.encode replays
    LazyBytes.putStr output


decode :: String -> IO Replay.Replay
decode x = do
    case Client.parseUrlThrow x of
        Nothing -> do
            let file = x
            Binary.decodeFile file
        Just request -> do
            manager <- TLS.getGlobalManager
            response <- Client.httpLbs request manager
            let input = Client.responseBody response
            let replay = Binary.decode input
            pure replay
