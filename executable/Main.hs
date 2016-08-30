module Main
  ( main
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Octane.Type.Replay as Replay
import qualified Octane.Version as Version
import qualified System.Environment as Environment

main :: IO ()
main = do
  manager <- Client.newManager TLS.tlsManagerSettings
  TLS.setGlobalManager manager
  args <- Environment.getArgs
  case args of
    [] -> main0
    ["--version"] -> mainVersion
    [x] -> main1 x
    xs -> main2 xs

main0 :: IO ()
main0 = do
  LazyBytes.interact
    (\input -> do
       let replay = Binary.decode input
       Aeson.encode (replay :: Replay.Replay))

mainVersion :: IO ()
mainVersion = do
  putStrLn Version.versionString

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
