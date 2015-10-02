module Octane where

import Octane.Types

import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as M

main :: IO ()
main = do
    files <- getArgs
    contents <- mapM BS.readFile files
    results <- mapM B.decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, BS.ByteString, Either (B.ByteOffset, String) Replay) -> IO ()
debug (file, contents, result) = do
    putStrLn file
    -- putStrLn ("input:\t" ++ show (BS.length contents) ++ " bytes")
    case result of
        Left (offset, message) -> do
            putStrLn ("error at byte " ++ show offset ++ ": " ++ message)
        Right replay -> do
            let output = B.encode replay
            -- putStrLn ("output:\t" ++ show (BSL.length output) ++ " bytes")

            mapM_
                (\ (index, object) -> do
                    putStrLn (show index ++ "\t=> " ++ show (unPCString object)))
                (M.assocs (unObjectMap (replayObjectMap replay)))
            putStrLn ""

            mapM_
                (\ (index, actor) -> do
                    putStrLn (show index ++ "\t=> " ++ show (unPCString actor)))
                (M.assocs (unActorMap (replayActorMap replay)))
            putStrLn ""

            mapM_
                (\ cacheItem -> do
                    putStrLn ("ID:\t" ++ show (unInt32LE (cacheItemTag cacheItem)))
                    putStrLn ("Start:\t" ++ show (unInt32LE (cacheItemStart cacheItem)))
                    putStrLn ("End:\t" ++ show (unInt32LE (cacheItemEnd cacheItem)))
                    putStrLn "Properties:"
                    mapM_
                        (\ cacheProperty -> do
                            putStrLn ("- " ++ show (unInt32LE (cachePropertyTag cacheProperty)) ++ "\t=> " ++ show (unInt32LE (cachePropertyIndex cacheProperty))))
                        (unList (cacheItemCacheProperties cacheItem))
                    putStrLn "")
                (unList (replayCacheItems replay))
