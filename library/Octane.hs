module Octane where

import Octane.Types

import System.Environment (getArgs)

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified System.IO as IO

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
            putStrLn ""

            putStrLn "# SIZE 1 #\n"
            print (unInt32LE (replaySize1 replay))
            putStrLn ""

            putStrLn "# CRC 1 #\n"
            debugByteString (replayCRC1 replay)
            putStrLn ""

            putStrLn "# VERSION #\n"
            print (unInt32LE (replayVersion1 replay))
            print (unInt32LE (replayVersion2 replay))
            putStrLn ""

            putStrLn "# LABEL #\n"
            print (unPCString (replayLabel replay))
            putStrLn ""

            putStrLn "# PROPERTIES #\n"
            mapM_
                (\ (name, property) -> do
                    putStr (show (unPCString name) ++ "\t=> ")
                    debugProperty property)
                (M.assocs (unTable (replayProperties replay)))
            putStrLn ""

            putStrLn "# SIZE 2 #\n"
            print (unInt32LE (replaySize2 replay))
            putStrLn ""

            putStrLn "# CRC 2 #\n"
            debugByteString (replayCRC2 replay)
            putStrLn ""

            putStrLn "# EFFECTS #\n"
            mapM_
                (\ effect -> do
                    print (unPCString effect))
                (unList (replayEffects replay))
            putStrLn ""

            putStrLn "# KEY FRAMES #\n"
            mapM_
                (\ keyFrame -> do
                    putStrLn (show (unFloat32LE (keyFrameTime keyFrame)) ++ "s @ frame " ++ show (unInt32LE (keyFrameFrame keyFrame)) ++ " - bit " ++ show (unInt32LE (keyFramePosition keyFrame))))
                (unList (replayKeyFrames replay))
            putStrLn ""

            putStrLn "# FRAMES #\n"
            putStrLn (show (BS.length (replayFrames replay)) ++ " bytes")
            putStrLn ""

            putStrLn "# MESSAGES #\n"
            mapM_
                (\ message -> do
                    putStrLn (show (unPCString (messageName message)) ++ " @ frame " ++ show (unInt32LE (messageFrame message)) ++ ": " ++ show (unPCString (messageContent message))))
                (unList (replayMessages replay))
            putStrLn ""

            putStrLn "# GOALS #\n"
            mapM_
                (\ goal -> do
                    putStrLn (show (goalTeam goal) ++ " @ frame " ++ show (unInt32LE (goalFrame goal))))
                (unList (replayGoals replay))
            putStrLn ""

            putStrLn "# PACKAGES #\n"
            mapM_
                (\ package -> do
                    print (unPCString package))
                (unList (replayPackages replay))
            putStrLn ""

            putStrLn "# OBJECTS #\n"
            mapM_
                (\ (index, object) -> do
                    putStrLn (show index ++ "\t=> " ++ show (unPCString object)))
                (IM.assocs (unObjectMap (replayObjectMap replay)))
            putStrLn ""

            putStrLn "# NAMES #\n"
            mapM_
                (\ name -> do
                    print (unPCString name))
                (unList (replayNames replay))
            putStrLn ""

            putStrLn "# ACTORS #\n"
            mapM_
                (\ (index, actor) -> do
                    putStrLn (show index ++ "\t=> " ++ show (unPCString actor)))
                (IM.assocs (unActorMap (replayActorMap replay)))
            putStrLn ""

            putStrLn "# CACHE #\n"
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

debugByteString :: BS.ByteString -> IO ()
debugByteString bytes = do
    BSB.hPutBuilder IO.stdout (BSB.byteStringHex bytes)
    putStrLn ""

debugProperty :: Property -> IO ()
debugProperty property = case property of
    ArrayProperty _ (NewList array) -> do
        putStrLn "[array]"
        mapM_
            (\ (NewTable table) -> mapM_
                (\ (NewPCString k, v) -> do
                    putStr (show k ++ "\t=> ")
                    debugProperty v)
                (M.assocs table) >> putStrLn "")
            array
    FloatProperty _ (NewFloat32LE value) -> print value
    IntProperty _ (NewInt32LE value) -> print value
    NameProperty _ (NewPCString value) -> putStrLn (show value ++ " [name]")
    StrProperty _ (NewPCString value) -> print value
