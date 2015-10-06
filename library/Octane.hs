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
            print (getInt32LE (replaySize1 replay))
            putStrLn ""

            putStrLn "# CRC 1 #\n"
            debugByteString (replayCRC1 replay)
            putStrLn ""

            putStrLn "# VERSION #\n"
            print (getInt32LE (replayVersion1 replay))
            print (getInt32LE (replayVersion2 replay))
            putStrLn ""

            putStrLn "# LABEL #\n"
            print (getPCString (replayLabel replay))
            putStrLn ""

            putStrLn "# PROPERTIES #\n"
            mapM_
                (\ (name, property) -> do
                    putStr (show (getPCString name) ++ "\t=> ")
                    debugProperty property)
                (M.assocs (getTable (replayProperties replay)))
            putStrLn ""

            putStrLn "# SIZE 2 #\n"
            print (getInt32LE (replaySize2 replay))
            putStrLn ""

            putStrLn "# CRC 2 #\n"
            debugByteString (replayCRC2 replay)
            putStrLn ""

            putStrLn "# EFFECTS #\n"
            mapM_
                (\ effect -> do
                    print (getPCString effect))
                (getList (replayEffects replay))
            putStrLn ""

            putStrLn "# KEY FRAMES #\n"
            mapM_
                (\ keyFrame -> do
                    putStrLn (show (getFloat32LE (keyFrameTime keyFrame)) ++ "s @ frame " ++ show (getInt32LE (keyFrameFrame keyFrame)) ++ " - bit " ++ show (getInt32LE (keyFramePosition keyFrame))))
                (getList (replayKeyFrames replay))
            putStrLn ""

            putStrLn "# FRAMES #\n"
            putStrLn (show (BS.length (replayFrames replay)) ++ " bytes")
            putStrLn ""

            putStrLn "# MESSAGES #\n"
            mapM_
                (\ message -> do
                    putStrLn (show (getPCString (messageName message)) ++ " @ frame " ++ show (getInt32LE (messageFrame message)) ++ ": " ++ show (getPCString (messageContent message))))
                (getList (replayMessages replay))
            putStrLn ""

            putStrLn "# MARKS #\n"
            mapM_
                (\ goal -> do
                    putStrLn (show (getPCString (markLabel goal)) ++ " @ frame " ++ show (getInt32LE (markFrame goal))))
                (getList (replayMarks replay))
            putStrLn ""

            putStrLn "# PACKAGES #\n"
            mapM_
                (\ package -> do
                    print (getPCString package))
                (getList (replayPackages replay))
            putStrLn ""

            putStrLn "# OBJECTS #\n"
            mapM_
                (\ (index, object) -> do
                    putStrLn (show index ++ "\t=> " ++ show (getPCString object)))
                (IM.assocs (getObjectMap (replayObjectMap replay)))
            putStrLn ""

            putStrLn "# NAMES #\n"
            mapM_
                (\ name -> do
                    print (getPCString name))
                (getList (replayNames replay))
            putStrLn ""

            putStrLn "# ACTORS #\n"
            mapM_
                (\ (index, actor) -> do
                    putStrLn (show index ++ "\t=> " ++ show (getPCString actor)))
                (IM.assocs (getActorMap (replayActorMap replay)))
            putStrLn ""

            putStrLn "# CACHE #\n"
            mapM_
                (\ cacheItem -> do
                    putStrLn ("ID:\t" ++ show (getInt32LE (cacheItemTag cacheItem)))
                    putStrLn ("Start:\t" ++ show (getInt32LE (cacheItemStart cacheItem)))
                    putStrLn ("End:\t" ++ show (getInt32LE (cacheItemEnd cacheItem)))
                    putStrLn "Properties:"
                    mapM_
                        (\ cacheProperty -> do
                            putStrLn ("- " ++ show (getInt32LE (cachePropertyTag cacheProperty)) ++ "\t=> " ++ show (getInt32LE (cachePropertyIndex cacheProperty))))
                        (getList (cacheItemCacheProperties cacheItem))
                    putStrLn "")
                (getList (replayCacheItems replay))

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
