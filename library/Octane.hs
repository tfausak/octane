module Octane where

import Octane.Types

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified System.Environment as Env
import qualified System.IO as IO

main :: IO ()
main = do
    files <- Env.getArgs
    contents <- mapM BS.readFile files
    results <- mapM Binary.decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, BS.ByteString, Either (Binary.ByteOffset, String) Replay) -> IO ()
debug (file, contents, result) = do
    putStrLn file
    putStrLn ("input:\t" ++ show (BS.length contents) ++ " bytes")
    case result of
        Left (offset, message) -> do
            putStrLn ("error at byte " ++ show offset ++ ": " ++ message)
        Right replay -> do
            let output = Binary.encode replay
            putStrLn ("output:\t" ++ show (BSL.length output) ++ " bytes")
            putStrLn ""

            putStrLn "# SIZE 1 #\n"
            print (getInt32LE (replaySize1 replay))
            putStrLn ""

            putStrLn "# CRC 1 #\n"
            print (getInt32LE (replayCRC1 replay))
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
                (Map.assocs (getTable (replayProperties replay)))
            putStrLn ""

            putStrLn "# SIZE 2 #\n"
            print (getInt32LE (replaySize2 replay))
            putStrLn ""

            putStrLn "# CRC 2 #\n"
            print (getInt32LE (replayCRC2 replay))
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
                (IntMap.assocs (getObjectMap (replayObjectMap replay)))
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
                (IntMap.assocs (getActorMap (replayActorMap replay)))
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
    BS.hPutBuilder IO.stdout (BS.byteStringHex bytes)
    putStrLn ""

debugProperty :: Property -> IO ()
debugProperty property = case property of
    ArrayProperty _ (NewList array) -> do
        putStrLn "[array]"
        mapM_
            (\ (NewTable table) -> mapM_
                (\ (NewPCString k, v) -> do
                    putStr ("\t" ++ show k ++ "\t=> ")
                    debugProperty v)
                (Map.assocs table) >> putStrLn "")
            array
    FloatProperty _ (NewFloat32LE value) -> print value
    IntProperty _ (NewInt32LE value) -> print value
    NameProperty _ (NewPCString value) -> putStrLn (show value ++ " [name]")
    StrProperty _ (NewPCString value) -> print value
