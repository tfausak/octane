module Octane.Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Octane.Core
import Octane.Type

main :: IO ()
main = do
    files <- getArgs
    contents <- mapM BS.readFile files
    results <- mapM decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, ByteString, Either (ByteOffset, String) Replay) -> IO ()
debug (file, contents, result) = case result of
    Left (offset, message) -> hPutStrLn stderr
        (file ++ " @ byte " ++ show offset ++ " - " ++ message)
    Right replay -> do
        putStrLn file

        let inputSize = contents & BS.length & fromIntegral
        let outputSize = replay & encode & BSL.length
        when (inputSize /= outputSize) $ do
            hPutStrLn stderr
                ( "input size ("
                ++ show inputSize
                ++ ") not equal to output size ("
                ++ show outputSize ++ ")!"
                )
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
            (Map.assocs (getDictionary (replayProperties replay)))
        putStrLn ""

        putStrLn "# SIZE 2 #\n"
        print (getInt32LE (replaySize2 replay))
        putStrLn ""

        putStrLn "# CRC 2 #\n"
        print (getInt32LE (replayCRC2 replay))
        putStrLn ""

        putStrLn "# LEVELS #\n"
        mapM_
            (\ level -> do
                print (getPCString level))
            (getList (replayLevels replay))
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

        putStrLn "# NAMES #\n"
        mapM_
            (\ name -> do
                print (getPCString name))
            (getList (replayNames replay))
        putStrLn ""

        putStrLn "# ACTORS #\n"

        putStrLn "# CACHE #\n"

debugProperty :: Property -> IO ()
debugProperty property = case property of
    ArrayProperty _ (NewList array) -> do
        putStrLn "[array]"
        mapM_
            (\ (NewDictionary dictionary) -> mapM_
                (\ (NewPCString k, v) -> do
                    putStr ("\t" ++ show k ++ "\t=> ")
                    debugProperty v)
                (Map.assocs dictionary) >> putStrLn "")
            array
    BoolProperty _ (NewBoolean value) -> print value
    ByteProperty _ (NewPCString key, NewPCString value) -> putStrLn (show key ++ ": " ++ show value)
    FloatProperty _ (NewFloat32LE value) -> print value
    IntProperty _ (NewInt32LE value) -> print value
    NameProperty _ (NewPCString value) -> putStrLn (show value ++ " [name]")
    QWordProperty _ (NewInt64LE value) -> print value
    StrProperty _ (NewPCString value) -> print value
