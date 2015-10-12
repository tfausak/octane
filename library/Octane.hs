{-# LANGUAGE OverloadedStrings #-}

module Octane where

import Octane.Types

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BB
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import Flow ((|>))
import qualified System.Environment as Env
import qualified System.IO as IO

main :: IO ()
main = do
    files <- Env.getArgs
    contents <- mapM BS.readFile files
    results <- mapM Binary.decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, BS.ByteString, Either (Binary.ByteOffset, String) Replay) -> IO ()
debug (file, contents, result) = case result of
    Left (offset, message) -> IO.hPutStrLn IO.stderr
        (file ++ " @ byte " ++ show offset ++ " - " ++ message)
    Right replay -> do
        putStrLn file

        let inputSize = contents |> BS.length |> fromIntegral
        let outputSize = replay |> Binary.encode |> BSL.length
        if inputSize == outputSize then return () else IO.hPutStrLn IO.stderr
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
        mapM_ (\ frame -> print frame) (getFrames replay)
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
                let a = cacheItem |> cacheItemTag |> getInt32LE |> fromIntegral
                let b = replay |> replayObjectMap |> getObjectMap |> IntMap.lookup a |> fmap getPCString
                putStrLn ("ID:\t" ++ show a ++ " (" ++ show b ++ ")")
                putStrLn ("Start:\t" ++ show (getInt32LE (cacheItemStart cacheItem)))
                putStrLn ("End:\t" ++ show (getInt32LE (cacheItemEnd cacheItem)))
                putStrLn "Properties:"
                mapM_
                    (\ cacheProperty -> do
                        let c = cacheProperty |> cachePropertyIndex |> getInt32LE |> fromIntegral
                        let d = replay |> replayObjectMap |> getObjectMap |> IntMap.lookup c |> fmap getPCString
                        putStrLn ("- " ++ show (getInt32LE (cachePropertyTag cacheProperty)) ++ "\t=> " ++ show c ++ " (" ++ show d ++ ")"))
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

-- Frames

type Frame = (Float, Float, Int, Maybe T.Text, Int, Maybe T.Text)
type Frames = [Frame]

getFrames :: Replay -> Frames
getFrames replay = Binary.runGet
    (BB.runBitGet (bitGetFrames replay))
    (replay |> replayFrames |> flipEndian |> BSL.fromStrict)

bitGetFrames :: Replay -> BB.BitGet Frames
bitGetFrames replay = do
    timeBytes <- BB.getByteString 4
    let time = timeBytes |> flipEndian |> BSL.fromStrict |> Binary.decode |> getFloat32LE
    deltaBytes <- BB.getByteString 4
    let delta = deltaBytes |> flipEndian |> BSL.fromStrict |> Binary.decode |> getFloat32LE
    hasActor <- BB.getBool
    if not hasActor then error "no actors" else do
        actorID <- case Map.lookup (NewPCString "MaxChannels") (getTable (replayProperties replay)) of
            Just (IntProperty _ (NewInt32LE x)) -> fmap fromIntegral (BB.getWord64be (log_2 x)) -- TODO: little endian
            x -> error ("unexpected max channel size: " ++ show x)
        let actor = replay |> replayObjectMap |> getObjectMap |> IntMap.lookup actorID |> fmap getPCString
        isOpen <- BB.getBool
        if not isOpen then error "channel closed" else do
            isNew <- BB.getBool
            if not isNew then error "existing actor" else do
                isStatic <- BB.getBool
                if isStatic then error "static actor" else do
                    -- TODO: Is this right? Maybe they're always 8 bits.
                    archetypeID <- case replay |> replayObjectMap |> getObjectMap |> IntMap.size |> log_2 |> (\ x -> x :: Int) of
                        8 -> fmap (fromIntegral . flipByte) (BB.getWord8 8)
                        9 -> fmap (fromIntegral . flipWord9) (BB.getWord16be 9)
                        x -> error ("unexpected size: " ++ show x)
                    let archetype = replay |> replayObjectMap |> getObjectMap |> IntMap.lookup archetypeID |> fmap getPCString
                    return [(time, delta, actorID, actor, archetypeID, archetype)]

log_2 :: (Integral a, Integral b) => a -> b
log_2 x = ceiling (log (fromIntegral x) / log (2 :: Float))

flipEndian :: BS.ByteString -> BS.ByteString
flipEndian bytes = BS.map flipByte bytes

flipWord9 :: Binary.Word16 -> Binary.Word16
flipWord9 x = Bits.zeroBits
    |> (if Bits.testBit x 0 then setBit 8 else id)
    |> (if Bits.testBit x 1 then setBit 7 else id)
    |> (if Bits.testBit x 2 then setBit 6 else id)
    |> (if Bits.testBit x 3 then setBit 5 else id)
    |> (if Bits.testBit x 4 then setBit 4 else id)
    |> (if Bits.testBit x 5 then setBit 3 else id)
    |> (if Bits.testBit x 6 then setBit 2 else id)
    |> (if Bits.testBit x 7 then setBit 1 else id)
    |> (if Bits.testBit x 8 then setBit 0 else id)

flipByte :: Binary.Word8 -> Binary.Word8
flipByte byte = Bits.zeroBits
    |> (if Bits.testBit byte 0 then setBit 7 else id)
    |> (if Bits.testBit byte 1 then setBit 6 else id)
    |> (if Bits.testBit byte 2 then setBit 5 else id)
    |> (if Bits.testBit byte 3 then setBit 4 else id)
    |> (if Bits.testBit byte 4 then setBit 3 else id)
    |> (if Bits.testBit byte 5 then setBit 2 else id)
    |> (if Bits.testBit byte 6 then setBit 1 else id)
    |> (if Bits.testBit byte 7 then setBit 0 else id)

setBit :: (Bits.Bits a) => Int -> a -> a
setBit n x = Bits.setBit x n
