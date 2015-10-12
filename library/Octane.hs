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

getFrames :: Replay -> [(Float, Float, Int, Int, T.Text)]
getFrames replay = Binary.runGet
    (BB.runBitGet (do
        timeBytes <- BB.getByteString 4
        let time = timeBytes |> flipEndian |> BSL.fromStrict |> Binary.decode |> getFloat32LE
        deltaBytes <- BB.getByteString 4
        let delta = deltaBytes |> flipEndian |> BSL.fromStrict |> Binary.decode |> getFloat32LE
        hasActor <- BB.getBool
        if not hasActor then error "no actors" else do
            actorID <- fmap fromIntegral getInt10LE
            isOpen <- BB.getBool
            if not isOpen then error "channel closed" else do
                isNew <- BB.getBool
                if not isNew then error "existing actor" else do
                    isStatic <- BB.getBool
                    if isStatic then error "static actor" else do
                        objectID <- fmap fromIntegral (BB.getWord8 8)
                        let Just object = replay |> replayObjectMap |> getObjectMap |> IntMap.lookup objectID |> fmap getPCString
                        case object of
                            "Archetypes.CarComponents.CarComponent_Boost" -> return ()
                            "Engine.Actor:bHardAttach" -> return ()
                            "Engine.GameReplicationInfo:bMatchHasBegun" -> return ()
                            "Engine.GameReplicationInfo:GameClass" -> return ()
                            "Engine.GameReplicationInfo:GoalScore" -> return ()
                            "Engine.GameReplicationInfo:Winner" -> return ()
                            "Engine.Info" -> return ()
                            "Engine.Pawn:bCanSwatTurn" -> return ()
                            "Engine.Pawn:bUsedByMatinee" -> return ()
                            "Engine.PlayerReplicationInfo:bIsInactive" -> return ()
                            "Engine.PlayerReplicationInfo:bIsSpectator" -> return ()
                            "Engine.PlayerReplicationInfo:bOnlySpectator" -> return ()
                            "Engine.PlayerReplicationInfo:bOutOfLives" -> return ()
                            "Engine.PlayerReplicationInfo:bReadyToPlay" -> return ()
                            "Engine.PlayerReplicationInfo:bWaitingPlayer" -> return ()
                            "Engine.PlayerReplicationInfo:Deaths" -> return ()
                            "Engine.PlayerReplicationInfo:Ping" -> return ()
                            "Engine.PlayerReplicationInfo:PlayerID" -> return ()
                            "Engine.PlayerReplicationInfo:PlayerName" -> return ()
                            "Engine.PlayerReplicationInfo:Team" -> return ()
                            "Engine.PlayerReplicationInfo" -> return ()
                            "Engine.TeamInfo:TeamIndex" -> return ()
                            "Engine.TeamInfo" -> return ()
                            "GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype" -> return ()
                            "ProjectX.GRI_X:Reservations" -> return ()
                            "ProjectX.Pawn_X" -> return ()
                            "ProjectX.PRI_X" -> return ()
                            "TAGame.Ball_TA:bEndOfGameHidden" -> return ()
                            "TAGame.Ball_TA:GameEvent" -> return ()
                            "TAGame.Ball_TA:HitTeamNum" -> return ()
                            "TAGame.Ball_TA:ReplicatedExplosionData" -> return ()
                            "TAGame.Car_TA:TeamPaint" -> return ()
                            "TAGame.CarComponent_Boost_TA:bNoBoost" -> return ()
                            "TAGame.CarComponent_Boost_TA:BoostModifier" -> return ()
                            "TAGame.CarComponent_Boost_TA:bUnlimitedBoost" -> return ()
                            "TAGame.CarComponent_Boost_TA:ClientFixBoostAmount" -> return ()
                            "TAGame.CarComponent_Boost_TA:ClientGiveBoost" -> return ()
                            "TAGame.CarComponent_Boost_TA:CurrentBoostAmount" -> return ()
                            "TAGame.CarComponent_Boost_TA:ServerConfirmBoostAmount" -> return ()
                            "TAGame.CarComponent_Boost_TA" -> return ()
                            "TAGame.CarComponent_Dodge_TA:DodgeTorque" -> return ()
                            "TAGame.CarComponent_FlipCar_TA:FlipCarTime" -> return ()
                            "TAGame.CarComponent_Jump_TA" -> return ()
                            "TAGame.CrowdActor_TA:GameEvent" -> return ()
                            "TAGame.CrowdActor_TA:ModifiedNoise" -> return ()
                            "TAGame.CrowdManager_TA:GameEvent" -> return ()
                            "TAGame.GameEvent_TA" -> return ()
                            "TAGame.GameEvent_Team_TA:bDisableMutingOtherTeam" -> return ()
                            "TAGame.PRI_TA:bBusy" -> return ()
                            "TAGame.PRI_TA:bIsInSplitScreen" -> return ()
                            "TAGame.PRI_TA:bReady" -> return ()
                            "TAGame.PRI_TA:bUsingFreecam" -> return ()
                            "TAGame.PRI_TA:bUsingSecondaryCamera" -> return ()
                            "TAGame.PRI_TA:CameraPitch" -> return ()
                            "TAGame.PRI_TA:CameraSettings" -> return ()
                            "TAGame.PRI_TA:CameraYaw" -> return ()
                            "TAGame.PRI_TA:ClientLoadout" -> return ()
                            "TAGame.PRI_TA:MatchAssists" -> return ()
                            "TAGame.PRI_TA:MatchSaves" -> return ()
                            "TAGame.PRI_TA:MatchShots" -> return ()
                            "TAGame.PRI_TA:PartyLeader" -> return ()
                            "TAGame.PRI_TA:ReplicatedGameEvent" -> return ()
                            "TAGame.PRI_TA:RespawnTimeRemaining" -> return ()
                            "TAGame.PRI_TA:ServerChangeTeam" -> return ()
                            "TAGame.PRI_TA:ServerSetCameraRotation" -> return ()
                            "TAGame.PRI_TA:ServerSetCameraSettings" -> return ()
                            "TAGame.PRI_TA:ServerSetTotalXP" -> return ()
                            "TAGame.PRI_TA:ServerSetUsingBehindView" -> return ()
                            "TAGame.PRI_TA:ServerSetUsingFreecam" -> return ()
                            "TAGame.PRI_TA:ServerSetUsingSecondaryCamera" -> return ()
                            "TAGame.PRI_TA:ServerSpectate" -> return ()
                            "TAGame.PRI_TA:ServerVoteToForfeit" -> return ()
                            "TAGame.PRI_TA:TotalXP" -> return ()
                            "TAGame.PRI_TA" -> return ()
                            "TAGame.Team_Soccar_TA" -> return ()
                            "TAGame.Team_TA:CustomTeamName" -> return ()
                            "TAGame.Team_TA" -> return ()
                            "TAGame.Vehicle_TA:bReplicatedHandbrake" -> return ()
                            "TAGame.Vehicle_TA:ReplicatedSteer" -> return ()
                            "TAGame.Vehicle_TA:ReplicatedThrottle" -> return ()
                            "TAGame.VehiclePickup_Boost_TA" -> return ()
                            "TAGame.VehiclePickup_TA" -> return ()
                            "trainstation_p.TheWorld:PersistentLevel.VehiclePickup_Boost_TA_0" -> return ()
                            _ -> error ("unknown object: " ++ show object)
                        return [(time, delta, actorID, objectID, object)]))
    (replay |> replayFrames |> flipEndian |> BSL.fromStrict)

getInt10LE :: BB.BitGet Binary.Word16
getInt10LE = do
    a <- BB.getBool
    b <- BB.getBool
    c <- BB.getBool
    d <- BB.getBool
    e <- BB.getBool
    f <- BB.getBool
    g <- BB.getBool
    h <- BB.getBool
    i <- BB.getBool
    j <- BB.getBool
    Bits.zeroBits
        |> (if a then setBit 0 else id)
        |> (if b then setBit 1 else id)
        |> (if c then setBit 2 else id)
        |> (if d then setBit 3 else id)
        |> (if e then setBit 4 else id)
        |> (if f then setBit 5 else id)
        |> (if g then setBit 6 else id)
        |> (if h then setBit 7 else id)
        |> (if i then setBit 8 else id)
        |> (if j then setBit 9 else id)
        |> return

flipEndian :: BS.ByteString -> BS.ByteString
flipEndian bytes = BS.map flipByte bytes

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
