module Octane.Parser where

import Octane.Internal.Core
import Octane.Type

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Unsafe.Coerce as Coerce

parseFrames :: Replay -> [Frame]
parseFrames replay = Binary.runGet
    (Bits.runBitGet (getFrames replay))
    (replay & replayStream & unpack & BSL.fromStrict)

getFrames :: Replay -> Bits.BitGet [Frame]
getFrames replay = do
    maybeFrame <- getFrame replay
    case maybeFrame of
        Nothing -> return []
        Just frame -> do
            frames <- getFrames replay
            return (frame : frames)

getFrame :: Replay -> Bits.BitGet (Maybe Frame)
getFrame replay = do
    rawTime <- Bits.getWord32be 32
    rawDelta <- Bits.getWord32be 32

    let time = wordToFloat rawTime
    let delta = wordToFloat rawDelta

    if time == 0 && delta == 0
    then return Nothing
    else do
        replications <- getReplications replay

        let frame = Frame
                { frameTime = time
                , frameDelta = delta
                , frameReplications = replications
                }

        return (Just frame)

getReplications :: Replay -> Bits.BitGet [Replication]
getReplications replay = do
    maybeReplication <- getReplication replay
    case maybeReplication of
        Nothing -> return []
        Just replication -> do
            replications <- getReplications replay
            return (replication : replications)

getReplication :: Replay -> Bits.BitGet (Maybe Replication)
getReplication _replay = do
    hasReplication <- Bits.getBool
    if not hasReplication
    then return Nothing
    else do
        -- TODO
        undefined

-- TODO: Does this actually work? I don't know yet.
wordToFloat :: Word32 -> Float
wordToFloat = Coerce.unsafeCoerce
