{-# LANGUAGE BinaryLiterals #-}

module Octane.Parser where

import Octane.Internal.Core
import Octane.Type

import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as BSL
import qualified Unsafe.Coerce as Coerce

parseFrames :: Replay -> [Frame]
parseFrames replay = Binary.runGet
    (replay & extractContext & getFrames & Bits.runBitGet)
    (replay & replayStream & unpack & BSL.fromStrict)

-- TODO: This will need at least the actors and cache items.
data Context = Context
    {
    }

extractContext :: Replay -> Context
extractContext _replay = Context
    {
    }

getFrames :: Context -> Bits.BitGet [Frame]
getFrames context = do
    maybeFrame <- getFrame context
    case maybeFrame of
        Nothing -> return []
        Just frame -> do
            frames <- getFrames context
            return (frame : frames)

getFrame :: Context -> Bits.BitGet (Maybe Frame)
getFrame context = do
    rawTime <- Bits.getWord32be 32
    rawDelta <- Bits.getWord32be 32

    let time = wordToFloat rawTime
    let delta = wordToFloat rawDelta

    if time == 0 && delta == 0
    then return Nothing
    else do
        replications <- getReplications context

        let frame = Frame
                { frameTime = time
                , frameDelta = delta
                , frameReplications = replications
                }

        return (Just frame)

getReplications :: Context -> Bits.BitGet [Replication]
getReplications context = do
    maybeReplication <- getReplication context
    case maybeReplication of
        Nothing -> return []
        Just replication -> do
            replications <- getReplications context
            return (replication : replications)

getReplication :: Context -> Bits.BitGet (Maybe Replication)
getReplication _context = do
    hasReplication <- Bits.getBool
    if not hasReplication
    then return Nothing
    else do
        -- TODO
        let replication = Replication
                {
                }

        return (Just replication)

-- TODO: Does this actually work? I don't know yet.
wordToFloat :: Word32 -> Float
wordToFloat word = word & reverseBits & Coerce.unsafeCoerce

-- TODO: This is also in the Stream. There has to be a more general way
-- to do it.
reverseBits :: Word32 -> Word32
reverseBits word
    = Bits.shiftR (word Bits..&. 0b10000000000000000000000000000000) 31
    + Bits.shiftR (word Bits..&. 0b01000000000000000000000000000000) 29
    + Bits.shiftR (word Bits..&. 0b00100000000000000000000000000000) 27
    + Bits.shiftR (word Bits..&. 0b00010000000000000000000000000000) 25
    + Bits.shiftR (word Bits..&. 0b00001000000000000000000000000000) 23
    + Bits.shiftR (word Bits..&. 0b00000100000000000000000000000000) 21
    + Bits.shiftR (word Bits..&. 0b00000010000000000000000000000000) 19
    + Bits.shiftR (word Bits..&. 0b00000001000000000000000000000000) 17
    + Bits.shiftR (word Bits..&. 0b00000000100000000000000000000000) 15
    + Bits.shiftR (word Bits..&. 0b00000000010000000000000000000000) 13
    + Bits.shiftR (word Bits..&. 0b00000000001000000000000000000000) 11
    + Bits.shiftR (word Bits..&. 0b00000000000100000000000000000000) 9
    + Bits.shiftR (word Bits..&. 0b00000000000010000000000000000000) 7
    + Bits.shiftR (word Bits..&. 0b00000000000001000000000000000000) 5
    + Bits.shiftR (word Bits..&. 0b00000000000000100000000000000000) 3
    + Bits.shiftR (word Bits..&. 0b00000000000000010000000000000000) 1
    + Bits.shiftL (word Bits..&. 0b00000000000000001000000000000000) 1
    + Bits.shiftL (word Bits..&. 0b00000000000000000100000000000000) 3
    + Bits.shiftL (word Bits..&. 0b00000000000000000010000000000000) 5
    + Bits.shiftL (word Bits..&. 0b00000000000000000001000000000000) 7
    + Bits.shiftL (word Bits..&. 0b00000000000000000000100000000000) 9
    + Bits.shiftL (word Bits..&. 0b00000000000000000000010000000000) 11
    + Bits.shiftL (word Bits..&. 0b00000000000000000000001000000000) 13
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000100000000) 15
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000010000000) 17
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000001000000) 19
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000000100000) 21
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000000010000) 23
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000000001000) 25
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000000000100) 27
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000000000010) 29
    + Bits.shiftL (word Bits..&. 0b00000000000000000000000000000001) 31
