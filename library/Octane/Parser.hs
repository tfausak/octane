module Octane.Parser where

import Octane.Internal.Core
import Octane.Type

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
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
        undefined

-- TODO: Does this actually work? I don't know yet.
wordToFloat :: Word32 -> Float
wordToFloat = Coerce.unsafeCoerce
