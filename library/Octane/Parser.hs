module Octane.Parser where

import Octane.Internal.Core
import Octane.Type

import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

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
    -- TODO: Convert time bytes into a float.
    time <- Bits.getByteString 32
    -- TODO: Convert delta bytes into a float.
    delta <- Bits.getByteString 32

    if BS.all (== 0) time && BS.all (== 0) delta
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
    (context', maybeReplication) <- getReplication context
    case maybeReplication of
        Nothing -> return []
        Just replication -> do
            replications <- getReplications context'
            return (replication : replications)

getReplication :: Context -> Bits.BitGet (Context, Maybe Replication)
getReplication context = do
    hasReplication <- Bits.getBool
    if not hasReplication
    then return (context, Nothing)
    else do
        -- TODO: Convert actor ID into an integer.
        actorId <- Bits.getByteString (bitSize maxChannels)
        isOpen <- Bits.getBool
        if isOpen
        then do
            isNew <- Bits.getBool
            if isNew
            then do
                -- TODO: Parse new actor.
                return (context, Just (Replication
                    { replicationActorId = actorId
                    , replicationIsOpen = isOpen
                    , replicationIsNew = Just isNew
                    }))
            else do
                -- TODO: Parse existing actor.
                return (context, Just (Replication
                    { replicationActorId = actorId
                    , replicationIsOpen = isOpen
                    , replicationIsNew = Just isNew
                    }))
        else return (context, Just (Replication
            { replicationActorId = actorId
            , replicationIsOpen = isOpen
            , replicationIsNew = Nothing
            }))

maxChannels :: (Integral a) => a
maxChannels = 1024

bitSize :: (Integral a) => a -> a
bitSize x = x & fromIntegral & logBase (2 :: Double) & ceiling
