module Octane.Parser where

import qualified Control.Newtype as Newtype
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Octane.Type as Type

parseFrames :: Type.Replay -> [Type.Frame]
parseFrames replay =
    Binary.runGet
        (replay & extractContext & getFrames & Bits.runBitGet)
        (replay & Type.replayStream & Newtype.unpack & BSL.fromStrict)

-- TODO: This will need at least the actors and cache items.
data Context = Context
    {
    }

extractContext :: Type.Replay -> Context
extractContext _replay =
    Context
    {
    }

getFrames :: Context -> Bits.BitGet [Type.Frame]
getFrames context = do
    maybeFrame <- getFrame context
    case maybeFrame of
        Nothing -> return []
        Just frame -> do
            frames <- getFrames context
            return (frame : frames)

getFrame :: Context -> Bits.BitGet (Maybe Type.Frame)
getFrame context = do
    -- TODO: Convert time bytes into a float.
    time <- Bits.getByteString 32
    -- TODO: Convert delta bytes into a float.
    delta <- Bits.getByteString 32
    if BS.all (== 0) time && BS.all (== 0) delta
        then return Nothing
        else do
            replications <- getReplications context
            let frame =
                    Type.Frame
                    { Type.frameTime = time
                    , Type.frameDelta = delta
                    , Type.frameReplications = replications
                    }
            return (Just frame)

getReplications :: Context -> Bits.BitGet [Type.Replication]
getReplications context = do
    (context',maybeReplication) <- getReplication context
    case maybeReplication of
        Nothing -> return []
        Just replication -> do
            replications <- getReplications context'
            return (replication : replications)

getReplication :: Context -> Bits.BitGet (Context, Maybe Type.Replication)
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
                            _unknownFlag <- Bits.getBool
                            _objectId <- getInt 32
                            -- TODO: Parse new actor.
                            return
                                ( context
                                , Just
                                      (Type.Replication
                                       { Type.replicationActorId = actorId
                                       , Type.replicationIsOpen = isOpen
                                       , Type.replicationIsNew = Just isNew
                                       }))
                        else do
                            -- TODO: Parse existing actor.
                            return
                                ( context
                                , Just
                                      (Type.Replication
                                       { Type.replicationActorId = actorId
                                       , Type.replicationIsOpen = isOpen
                                       , Type.replicationIsNew = Just isNew
                                       }))
                else return
                         ( context
                         , Just
                               (Type.Replication
                                { Type.replicationActorId = actorId
                                , Type.replicationIsOpen = isOpen
                                , Type.replicationIsNew = Nothing
                                }))

maxChannels
    :: (Integral a)
    => a
maxChannels = 1024

bitSize
    :: (Integral a)
    => a -> a
bitSize x = x & fromIntegral & logBase (2 :: Double) & ceiling

-- Reads an integer bitwise. The bits of the integer are backwards, so the
-- least significant bit is first. The argument is the maximum value this
-- integer can have. Bits will be read until the next bit would be greater than
-- the maximum value, or the number of bits necessary to reach the maximum
-- value has been reached, whichever comes first.
--
-- For example, if the maximum value is 4 and "11" has been read already,
-- nothing more will be read because another "1" would put the value over the
-- maximum.
getInt
    :: Int -> Bits.BitGet Int
getInt maxValue = do
    let maxBits = bitSize maxValue
        go i value = do
            let x = Bits.shiftL 1 i
            if i < maxBits && value + x <= maxValue
                then do
                    bit <- Bits.getBool
                    let newValue =
                            if bit
                                then value + x
                                else value
                    go (i + 1) newValue
                else return value
    go 0 0
