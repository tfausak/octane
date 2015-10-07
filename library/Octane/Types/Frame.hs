{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Octane.Types.Frame where

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as Binary
import qualified Data.Binary.Bits.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Flow ((|>))
import Octane.Types.Float32LE

data Frame = NewFrame {
    frameTime :: Float32LE,
    frameDelta :: Float32LE,
    frameHasActors :: Bool,
    frameActorID :: Maybe Binary.Word16,
    frameChannelOpen :: Maybe Bool,
    frameActorNew :: Maybe Bool,
    frameActorStatic :: Maybe Bool
} deriving (Show)

instance Binary.BinaryBit Frame where
    getBits _ = do
        timeBytes <- Binary.getByteString 4
        let time = timeBytes |> flipEndian |> BSL.fromStrict |> Binary.decode

        deltaBytes <- Binary.getByteString 4
        let delta = deltaBytes |> flipEndian |> BSL.fromStrict |> Binary.decode

        hasActors <- Binary.getBool
        let frame = NewFrame {
                frameTime = time,
                frameDelta = delta,
                frameHasActors = hasActors,
                frameActorID = Nothing,
                frameChannelOpen = Nothing,
                frameActorNew = Nothing,
                frameActorStatic = Nothing
            }

        if not hasActors
        then do -- this is the end of the frame
            return frame
        else do -- there is more to the frame
            actorID <- getInt10LE
            channelOpen <- Binary.getBool
            let frame = frame {
                    frameActorID = Just actorID,
                    frameChannelOpen = Just channelOpen
                }

            if not channelOpen
            then do -- the channel is closed and the actor is destroyed
                return frame
            else do -- the channel is open
                actorNew <- Binary.getBool
                let frame = frame {
                        frameActorNew = Just actorNew
                    }

                if not actorNew
                then do -- the actor already exists
                    -- TODO: while readbit is true, read property?
                    error "don't know what to do with existing actors"
                else do -- the actor does not already exist
                    actorStatic <- Binary.getBool
                    let frame = frame {
                            frameActorStatic = Just actorStatic
                        }

                    if actorStatic
                    then do -- the actor is static
                        -- TODO
                        error "don't know what to do with static actors"
                    else do -- the actor is dynamic
                        -- TODO: next 8? bits are actor type id?
                        -- TODO: optional initial location vector?
                        -- TODO: 3 bytes for pitch, yaw, and roll?
                        error "don't know what to do with dynamic actors"

    putBits _ _ = undefined -- TODO

-- TODO: This is awful. But it's the easiest way to get a 10-bit little-endian
--   integer from a frame.
getInt10LE :: Binary.BitGet Binary.Word16
getInt10LE = do
    a <- Binary.getBool
    b <- Binary.getBool
    c <- Binary.getBool
    d <- Binary.getBool
    e <- Binary.getBool
    f <- Binary.getBool
    g <- Binary.getBool
    h <- Binary.getBool
    i <- Binary.getBool
    j <- Binary.getBool
    return $
        (if a then flip Bits.setBit 0 else id) $
        (if b then flip Bits.setBit 1 else id) $
        (if c then flip Bits.setBit 2 else id) $
        (if d then flip Bits.setBit 3 else id) $
        (if e then flip Bits.setBit 4 else id) $
        (if f then flip Bits.setBit 5 else id) $
        (if g then flip Bits.setBit 6 else id) $
        (if h then flip Bits.setBit 7 else id) $
        (if i then flip Bits.setBit 8 else id) $
        (if j then flip Bits.setBit 9 else id) $
        Bits.zeroBits

-- TODO: There has got to be a better way.
flipEndian :: BS.ByteString -> BS.ByteString
flipEndian bytes = BS.map go bytes where
    go byte =
        let a = Bits.testBit byte 0
            b = Bits.testBit byte 1
            c = Bits.testBit byte 2
            d = Bits.testBit byte 3
            e = Bits.testBit byte 4
            f = Bits.testBit byte 5
            g = Bits.testBit byte 6
            h = Bits.testBit byte 7
        in  (if a then flip Bits.setBit 7 else id) $
            (if b then flip Bits.setBit 6 else id) $
            (if c then flip Bits.setBit 5 else id) $
            (if d then flip Bits.setBit 4 else id) $
            (if e then flip Bits.setBit 3 else id) $
            (if f then flip Bits.setBit 2 else id) $
            (if g then flip Bits.setBit 1 else id) $
            (if h then flip Bits.setBit 0 else id) $
            Bits.zeroBits
