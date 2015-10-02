module Octane.Types.Frame where

import Octane.Types.Float32LE (Float32LE)
import Octane.Utilities (flipEndianness)

import Data.Bits (setBit, zeroBits)

import qualified Data.Binary as B
import qualified Data.Binary.Bits as BB
import qualified Data.Binary.Bits.Get as BB
import qualified Data.ByteString.Lazy as BSL

data Frame = NewFrame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    , frameHasActors :: Bool
    , frameActorID :: Maybe B.Word16
    } deriving (Show)

instance BB.BinaryBit Frame where
    getBits _ = do
        timeBytes <- BB.getByteString 4
        deltaBytes <- BB.getByteString 4
        hasActors <- BB.getBool
        if hasActors
        then do -- there is more to the frame
            actorID <- getInt10LE
            return NewFrame
                { frameTime = B.decode (BSL.fromStrict (flipEndianness timeBytes))
                , frameDelta = B.decode (BSL.fromStrict (flipEndianness deltaBytes))
                , frameHasActors = hasActors
                , frameActorID = Just actorID
                }
        else do -- this is the end of the frame
            return NewFrame
                { frameTime = B.decode (BSL.fromStrict (flipEndianness timeBytes))
                , frameDelta = B.decode (BSL.fromStrict (flipEndianness deltaBytes))
                , frameHasActors = hasActors
                , frameActorID = Nothing
                }

    putBits _ _ = undefined

-- TODO: This is awful. But it's the easiest way to get a 10-bit little-endian
--   integer from a frame.
getInt10LE :: BB.BitGet B.Word16
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
    return $
        (if a then flip setBit 0 else id) $
        (if b then flip setBit 1 else id) $
        (if c then flip setBit 2 else id) $
        (if d then flip setBit 3 else id) $
        (if e then flip setBit 4 else id) $
        (if f then flip setBit 5 else id) $
        (if g then flip setBit 6 else id) $
        (if h then flip setBit 7 else id) $
        (if i then flip setBit 8 else id) $
        (if j then flip setBit 9 else id) $
        zeroBits
