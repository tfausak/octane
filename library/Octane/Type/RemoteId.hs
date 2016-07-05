{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.RemoteId
    ( RemoteId(..)
    , SteamId(..)
    , PlayStationId(..)
    , SplitscreenId(..)
    , XboxId(..)
    ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as Encoding
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Type.Word8 as Word8
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


-- | A player's canonical remote ID. This is the best way to uniquely identify
-- players
data RemoteId
    = RemotePlayStationId PlayStationId
    | RemoteSplitscreenId SplitscreenId
    | RemoteSteamId SteamId
    | RemoteXboxId XboxId
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RemoteId where

-- | Encodes the remote ID as an object with "Type" and "Value" keys.
--
-- >>> Aeson.encode (RemoteSplitscreenId (SplitscreenId (Just 1)))
-- "{\"Value\":1,\"Type\":\"Splitscreen\"}"
instance Aeson.ToJSON RemoteId where
    toJSON remoteId = case remoteId of
        RemotePlayStationId x -> Aeson.object
            [ "Type" .= ("PlayStation" :: Text.Text)
            , "Value" .= Aeson.toJSON x
            ]
        RemoteSplitscreenId x -> Aeson.object
            [ "Type" .= ("Splitscreen" :: Text.Text)
            , "Value" .= Aeson.toJSON x
            ]
        RemoteSteamId x -> Aeson.object
            [ "Type" .= ("Steam" :: Text.Text)
            , "Value" .= Aeson.toJSON x
            ]
        RemoteXboxId x -> Aeson.object
            [ "Type" .= ("Xbox" :: Text.Text)
            , "Value" .= Aeson.toJSON x
            ]


data PlayStationId = PlayStationId
    { playStationName :: Text.Text
    , playStationUnknown :: LazyBytes.ByteString
    } deriving (Eq, Generics.Generic, Show)

-- | Each part is stored as exactly 16 bits.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80" :: PlayStationId
-- PlayStationId {playStationName = "B", playStationUnknown = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (PlayStationId "A" "\x01")))
-- "\130\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\128"
instance BinaryBit.BinaryBit PlayStationId where
    getBits _ = do
        nameBytes <- BinaryBit.getByteString 16
        let name = nameBytes
                & Endian.reverseBitsInStrictBytes
                & Encoding.decodeLatin1
                & StrictText.dropWhileEnd (== '\0')
                & Text.Text

        unknownBytes <- BinaryBit.getByteString 16
        let unknown = unknownBytes
                & Endian.reverseBitsInStrictBytes
                & LazyBytes.fromStrict

        pure (PlayStationId name unknown)

    putBits _ playStationId = do
        playStationId
            & playStationName
            & Text.unpack
            & StrictText.justifyLeft 16 '\x00'
            & StrictText.take 16
            & Text.encodeLatin1
            & Endian.reverseBitsInStrictBytes
            & BinaryBit.putByteString

        playStationId
            & playStationUnknown
            & LazyBytes.toStrict
            & Endian.reverseBitsInStrictBytes
            & BinaryBit.putByteString

instance DeepSeq.NFData PlayStationId where

-- | >>> Aeson.encode (PlayStationId "A" "B")
-- "{\"Unknown\":\"0x42\",\"Name\":\"A\"}"
instance Aeson.ToJSON PlayStationId where
    toJSON playStationId = Aeson.object
        [ "Name" .= playStationName playStationId
        , "Unknown" .= (playStationId
            & playStationUnknown
            & LazyBytes.unpack
            & concatMap (Printf.printf "%02x")
            & ("0x" ++)
            & StrictText.pack)
        ]


newtype SplitscreenId = SplitscreenId
    { unpackSplitscreenId :: Maybe Int
    } deriving (Eq, Generics.Generic, Show)

-- | Stored as a bare byte string.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x00\x00\x00" :: SplitscreenId
-- SplitscreenId {unpackSplitscreenId = Just 0}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (SplitscreenId (Just 0))))
-- "\NUL\NUL\NUL"
instance BinaryBit.BinaryBit SplitscreenId where
    getBits _ = do
        bytes <- BinaryBit.getByteString 3
        case bytes of
            "\x00\x00\x00" -> do
                pure (SplitscreenId (Just 0))
            _ -> do
                fail ("Unexpected SplitscreenId value " ++ show bytes)

    putBits _ _splitscreenId = do
        BinaryBit.putByteString "\x00\x00\x00"

instance DeepSeq.NFData SplitscreenId where

-- | Encoded as an optional number.
--
-- >>> Aeson.encode (SplitscreenId Nothing)
-- "null"
--
-- >>> Aeson.encode (SplitscreenId (Just 0))
-- "0"
instance Aeson.ToJSON SplitscreenId where
    toJSON splitscreenId = splitscreenId & unpackSplitscreenId & Aeson.toJSON


data SteamId = SteamId
    { steamUniverse :: Word8.Word8
    , steamType :: Word.Word8
    , steamInstance :: Word.Word32
    , steamNumber :: Word32.Word32
    } deriving (Eq, Generics.Generic, Show)

-- | Stored in this order: number, instance, type, universe. For byte-aligned
-- values, the bits in each byte are reversed. For other values, all bits are
-- reversed.
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (SteamId 1 2 3 4)))
-- " \NUL\NUL\NUL\192\NUL\EOT\128"
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x20\x00\x00\x00\xc0\x00\x04\x80" :: SteamId
-- SteamId {steamUniverse = 0x01, steamType = 2, steamInstance = 3, steamNumber = 0x00000004}
instance BinaryBit.BinaryBit SteamId where
    getBits _ = do
        number_ <- BinaryBit.getBits 0
        instance_ <- BinaryBit.getWord32be 20
        type_ <- BinaryBit.getWord8 4
        universe <- BinaryBit.getBits 0

        pure (SteamId
            { steamUniverse = universe
            , steamType = Endian.reverseBits4 type_
            , steamInstance = Endian.reverseBits20 instance_
            , steamNumber = number_
            })

    putBits _ steamId = do
        steamId & steamNumber & BinaryBit.putBits 0
        steamId & steamInstance & Endian.reverseBits20 & BinaryBit.putWord32be 20
        steamId & steamType & Endian.reverseBits4 & BinaryBit.putWord8 4
        steamId & steamUniverse & BinaryBit.putBits 0

instance DeepSeq.NFData SteamId where

-- | >>> Aeson.encode (SteamId 1 2 3 4)
-- "{\"Universe\":1,\"Type\":2,\"Number\":4,\"Instance\":3}"
instance Aeson.ToJSON SteamId where
    toJSON steamId = Aeson.object
        [ "Universe" .= steamUniverse steamId
        , "Type" .= steamType steamId
        , "Instance" .= steamInstance steamId
        , "Number" .= steamNumber steamId
        ]


newtype XboxId = XboxId
    { unpackXboxId :: Word64.Word64
    } deriving (Eq, Generics.Generic, Show)

-- | Stored as a plain 'Word64.Word64'.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x80\x00\x00\x00\x00\x00\x00\x00" :: XboxId
-- XboxId {unpackXboxId = 0x0000000000000001}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (XboxId 1)))
-- "\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
instance BinaryBit.BinaryBit XboxId where
    getBits _ = do
        xboxId <- BinaryBit.getBits 0
        pure (XboxId xboxId)

    putBits _ xboxId = xboxId & unpackXboxId & BinaryBit.putBits 0


instance DeepSeq.NFData XboxId where

-- | Encoded directly as a number.
--
-- >>> Aeson.encode (XboxId 1)
-- "1"
instance Aeson.ToJSON XboxId where
    toJSON xboxId = xboxId & unpackXboxId & Aeson.toJSON
