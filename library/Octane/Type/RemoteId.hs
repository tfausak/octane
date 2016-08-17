module Octane.Type.RemoteId
    ( RemoteId(..)
    , SteamId(..)
    , PlayStationId(..)
    , SplitscreenId(..)
    , XboxId(..)
    ) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as Encoding
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


data PlayStationId = PlayStationId
    { playStationIdName :: Text.Text
    , playStationIdUnknown :: LazyBytes.ByteString
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''PlayStationId)

-- | Each part is stored as exactly 16 bits.
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
            & #name
            & #unpack
            & StrictText.justifyLeft 16 '\x00'
            & StrictText.take 16
            & Text.encodeLatin1
            & Endian.reverseBitsInStrictBytes
            & BinaryBit.putByteString

        playStationId
            & #unknown
            & LazyBytes.toStrict
            & Endian.reverseBitsInStrictBytes
            & BinaryBit.putByteString

instance NFData PlayStationId where

instance ToJSON PlayStationId where
    toJSON playStationId = Aeson.object
        [ "Name" .= #name playStationId
        , "Unknown" .= (playStationId
            & #unknown
            & LazyBytes.unpack
            & concatMap (Printf.printf "%02x")
            & ("0x" ++)
            & StrictText.pack)
        ]


newtype SplitscreenId = SplitscreenId
    { splitscreenIdUnpack :: Maybe Int
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''SplitscreenId)

-- | Stored as a bare byte string.
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

instance NFData SplitscreenId where

-- | Encoded as an optional number.
instance ToJSON SplitscreenId where
    toJSON splitscreenId = splitscreenId & #unpack & toJSON


newtype SteamId = SteamId
    { steamIdUnpack :: Word64.Word64
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''SteamId)

-- | Stored as a plain 'Word64.Word64'.
instance BinaryBit.BinaryBit SteamId where
    getBits _ = do
        steamId <- BinaryBit.getBits 0
        pure (SteamId steamId)

    putBits _ steamId = steamId & #unpack & BinaryBit.putBits 0

instance NFData SteamId where

-- | Encoded directly as a number.
instance ToJSON SteamId where
    toJSON steamId = steamId & #unpack & toJSON


newtype XboxId = XboxId
    { xboxIdUnpack :: Word64.Word64
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''XboxId)

-- | Stored as a plain 'Word64.Word64'.
instance BinaryBit.BinaryBit XboxId where
    getBits _ = do
        xboxId <- BinaryBit.getBits 0
        pure (XboxId xboxId)

    putBits _ xboxId = xboxId & #unpack & BinaryBit.putBits 0


instance NFData XboxId where

-- | Encoded directly as a number.
instance ToJSON XboxId where
    toJSON xboxId = xboxId & #unpack & toJSON


-- | A player's canonical remote ID. This is the best way to uniquely identify
-- players
data RemoteId
    = RemotePlayStationId PlayStationId
    | RemoteSplitscreenId SplitscreenId
    | RemoteSteamId SteamId
    | RemoteXboxId XboxId
    deriving (Eq, Generic, Show)

instance NFData RemoteId where

-- | Encodes the remote ID as an object with "Type" and "Value" keys.
instance ToJSON RemoteId where
    toJSON remoteId = case remoteId of
        RemotePlayStationId x -> Aeson.object
            [ "Type" .= ("PlayStation" :: Text.Text)
            , "Value" .= toJSON x
            ]
        RemoteSplitscreenId x -> Aeson.object
            [ "Type" .= ("Splitscreen" :: Text.Text)
            , "Value" .= toJSON x
            ]
        RemoteSteamId x -> Aeson.object
            [ "Type" .= ("Steam" :: Text.Text)
            , "Value" .= toJSON x
            ]
        RemoteXboxId x -> Aeson.object
            [ "Type" .= ("Xbox" :: Text.Text)
            , "Value" .= toJSON x
            ]
