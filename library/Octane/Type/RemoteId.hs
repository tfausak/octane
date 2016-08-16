{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


data PlayStationId = PlayStationId
    { playStationIdName :: Text.Text
    , playStationIdUnknown :: LazyBytes.ByteString
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''PlayStationId)

-- | Each part is stored as exactly 16 bits.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80" :: PlayStationId
-- PlayStationId {playStationIdName = "B", playStationIdUnknown = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"}
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

instance DeepSeq.NFData PlayStationId where

-- | >>> Aeson.encode (PlayStationId "A" "B")
-- "{\"Unknown\":\"0x42\",\"Name\":\"A\"}"
instance Aeson.ToJSON PlayStationId where
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
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''SplitscreenId)

-- | Stored as a bare byte string.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x00\x00\x00" :: SplitscreenId
-- SplitscreenId {splitscreenIdUnpack = Just 0}
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
    toJSON splitscreenId = splitscreenId & #unpack & Aeson.toJSON


newtype SteamId = SteamId
    { steamIdUnpack :: Word64.Word64
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''SteamId)

-- | Stored as a plain 'Word64.Word64'.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x80\x00\x00\x00\x00\x00\x00\x00" :: SteamId
-- SteamId {steamIdUnpack = 0x0000000000000001}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (SteamId 1)))
-- "\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
instance BinaryBit.BinaryBit SteamId where
    getBits _ = do
        steamId <- BinaryBit.getBits 0
        pure (SteamId steamId)

    putBits _ steamId = steamId & #unpack & BinaryBit.putBits 0

instance DeepSeq.NFData SteamId where

-- | Encoded directly as a number.
--
-- >>> Aeson.encode (SteamId 1)
-- "1"
instance Aeson.ToJSON SteamId where
    toJSON steamId = steamId & #unpack & Aeson.toJSON


newtype XboxId = XboxId
    { xboxIdUnpack :: Word64.Word64
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''XboxId)

-- | Stored as a plain 'Word64.Word64'.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x80\x00\x00\x00\x00\x00\x00\x00" :: XboxId
-- XboxId {xboxIdUnpack = 0x0000000000000001}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (XboxId 1)))
-- "\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
instance BinaryBit.BinaryBit XboxId where
    getBits _ = do
        xboxId <- BinaryBit.getBits 0
        pure (XboxId xboxId)

    putBits _ xboxId = xboxId & #unpack & BinaryBit.putBits 0


instance DeepSeq.NFData XboxId where

-- | Encoded directly as a number.
--
-- >>> Aeson.encode (XboxId 1)
-- "1"
instance Aeson.ToJSON XboxId where
    toJSON xboxId = xboxId & #unpack & Aeson.toJSON


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
-- >>> Aeson.encode (RemoteSteamId (SteamId 1))
-- "{\"Value\":1,\"Type\":\"Steam\"}"
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
