{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RawReplay
    ( RawReplay(..)
    , newRawReplay
    ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Utility.CRC as CRC
import qualified Octane.Type.Word32 as Word32
import qualified Text.Printf as Printf


-- | A raw, unprocessed replay. Only enough parsing is done to make sure that
-- the CRCs are valid.
--
-- See 'Octane.Type.ReplayWithoutFrames.ReplayWithoutFrames'.
data RawReplay = RawReplay
    { rawReplayHeaderSize :: Word32.Word32
    -- ^ The byte size of the first section.
    , rawReplayHeaderCRC :: Word32.Word32
    -- ^ The CRC of the first section.
    , rawReplayHeader :: LazyBytes.ByteString
    -- ^ The first section.
    , rawReplayContentSize :: Word32.Word32
    -- ^ The byte size of the second section.
    , rawReplayContentCRC :: Word32.Word32
    -- ^ The CRC of the second section.
    , rawReplayContent :: LazyBytes.ByteString
    -- ^ The second section.
    , rawReplayFooter :: LazyBytes.ByteString
    -- ^ Arbitrary data after the second section. In replays generated by
    -- Rocket League, this is always empty. However it is not technically
    -- invalid to put something here.
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''RawReplay)

-- | Decoding will fail if the CRCs don't match, but it is possible to encode
-- invalid replays. That means @decode (encode rawReplay)@ can fail.
instance Binary.Binary RawReplay where
    get = do
        headerSize <- Binary.get
        headerCRC <- Binary.get
        header <- Binary.getLazyByteString (Word32.fromWord32 headerSize)
        checkCRC headerCRC header

        contentSize <- Binary.get
        contentCRC <- Binary.get
        content <- Binary.getLazyByteString (Word32.fromWord32 contentSize)
        checkCRC contentCRC content

        footer <- Binary.getRemainingLazyByteString

        pure (RawReplay headerSize headerCRC header contentSize contentCRC content footer)

    put replay = do
        Binary.put (#headerSize replay)
        Binary.put (#headerCRC replay)
        Binary.putLazyByteString (#header replay)

        Binary.put (#contentSize replay)
        Binary.put (#contentCRC replay)
        Binary.putLazyByteString (#content replay)

        Binary.putLazyByteString (#footer replay)

instance DeepSeq.NFData RawReplay where


-- | Creates a new 'RawReplay'. You should prefer this over directly using the
-- constructor so that the sizes and CRCs are set correctly.
newRawReplay
    :: LazyBytes.ByteString -- ^ The header.
    -> LazyBytes.ByteString -- ^ The content.
    -> LazyBytes.ByteString -- ^ The footer.
    -> RawReplay
newRawReplay header content footer = do
    let headerSize = Word32.toWord32 (LazyBytes.length header)
    let headerCRC = Word32.Word32 (CRC.crc32 header)

    let contentSize = Word32.toWord32 (LazyBytes.length content)
    let contentCRC = Word32.Word32 (CRC.crc32 content)

    RawReplay headerSize headerCRC header contentSize contentCRC content footer


checkCRC :: (Monad m) => Word32.Word32 -> LazyBytes.ByteString -> m ()
checkCRC (Word32.Word32 expected) bytes = do
    let actual = CRC.crc32 bytes
    Monad.when (actual /= expected) (do
        let message = Printf.printf
                "CRC 0x%08x does not equal expected value 0x%08x"
                actual
                expected
        fail message)
