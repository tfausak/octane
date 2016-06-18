{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replay (Replay(..), fromRawReplay, toRawReplay) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LazyBytes
import qualified GHC.Generics as Generics
import qualified Octane.Type.RawReplay as RawReplay


-- | A processed Rocket League replay. Typically this is converted from a
-- 'RawReplay.RawReplay' value.
--
-- Although you can create these values directly, it is easier to use
-- 'fromRawReplay'.
data Replay = Replay
    deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON Replay where
    parseJSON json = case json of
        Aeson.Object _object -> pure Replay
        _ -> Aeson.typeMismatch "Replay" json

instance DeepSeq.NFData Replay where

instance Aeson.ToJSON Replay where
    toJSON _replay = Aeson.object []


-- | Converts a 'RawReplay.RawReplay' into a 'Replay'.
--
-- >>> fromRawReplay (RawReplay.RawReplay {RawReplay.headerSize = 0x00000000, RawReplay.headerCRC = 0xefcbf201, RawReplay.header = LazyBytes.empty, RawReplay.contentSize = 0x00000000, RawReplay.contentCRC = 0xefcbf201, RawReplay.content = LazyBytes.empty, RawReplay.footer = LazyBytes.empty})
-- Replay
fromRawReplay :: RawReplay.RawReplay -> Replay
fromRawReplay _rawReplay = do
    Replay


-- | Converts a 'Replay' into a 'RawReplay.RawReplay'. This is generally the
-- easiest way to create 'RawReplay.RawReplay' values that actually make sense.
--
-- >>> toRawReplay (Replay)
-- RawReplay {headerSize = 0x00000000, headerCRC = 0xefcbf201, header = "", contentSize = 0x00000000, contentCRC = 0xefcbf201, content = "", footer = ""}
toRawReplay :: Replay -> RawReplay.RawReplay
toRawReplay _replay = do
    let header = LazyBytes.empty
    let content = LazyBytes.empty
    let footer = LazyBytes.empty
    RawReplay.newRawReplay header content footer
