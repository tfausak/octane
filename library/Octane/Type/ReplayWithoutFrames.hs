{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.ReplayWithoutFrames (ReplayWithoutFrames(..), fromRawReplay, toRawReplay) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified GHC.Generics as Generics
import qualified Octane.Type.RawReplay as RawReplay
import qualified Octane.Type.Word32 as Word32


data ReplayWithoutFrames = ReplayWithoutFrames
    { version1 :: Word32.Word32
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary ReplayWithoutFrames where
    get = do
        rawReplay <- Binary.get
        pure (fromRawReplay rawReplay)

    put replayWithoutFrames = do
        let rawReplay = toRawReplay replayWithoutFrames
        Binary.put rawReplay

instance DeepSeq.NFData ReplayWithoutFrames where


fromRawReplay :: RawReplay.RawReplay -> ReplayWithoutFrames
fromRawReplay rawReplay = do
    let header = RawReplay.header rawReplay
    let content = RawReplay.content rawReplay
    Binary.runGet
        (do
            version1 <- Binary.get

            pure ReplayWithoutFrames { .. })
        (LazyBytes.append header content)


toRawReplay :: ReplayWithoutFrames -> RawReplay.RawReplay
toRawReplay _replay = do
    let header = LazyBytes.empty
    let content = LazyBytes.empty
    let footer = LazyBytes.empty
    RawReplay.newRawReplay header content footer
