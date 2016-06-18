{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.ReplayWithoutFrames (ReplayWithoutFrames(..), fromRawReplay, toRawReplay) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified GHC.Generics as Generics
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.List as List
import qualified Octane.Type.Property as Property
import qualified Octane.Type.RawReplay as RawReplay
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


data ReplayWithoutFrames = ReplayWithoutFrames
    { version1 :: Word32.Word32
    , version2 :: Word32.Word32
    , label :: Text.Text
    , properties :: Dictionary.Dictionary Property.Property
    , levels :: List.List Text.Text
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary ReplayWithoutFrames where
    get = do
        rawReplay <- Binary.get
        fromRawReplay rawReplay

    put replayWithoutFrames = do
        rawReplay <- toRawReplay replayWithoutFrames
        Binary.put rawReplay

instance DeepSeq.NFData ReplayWithoutFrames where


fromRawReplay :: (Monad m) => RawReplay.RawReplay -> m ReplayWithoutFrames
fromRawReplay rawReplay = do
    let header = RawReplay.header rawReplay
    let content = RawReplay.content rawReplay

    let get = do
            version1 <- Binary.get
            version2 <- Binary.get
            label <- Binary.get
            properties <- Binary.get
            levels <- Binary.get

            pure ReplayWithoutFrames { .. }
    let bytes = LazyBytes.append header content

    pure (Binary.runGet get bytes)


toRawReplay :: (Monad m) => ReplayWithoutFrames -> m RawReplay.RawReplay
toRawReplay replay = do
    let header = Binary.runPut (do
            Binary.put (version1 replay)
            Binary.put (version2 replay)
            Binary.put (label replay)
            Binary.put (properties replay)
            Binary.put (levels replay))

    let content = LazyBytes.empty

    let footer = LazyBytes.empty

    pure (RawReplay.newRawReplay header content footer)
