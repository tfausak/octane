module Octane.Utility.Generator (generateStream) where

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.List as List
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text


-- | Generates a network stream.
generateStream
    :: [Frame.Frame]
    -> List.List Text.Text
    -> List.List Text.Text
    -> List.List ClassItem.ClassItem
    -> List.List CacheItem.CacheItem
    -> Stream.Stream
generateStream _frames _objects _names _classes _cache = do
    Stream.Stream LazyBytes.empty -- TODO
