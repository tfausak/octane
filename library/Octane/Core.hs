module Octane.Core
    ( module Control.DeepSeq
    , module Control.Monad
    , module Data.Binary
    , module Data.Binary.Get
    , module Data.Binary.IEEE754
    , module Data.Binary.Put
    , module Data.ByteString
    , module Data.Char
    , module Data.Function
    , module Data.Int
    , module Data.IntMap
    , module Data.Map
    , module Data.Text
    , module Data.Text.Encoding
    , module GHC.Generics
    , module System.Environment
    , module System.IO
    ) where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM, when)
import Data.Binary (Binary, Get, Put, decodeFileOrFail, encode, get, getWord8, put, putWord8)
import Data.Binary.Get (ByteOffset, getByteString, getWord32le, getWord64le)
import Data.Binary.IEEE754 (getFloat32le, putFloat32le)
import Data.Binary.Put (putByteString, putWord32le, putWord64le)
import Data.ByteString (ByteString)
import Data.Char (isLatin1)
import Data.Function ((&))
import Data.Int (Int32, Int64)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, decodeUtf16LE, encodeUtf16LE)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdout)
