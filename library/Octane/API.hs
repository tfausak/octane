{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Octane.API where

import qualified Data.ByteString as BS
import Octane.Parser.Types
import qualified Servant.API as Servant
import Servant.API ((:>))

type API
    = UploadReplay

type UploadReplay
    = "replays"
    :> Servant.ReqBody '[Servant.OctetStream] BS.ByteString
    :> Servant.Post '[Servant.JSON] Replay
