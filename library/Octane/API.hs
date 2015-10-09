{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Octane.API where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Proxy as Proxy
import Octane.Parser.Types
import qualified Servant.API as Servant
import Servant.API ((:>))

api :: Proxy.Proxy API
api = Proxy.Proxy

type API
    = UploadReplay

type UploadReplay
    = "replays"
    :> Servant.ReqBody '[Servant.OctetStream] BSL.ByteString
    :> Servant.Post '[Servant.JSON] Replay
