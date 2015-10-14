module Octane.Server where

import qualified Control.Monad.Trans.Either as Either
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane.API
import Octane.Parser.Types
import qualified Servant as Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 8080 application

application :: Wai.Application
application = Servant.serve api server

server :: Servant.Server API
server
    = uploadReplay

uploadReplay :: BSL.ByteString -> Handler Replay
uploadReplay bytes = do
    -- TODO: Upload replay to S3 or something.
    let replay = Binary.decode bytes
    return replay

type Handler a = Either.EitherT Servant.ServantErr IO a
