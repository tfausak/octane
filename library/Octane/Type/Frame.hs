{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Frame (Frame(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString as BS
import qualified GHC.Generics as Generics
import qualified Octane.Type.Replication as Replication

data Frame = Frame
    { frameTime :: BS.ByteString
    , frameDelta :: BS.ByteString
    , frameReplications :: [Replication.Replication]
    } deriving (Eq,Generics.Generic,Show)

instance DeepSeq.NFData Frame
