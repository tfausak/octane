{- |
    A length-prefixed list of objects.
-}
module Octane.Parser.Types.List where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Flow ((|>))
import Octane.Parser.Types.Int32LE

newtype List a = NewList {
    getList :: [a]
} deriving (Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (List a) where
    toJSON (NewList list) = Aeson.toJSON list

instance (Binary.Binary a) => Binary.Binary (List a) where
    get = do
        (NewInt32LE size) <- Binary.get
        elements <- Monad.replicateM (fromIntegral size) Binary.get
        elements |> NewList |> return

    put (NewList list) = do
        list |> length |> fromIntegral |> NewInt32LE |> Binary.put
        list |> mapM_ Binary.put