{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.List (List(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Exts as Exts
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word32 as Word32

-- $setup
-- >>> import Octane.Type.Int8


-- | A list of values.
newtype List a = List
    { listUnpack :: [a]
    } deriving (Eq, Generics.Generic, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''List)

-- | Prefixed with the number of elements in the list.
--
-- >>> Binary.decode "\x01\x00\x00\x00\x02" :: List Int8
-- fromList [2]
--
-- >>> Binary.encode ([2] :: List Int8)
-- "\SOH\NUL\NUL\NUL\STX"
instance (Binary.Binary a) => Binary.Binary (List a) where
    get = do
        size <- Binary.get
        elements <- Monad.replicateM (Word32.fromWord32 size) Binary.get
        elements & List & pure

    put list = do
        list & #unpack & length & fromIntegral & Word32.Word32 & Binary.put
        list & #unpack & mapM_ Binary.put

-- | Allows creating 'List' values with 'Exts.fromList'. Also allows 'List'
-- literals with the @OverloadedLists@ extension.
--
-- >>> [2] :: List Int
-- fromList [2]
instance Exts.IsList (List a) where
    type Item (List a) = a

    fromList items = List items

    toList list = #unpack list

instance (DeepSeq.NFData a) => DeepSeq.NFData (List a) where

-- | >>> show ([2] :: List Int)
-- "fromList [2]"
instance (Show a) => Show (List a) where
    show list = "fromList " ++ show (#unpack list)

-- | Encoded as a JSON array directly.
--
-- >>> Aeson.encode ([2] :: List Int)
-- "[2]"
instance (Aeson.ToJSON a) => Aeson.ToJSON (List a) where
    toJSON list = list
        & #unpack
        & Aeson.toJSON
