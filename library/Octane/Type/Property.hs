{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Property (Property(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Boolean as Boolean
import qualified Octane.Type.Primitive.Dictionary as Dictionary
import qualified Octane.Type.Primitive.Float32LE as Float32LE
import qualified Octane.Type.Primitive.List as List
import qualified Octane.Type.Primitive.PCString as PCString
import qualified Octane.Type.Primitive.Word32LE as Word32LE
import qualified Octane.Type.Primitive.Word64LE as Word64LE

data Property
    = ArrayProperty Word64LE.Word64LE
                    (List.List (Dictionary.Dictionary Property))
    | BoolProperty Word64LE.Word64LE
                   Boolean.Boolean
    | ByteProperty Word64LE.Word64LE
                   (PCString.PCString, PCString.PCString)
    | FloatProperty Word64LE.Word64LE
                    Float32LE.Float32LE
    | IntProperty Word64LE.Word64LE
                  Word32LE.Word32LE
    | NameProperty Word64LE.Word64LE
                   PCString.PCString
    | QWordProperty Word64LE.Word64LE
                    Word64LE.Word64LE
    | StrProperty Word64LE.Word64LE
                  PCString.PCString
    deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Property where
    get = do
        PCString.PCString kind <- Binary.get
        case kind of
            "ArrayProperty" -> do
                size <- Binary.get
                value <- Binary.get
                value & ArrayProperty size & return
            "BoolProperty" -> do
                size <- Binary.get
                value <- Binary.get
                value & BoolProperty size & return
            "ByteProperty" -> do
                size <- Binary.get
                key <- Binary.get
                value <- Binary.get
                (key, value) & ByteProperty size & return
            "FloatProperty" -> do
                size <- Binary.get
                value <- 
                    case Newtype.unpack size of
                        4 -> Binary.get
                        x -> fail ("unknown FloatProperty size " ++ show x)
                value & FloatProperty size & return
            "IntProperty" -> do
                size <- Binary.get
                value <- 
                    case Newtype.unpack size of
                        4 -> Binary.get
                        x -> fail ("unknown IntProperty size " ++ show x)
                value & IntProperty size & return
            "NameProperty" -> do
                size <- Binary.get
                value <- Binary.get
                value & NameProperty size & return
            "QWordProperty" -> do
                size <- Binary.get
                value <- 
                    case Newtype.unpack size of
                        8 -> Binary.get
                        x -> fail ("unknown QWordProperty size " ++ show x)
                value & QWordProperty size & return
            "StrProperty" -> do
                size <- Binary.get
                value <- Binary.get
                value & StrProperty size & return
            x -> fail ("unknown property type " ++ show x)
    put property = 
        case property of
            ArrayProperty size value -> do
                "ArrayProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value
            BoolProperty size value -> do
                "BoolProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value
            ByteProperty size (key,value) -> do
                "ByteProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put key
                Binary.put value
            FloatProperty size value -> do
                "FloatProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value
            IntProperty size value -> do
                "IntProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value
            NameProperty size value -> do
                "NameProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value
            QWordProperty size value -> do
                "QWordProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value
            StrProperty size value -> do
                "StrProperty" & PCString.PCString & Binary.put
                Binary.put size
                Binary.put value

instance DeepSeq.NFData Property

instance Aeson.ToJSON Property where
    toJSON property = 
        case property of
            ArrayProperty _ x -> Aeson.toJSON x
            BoolProperty _ x -> Aeson.toJSON x
            ByteProperty _ (_,x) -> Aeson.toJSON x
            FloatProperty _ x -> Aeson.toJSON x
            IntProperty _ x -> Aeson.toJSON x
            NameProperty _ x -> Aeson.toJSON x
            QWordProperty _ x -> Aeson.toJSON x
            StrProperty _ x -> Aeson.toJSON x
