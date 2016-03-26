{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Property (Property(..)) where

import Octane.Core
import Octane.Type.Primitive.Boolean
import Octane.Type.Primitive.Dictionary
import Octane.Type.Primitive.Float32LE
import Octane.Type.Primitive.List
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Word32LE
import Octane.Type.Primitive.Word64LE

data Property
    = ArrayProperty Word64LE (List (Dictionary Property))
    | BoolProperty Word64LE Boolean
    | ByteProperty Word64LE (PCString, PCString)
    | FloatProperty Word64LE Float32LE
    | IntProperty Word64LE Word32LE
    | NameProperty Word64LE PCString
    | QWordProperty Word64LE Word64LE
    | StrProperty Word64LE PCString
    deriving (Eq, Generic, NFData, Show)

instance Binary Property where
    get = do
        PCString kind <- get
        case kind of
            "ArrayProperty" -> do
                size <- get
                value <- get
                ArrayProperty size value & return
            "BoolProperty" -> do
                size <- get
                value <- get
                BoolProperty size value & return
            "ByteProperty" -> do
                size <- get
                key <- get
                value <- get
                ByteProperty size (key, value) & return
            "FloatProperty" -> do
                size <- get
                value <- case unpack size of
                    4 -> get
                    x -> fail ("unknown FloatProperty size " ++ show x)
                FloatProperty size value & return
            "IntProperty" -> do
                size <- get
                value <- case unpack size of
                    4 -> get
                    x -> fail ("unknown IntProperty size " ++ show x)
                IntProperty size value & return
            "NameProperty" -> do
                size <- get
                value <- get
                NameProperty size value & return
            "QWordProperty" -> do
                size <- get
                value <- case unpack size of
                    8 -> get
                    x -> fail ("unknown QWordProperty size " ++ show x)
                QWordProperty size value & return
            "StrProperty" -> do
                size <- get
                value <- get
                StrProperty size value & return
            x -> fail ("unknown property type " ++ show x)

    put property = case property of
        ArrayProperty size value -> do
            "ArrayProperty" & PCString & put
            size & put
            value & put
        BoolProperty size value -> do
            "BoolProperty" & PCString & put
            size & put
            value & put
        ByteProperty size (key, value) -> do
            "ByteProperty" & PCString & put
            size & put
            key & put
            value & put
        FloatProperty size value -> do
            "FloatProperty" & PCString & put
            size & put
            value & put
        IntProperty size value -> do
            "IntProperty" & PCString & put
            size & put
            value & put
        NameProperty size value -> do
            "NameProperty" & PCString & put
            size & put
            value & put
        QWordProperty size value -> do
            "QWordProperty" & PCString & put
            size & put
            value & put
        StrProperty size value -> do
            "StrProperty" & PCString & put
            size & put
            value & put

instance ToJSON Property where
    toJSON property = case property of
        ArrayProperty _ x -> toJSON x
        BoolProperty _ x -> toJSON x
        ByteProperty _ (_, x) -> toJSON x
        FloatProperty _ x -> toJSON x
        IntProperty _ x -> toJSON x
        NameProperty _ x -> toJSON x
        QWordProperty _ x -> toJSON x
        StrProperty _ x -> toJSON x
