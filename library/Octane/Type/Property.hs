{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Property (Property(..)) where

import Octane.Core
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Boolean
import Octane.Type.Primitive.Float32LE
import Octane.Type.Primitive.Int32LE
import Octane.Type.Primitive.Int64LE
import Octane.Type.Primitive.List
import Octane.Type.Primitive.Dictionary

data Property
    = ArrayProperty Int64LE (List (Dictionary Property))
    | BoolProperty Int64LE Boolean
    | ByteProperty Int64LE (PCString, PCString)
    | FloatProperty Int64LE Float32LE
    | IntProperty Int64LE Int32LE
    | NameProperty Int64LE PCString
    | QWordProperty Int64LE Int64LE
    | StrProperty Int64LE PCString
    deriving (Eq, Generic, NFData, Show)

instance Binary Property where
    get = do
        kind <- get
        case getPCString kind of
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
                value <- case getInt64LE size of
                    4 -> get
                    x -> fail ("unknown FloatProperty size " ++ show x)
                FloatProperty size value & return
            "IntProperty" -> do
                size <- get
                value <- case getInt64LE size of
                    4 -> get
                    x -> fail ("unknown IntProperty size " ++ show x)
                IntProperty size value & return
            "NameProperty" -> do
                size <- get
                value <- get
                NameProperty size value & return
            "QWordProperty" -> do
                size <- get
                value <- case getInt64LE size of
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
