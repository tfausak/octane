{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.Property where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Flow ((|>))
import Octane.Parser.Types.Boolean
import Octane.Parser.Types.Float32LE
import Octane.Parser.Types.Int32LE
import Octane.Parser.Types.Int64LE
import Octane.Parser.Types.List
import Octane.Parser.Types.PCString
import Octane.Parser.Types.Table

data Property
    = ArrayProperty Int64LE (List (Table Property))
    | BoolProperty Int64LE Boolean
    | ByteProperty Int64LE (PCString, PCString)
    | FloatProperty Int64LE Float32LE
    | IntProperty Int64LE Int32LE
    | NameProperty Int64LE PCString
    | QWordProperty Int64LE Int64LE
    | StrProperty Int64LE PCString
    deriving (Show)

instance Aeson.ToJSON Property where
    toJSON property = case property of
        ArrayProperty _ value -> Aeson.toJSON value
        BoolProperty _ value -> Aeson.toJSON value
        ByteProperty _ value -> Aeson.toJSON value
        FloatProperty _ value -> Aeson.toJSON value
        IntProperty _ value -> Aeson.toJSON value
        NameProperty _ value -> Aeson.toJSON value
        QWordProperty _ value -> Aeson.toJSON value
        StrProperty _ value -> Aeson.toJSON value

instance Binary.Binary Property where
    get = do
        kind <- Binary.get
        size <- Binary.get
        case kind :: PCString of
            NewPCString "ArrayProperty" -> do
                value <- Binary.get
                return (ArrayProperty size value)
            NewPCString "BoolProperty" -> do
                value <- Binary.get
                return (BoolProperty size value)
            NewPCString "ByteProperty" -> do
                key <- Binary.get
                value <- Binary.get
                return (ByteProperty size (key, value))
            NewPCString "FloatProperty" -> do
                value <- case size of
                    NewInt64LE 4 -> Binary.get
                    _ -> fail ("unknown FloatProperty size " ++ show size)
                return (FloatProperty size value)
            NewPCString "IntProperty" -> do
                value <- case size of
                    NewInt64LE 4 -> Binary.get
                    _ -> fail ("unknown IntProperty size " ++ show size)
                return (IntProperty size value)
            NewPCString "NameProperty" -> do
                value <- Binary.get
                return (NameProperty size value)
            NewPCString "StrProperty" -> do
                value <- Binary.get
                return (StrProperty size value)
            NewPCString "QWordProperty" -> do
                -- TODO: This isn't correct. The `size` contains the number of
                --   bytes to read. It just usually happens to be 8.
                value <- Binary.get
                return (QWordProperty size value)
            _ -> fail ("unknown property type " ++ show kind)

    put property = case property of
        ArrayProperty size value -> do
            "ArrayProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put

        BoolProperty size value -> do
            "BoolProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put

        ByteProperty size (key, value) -> do
            "ByteProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            key |> Binary.put
            value |> Binary.put

        FloatProperty size value -> do
            "FloatProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put

        IntProperty size value -> do
            "IntProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put

        NameProperty size value -> do
            "NameProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put

        QWordProperty size value -> do
            "QWordProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put

        StrProperty size value -> do
            "StrProperty" |> NewPCString |> Binary.put
            size |> Binary.put
            value |> Binary.put
