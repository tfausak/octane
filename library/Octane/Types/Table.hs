{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Table where

import Octane.Types.PCString (PCString)

import qualified Data.Binary as B
import qualified Data.Map as M

newtype Table a = NewTable
    { unTable :: M.Map PCString a
    } deriving (Show)

instance (B.Binary a) => B.Binary (Table a) where
    get = do
        key <- B.get
        if key == "None"
        then return (NewTable M.empty)
        else do
            value <- B.get
            let row = M.singleton key value
            table <- B.get
            return (NewTable (M.union row table))

    put (NewTable table) = do
        mapM_ (\ (key, value) -> B.put key >> B.put value) (M.assocs table)
        B.put ("None" :: PCString)
