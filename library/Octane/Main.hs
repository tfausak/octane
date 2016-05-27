module Octane.Main (main) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Octane.Parser as Parser
import qualified Octane.Type as Type
import qualified System.Environment as Environment
import qualified Text.Printf as Printf

main :: IO ()
main = do
    files <- Environment.getArgs
    contents <- mapM BS.readFile files
    results <- mapM Binary.decodeFileOrFail files
    mapM_ debug (zip3 files contents results)

debug :: (String, BS.ByteString, Either (Binary.ByteOffset, String) Type.Replay)
      -> IO ()
debug (file,contents,result) =
    case result of
        Left (offset,message) -> error
            ( file
            ++ " @ byte "
            ++ show offset
            ++ ": "
            ++ message
            )
        Right replay -> do
            DeepSeq.deepseq replay (return ())
            let inputSize = contents & BS.length & fromIntegral
            let outputSize = replay & Binary.encode & BSL.length
            Monad.when (inputSize /= outputSize) (error
                ( "input size "
                ++ show inputSize
                ++ " not equal to output size "
                ++ show outputSize
                ))

            let toInt x = x & fromIntegral & (\ y -> y :: Int)
            let stripBlanks x = x & lines & filter (\ y -> y & null & not) & unlines

            let classCache = replay
                    & Type.replayCacheItems
                    & Newtype.unpack
                    & map (\ cacheItem ->
                        ( cacheItem & Type.cacheItemClassId & Newtype.unpack & toInt
                        , cacheItem & Type.cacheItemCacheId & Newtype.unpack & toInt
                        , cacheItem & Type.cacheItemParentCacheId & Newtype.unpack & toInt
                        ))
            let classMap = classCache
                    & reverse
                    & List.tails
                    & Maybe.mapMaybe (\ xs ->
                        case xs of
                            [] -> Nothing
                            (classId, _, parentCacheId) : ys ->
                                case dropWhile (\ (_, cacheId, _) -> cacheId /= parentCacheId) ys of
                                    [] -> Nothing
                                    (parentClassId, _, _) : _ -> Just (classId, parentClassId))
                    & IntMap.fromList
            let propertyMap = replay
                    & Type.replayObjects
                    & Newtype.unpack
                    & map Newtype.unpack
                    & map Text.unpack
                    & zip [0 ..]
                    & IntMap.fromDistinctAscList
            let basicClassPropertyMap = replay
                    & Type.replayCacheItems
                    & Newtype.unpack
                    & map (\ cacheItem -> let
                        classId = cacheItem & Type.cacheItemClassId & Newtype.unpack & toInt
                        properties = cacheItem
                            & Type.cacheItemCacheProperties
                            & Newtype.unpack
                            & map (\ cacheProperty -> let
                                streamId = cacheProperty & Type.cachePropertyStreamId & Newtype.unpack & toInt
                                propertyId = cacheProperty & Type.cachePropertyObjectId & Newtype.unpack & toInt
                                name = propertyMap IntMap.! propertyId
                                in (streamId, name))
                            & IntMap.fromList
                        in (classId, properties))
                    & IntMap.fromList
            let classPropertyMap = basicClassPropertyMap -- TODO

            putStrLn "OBJECT STREAM ID => OBJECT NAME"
            propertyMap
                & IntMap.toAscList
                & map (\ (streamId, objectName) ->
                    Printf.printf " %-3d => %s" streamId objectName)
                & unlines
                & putStrLn

            putStrLn "CLASS STREAM ID => CLASS NAME"
            replay
                & Type.replayActors
                & Newtype.unpack
                & map (\ actor ->
                    ( actor & Type.actorStreamId & Newtype.unpack & toInt
                    , actor & Type.actorName & Newtype.unpack & Text.unpack
                    ))
                & map (\ (streamId, name) ->
                    Printf.printf " %-3d => %s" streamId name)
                & unlines
                & putStrLn

            putStrLn "CLASS ID => (CACHE ID, PARENT CACHE ID)"
            classCache
                & map (\ (classId, cacheId, parentCacheId) ->
                    Printf.printf " %-3d => (%-2d, %-2d)" classId cacheId parentCacheId)
                & unlines
                & putStrLn

            putStrLn "CLASS ID => PARENT CLASS ID"
            classMap
                & IntMap.toAscList
                & map (\ (classId, parentId) ->
                    Printf.printf " %d => %d" classId parentId)
                & unlines
                & putStrLn

            putStrLn "CLASS ID => { PROPERTY STREAM ID => PROPERTY NAME }"
            classPropertyMap
                & IntMap.toAscList
                & map (\ (classId, properties) ->
                    ( classId
                    , properties
                        & IntMap.toAscList
                        & map (\ (streamId, name) ->
                            Printf.printf "  %-2d => %s" streamId name)
                        & unlines
                    ))
                & map (\ (classId, properties) ->
                    Printf.printf " %-3d =>\n%s" classId properties)
                & unlines
                & stripBlanks
                & putStrLn

            -- putStrLn "OBJECT ID => CLASS ID"

            let frames = Parser.parseFrames replay
            DeepSeq.deepseq frames (return ())
            let expectedFrames = replay
                    & Type.replayProperties
                    & Newtype.unpack
                    & Map.lookup ("NumFrames" & Text.pack & Newtype.pack)
            let actualFrames = frames
                    & length
                    & fromIntegral
                    & Newtype.pack
                    & Type.IntProperty (Newtype.pack 4)
                    & Just
            Monad.when (expectedFrames /= actualFrames) (error
                ( "expected "
                ++ show expectedFrames
                ++ " frames but found "
                ++ show actualFrames
                ++ " frames"
                ))

            putStr "{\"meta\":"
            replay & Aeson.encode & BSL8.putStrLn
            putStr ",\"frames\":"
            frames & Aeson.encode & BSL8.putStrLn
            putStrLn "}"
