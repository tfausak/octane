{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Utility.Optimizer
  ( optimizeFrames
  ) where

import Data.Function ((&))

import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as StrictText
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Value as Value

-- | Optimizes frames by removing unnecessary replications.
optimizeFrames :: [Frame.Frame] -> [Frame.Frame]
optimizeFrames frames =
  frames &
  Foldable.foldl'
    (\(state, fs) f ->
        let newState = updateState f state
            minimalFrame = getDelta state f
        in (newState, minimalFrame : fs))
    (initialState, []) &
  snd &
  reverse

-- { actor id => (alive?, { property name => property value } ) }
type State = IntMap.IntMap (Bool, Map.Map StrictText.Text Value.Value)

initialState :: State
initialState = IntMap.empty

updateState :: Frame.Frame -> State -> State
updateState frame state1 =
  let spawned =
        frame & #replications &
        filter (\replication -> replication & #state & State.isOpening) &
        map #actorId &
        map CompressedWord.fromCompressedWord
      state2 =
        spawned &
        foldr
          (IntMap.alter
             (\maybeValue ->
                 Just
                   (case maybeValue of
                      Nothing -> (True, Map.empty)
                      Just (_, properties) -> (True, properties))))
          state1
      destroyed =
        frame & #replications &
        filter (\replication -> replication & #state & State.isClosing) &
        map #actorId &
        map CompressedWord.fromCompressedWord
      state3 =
        destroyed &
        foldr
          (IntMap.alter
             (\maybeValue ->
                 Just
                   (case maybeValue of
                      Nothing -> (False, Map.empty)
                      Just (_, properties) -> (False, properties))))
          state2
      updated =
        frame & #replications &
        filter (\replication -> replication & #state & State.isExisting)
      state4 =
        updated &
        foldr
          (\replication ->
              IntMap.alter
                (\maybeValue ->
                    Just
                      (case maybeValue of
                         Nothing -> (True, #properties replication)
                         Just (alive, properties) ->
                           ( alive
                           , Map.union (#properties replication) properties)))
                (replication & #actorId & CompressedWord.fromCompressedWord))
          state3
  in state4

getDelta :: State -> Frame.Frame -> Frame.Frame
getDelta state frame =
  let newReplications =
        frame & #replications &
        reject
          (\replication ->
              let isOpening = replication & #state & State.isOpening
                  actorId = #actorId replication
                  currentState =
                    IntMap.lookup
                      (CompressedWord.fromCompressedWord actorId)
                      state
                  isAlive = fmap fst currentState
                  wasAlreadyAlive = isAlive == Just True
              in isOpening && wasAlreadyAlive) &
        map
          (\replication ->
              if replication & #state & State.isExisting
                then let actorId = #actorId replication
                         currentState =
                           IntMap.findWithDefault
                             (True, Map.empty)
                             (CompressedWord.fromCompressedWord actorId)
                             state
                         currentProperties = snd currentState
                         newProperties = #properties replication
                         changes =
                           newProperties &
                           Map.filterWithKey
                             (\name newValue ->
                                 let oldValue =
                                       Map.lookup name currentProperties
                                 in Just newValue /= oldValue)
                     in replication
                        { Replication.replicationProperties = changes
                        }
                else replication)
  in frame
     { Frame.frameReplications = newReplications
     }

reject :: (a -> Bool) -> [a] -> [a]
reject p xs = filter (\x -> not (p x)) xs
