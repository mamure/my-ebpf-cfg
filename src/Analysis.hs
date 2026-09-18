module Analysis
  ( analyze
  , nodesOf
  , predsOf
  , succsOf
  ) where

import AbsState
import CFG
import Transfer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

analyze :: AbsState -> CFG -> Map Label AbsState
analyze initial graph =
  loop initialStates [0] Map.empty
  where
    successors = succsOf graph
    wideningLabels = wideningPoints graph

    initialStates =
      Map.insert
        0
        initial
        (Map.fromSet (const botState) (nodesOf graph))

    loop states [] _ =
      states

    loop states (label:queue) counts =
      loop states' queue' counts'
      where
        outgoing =
          Map.findWithDefault [] label successors

        oldState =
          Map.findWithDefault botState label states

        (states', queue', counts') =
          foldl propagate (states, queue, counts) outgoing

        propagate (currentStates, currentQueue, currentCounts)
                  (transition, target) =
          let incoming = step transition oldState
              oldTarget =
                Map.findWithDefault botState target currentStates
              count =
                Map.findWithDefault 0 target currentCounts
              updatedTarget =
                if target `Set.member` wideningLabels && count >= 2
                then widenState oldTarget incoming
                else joinState oldTarget incoming
              nextCounts =
                Map.insert target (count + 1) currentCounts
          in
            if updatedTarget == oldTarget
            then
              (currentStates, currentQueue, nextCounts)
            else
              ( Map.insert target updatedTarget currentStates
              , currentQueue ++ [target]
              , nextCounts
              )

nodesOf :: CFG -> Set Label
nodesOf graph =
  Set.fromList
    [ label
    | (source, _, target) <- Set.toList graph
    , label <- [source, target]
    ]

predsOf :: CFG -> Map Label [(Trans, Label)]
predsOf graph =
  Map.fromListWith (++)
    [ (target, [(transition, source)])
    | (source, transition, target) <- Set.toList graph
    ]

succsOf :: CFG -> Map Label [(Trans, Label)]
succsOf graph =
  Map.fromListWith (++)
    [ (source, [(transition, target)])
    | (source, transition, target) <- Set.toList graph
    ]

wideningPoints :: CFG -> Set Label
wideningPoints graph =
  Set.fromList
    [ target
    | (source, _, target) <- Set.toList graph
    , target <= source
    ]