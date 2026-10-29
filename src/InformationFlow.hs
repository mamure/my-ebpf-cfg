module InformationFlow
  ( Level(..)
  , SecState(..)
  , IFState(..)
  , lowSecState
  , highSecState
  , analyzeIF
  , exitSecure
  , readRegLevel
  , readMemLevel
  ) where

import AbsState
import Analysis (nodesOf, succsOf)
import CFG
import Data.Array
import Data.Int (Int64)
import Data.List (foldl', maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Ebpf.Asm
import Interval
import qualified Transfer

data Level = Low | High
  deriving (Eq, Ord, Show)

data SecState = SecState
  { regLevels :: Array Int Level
  , memLevels :: Array Int Level
  , pcLevel :: Level
  }
  deriving (Eq, Show)

data IFState = IFState
  { valueState :: AbsState
  , secState :: SecState
  }
  deriving (Eq, Show)

lowSecState :: SecState
lowSecState = SecState
  { regLevels = array (0, 10) [(r, Low) | r <- [0..10]]
  , memLevels = array (0, 511) [(a, Low) | a <- [0..511]]
  , pcLevel = Low
  }

highSecState :: SecState
highSecState = SecState
  { regLevels = array (0, 10) [(r, High) | r <- [0..10]]
  , memLevels = array (0, 511) [(a, High) | a <- [0..511]]
  , pcLevel = High
  }

readRegLevel :: Int -> SecState -> Level
readRegLevel register state = regLevels state ! register

readMemLevel :: Int -> SecState -> Level
readMemLevel address state = memLevels state ! address

joinLevel :: Level -> Level -> Level
joinLevel Low Low = Low
joinLevel _ _ = High

joinSec :: SecState -> SecState -> SecState
joinSec left right = SecState
  { regLevels = array (0, 10)
      [ (r, joinLevel (readRegLevel r left) (readRegLevel r right))
      | r <- [0..10]
      ]
  , memLevels = array (0, 511)
      [ (a, joinLevel (readMemLevel a left) (readMemLevel a right))
      | a <- [0..511]
      ]
  , pcLevel = joinLevel (pcLevel left) (pcLevel right)
  }

joinIF :: IFState -> IFState -> IFState
joinIF left right = IFState
  { valueState = joinState (valueState left) (valueState right)
  , secState = joinSec (secState left) (secState right)
  }

widenSec :: SecState -> SecState -> SecState
widenSec = joinSec

widenIF :: IFState -> IFState -> IFState
widenIF left right = IFState
  { valueState = widenState (valueState left) (valueState right)
  , secState = widenSec (secState left) (secState right)
  }

bottomIF :: IFState
bottomIF = IFState botState lowSecState

regNumber :: Reg -> Int
regNumber (Reg number) = number

operandLevel :: RegImm -> SecState -> Level
operandLevel (Imm _) _ = Low
operandLevel (R register) state = readRegLevel (regNumber register) state

writeRegLevel :: Int -> Level -> SecState -> SecState
writeRegLevel register level state =
  state { regLevels = regLevels state // [(register, level)] }

addressItv :: Reg -> Maybe Int64 -> AbsState -> Itv
addressItv register offset state =
  addItv
    (readReg (regNumber register) state)
    (constItv (maybe 0 fromIntegral offset))

possibleAddresses :: Itv -> [Int]
possibleAddresses (Ival (Fin lower) (Fin upper)) =
  [ max 0 (fromInteger lower) .. min 511 (fromInteger upper) ]
possibleAddresses _ = [0..511]

addressLevel :: Reg -> Maybe Int64 -> IFState -> Level
addressLevel register offset state =
  let value = valueState state
      addresses = possibleAddresses (addressItv register offset value)
      memoryLevel = foldl' joinLevel Low
        [ readMemLevel address (secState state) | address <- addresses ]
  in joinLevel
       (readRegLevel (regNumber register) (secState state))
       memoryLevel

writeMemoryLevels :: [Int] -> Level -> SecState -> SecState
writeMemoryLevels addresses level state =
  state
    { memLevels = memLevels state //
        [ (address, joinLevel (readMemLevel address state) level)
        | address <- addresses
        ]
    }

transferSecurity :: Level -> Instruction -> IFState -> IFState
transferSecurity context instruction state =
  let old = (secState state) { pcLevel = context }
      write destination level =
        state { secState = writeRegLevel (regNumber destination)
                  (joinLevel context level) old }
      destinationLevel destination =
        readRegLevel (regNumber destination) old
      binaryLevel destination source =
        joinLevel (destinationLevel destination) (operandLevel source old)
  in case instruction of
    LoadImm destination _ -> write destination Low
    Binary _ Mov destination source -> write destination (operandLevel source old)
    Binary _ _ destination source -> write destination (binaryLevel destination source)
    Unary _ _ destination -> write destination (destinationLevel destination)
    Load _ destination source offset ->
      write destination (addressLevel source offset state)
    Store _ destination offset source ->
      let addresses = possibleAddresses (addressItv destination offset (valueState state))
          level = joinLevel
            (addressLevel destination offset state)
            (operandLevel source old)
      in state { secState = writeMemoryLevels addresses
                    (joinLevel context level) old }
    LoadMapFd destination _ -> write destination High
    _ -> state { secState = old { pcLevel = context } }

stepIF :: Level -> Trans -> IFState -> IFState
stepIF context transition state =
  let values = Transfer.step transition (valueState state)
      state' = state { valueState = values }
  in case transition of
    NonCF instruction -> transferSecurity context instruction state'
    _ -> state' { secState = (secState state') { pcLevel = context } }

branchSources :: CFG -> Set Label
branchSources graph =
  Set.fromList
    [ source
    | (source, transition, _) <- Set.toList graph
    , isAssert transition
    ]
  where
    isAssert (Assert _ _ _) = True
    isAssert _ = False

syntheticExit :: Program -> Label
syntheticExit program = length program

successorsWithExit :: Program -> CFG -> Map Label [Label]
successorsWithExit program graph =
  let sink = syntheticExit program + 1
      raw = succsOf graph
      normal = Map.fromList
        [ (source, map (replaceTarget sink . snd) targets)
        | (source, targets) <- Map.toList raw
        ]
      exits =
        [ (source, [sink])
        | (source, instruction) <- zip [0..] program
        , instruction == Exit
        ]
      withExits = Map.unionWith (++) normal (Map.fromList exits)
      terminal = Map.singleton sink []
  in Map.union terminal withExits
  where
    replaceTarget sink target
      | target >= length program = sink
      | otherwise = target

postDominators :: Program -> CFG -> Map Label (Set Label)
postDominators program graph = iterateToFixpoint initial
  where
    sink = syntheticExit program + 1
    successors = successorsWithExit program graph
    nodes = Set.fromList (sink : [0 .. length program - 1])
    initial = Map.fromSet (const nodes) nodes
    iterateToFixpoint current =
      let next = Map.mapWithKey (update current) current
      in if next == current then current else iterateToFixpoint next
    update current node _
      | node == sink = Set.singleton sink
      | otherwise =
          let following = Map.findWithDefault [sink] node successors
              successorSets =
                [ Map.findWithDefault (Set.singleton sink) successor current
                | successor <- following
                ]
          in Set.insert node (foldl1 Set.intersection successorSets)

immediatePostDominator :: Map Label (Set Label) -> Label -> Maybe Label
immediatePostDominator dominators node =
  case candidates of
    [] -> Nothing
    _ -> Just (maximumBy (comparing size) candidates)
  where
    candidates = Set.toList
      (Set.delete node (Map.findWithDefault Set.empty node dominators))
    size candidate = Set.size
      (Map.findWithDefault Set.empty candidate dominators)

reachableUntil :: Map Label [Label] -> Set Label -> Label -> Set Label
reachableUntil successors stop start = go Set.empty [start]
  where
    go visited [] = visited
    go visited (node:queue)
      | node `Set.member` visited = go visited queue
      | node `Set.member` stop = go visited queue
      | otherwise =
          let next = Map.findWithDefault [] node successors
          in go (Set.insert node visited) (next ++ queue)

controlRegions :: Program -> CFG -> Map Label (Set Label)
controlRegions program graph =
  Map.fromListWith Set.union
    [ (source, region)
    | source <- Set.toList (branchSources graph)
    , Just post <- [immediatePostDominator dominators source]
    , ( _, _, firstTarget) <- branchEdges source
    , ( _, _, secondTarget) <- branchEdges source
    , firstTarget /= secondTarget
    , let region = regionFor post firstTarget secondTarget
    ]
  where
    dominators = postDominators program graph
    successors = successorsWithExit program graph
    regionFor post firstTarget secondTarget = Set.union
      (reachableUntil successors (Set.singleton post) firstTarget)
      (reachableUntil successors (Set.singleton post) secondTarget)
    branchEdges source =
      [ edge
      | edge@(source', _, _) <- Set.toList graph
      , source' == source
      ]

branchGuardLevel :: Trans -> SecState -> Level
branchGuardLevel (Assert _ register operand) state =
  joinLevel
    (readRegLevel (regNumber register) state)
    (operandLevel operand state)
branchGuardLevel _ _ = Low

-- The observable eBPF result is r0.  Requiring a low pc as well makes
-- termination/control dependence explicit at an exit point.
exitSecure :: IFState -> Bool
exitSecure state =
  readRegLevel 0 (secState state) == Low
  && pcLevel (secState state) == Low

analyzeIF :: SecState -> Program -> CFG -> Map Label IFState
analyzeIF initial program graph = loop initialStates [0] Map.empty
  where
    regions = controlRegions program graph
    successors = succsOf graph
    wideningLabels = Set.fromList
      [ target
      | (source, _, target) <- Set.toList graph
      , target <= source
      ]
    guards = Map.fromListWith (++)
      [ (source, [transition])
      | (source, transition@(Assert _ _ _), _) <- Set.toList graph
      ]
    labels = Set.fromList [0 .. length program - 1]
      `Set.union` nodesOf graph
    initialStates = Map.insert 0
      (IFState topState initial)
      (Map.fromSet (const bottomIF) labels)
    loop states [] _ = states
    loop states (label:queue) counts =
      let oldState = Map.findWithDefault bottomIF label states
          (states', queue', counts') = foldl' (propagate label oldState)
            (states, queue, counts) (Map.findWithDefault [] label successors)
      in loop states' queue' counts'
    propagate source sourceState (states, queue, counts) (transition, target) =
      let context = executionContext transition target states source sourceState
          targetContext = contextAt target states source sourceState
          incoming =
            setPc targetContext (stepIF context transition sourceState)
          oldTarget = Map.findWithDefault bottomIF target states
          count = Map.findWithDefault 0 target counts
          updated
            | target `Set.member` wideningLabels && count >= 2 =
                widenIF oldTarget incoming
            | otherwise =
                joinIF oldTarget incoming
          nextCounts = Map.insert target (count + 1) counts
      in if updated == oldTarget
         then (states, queue, nextCounts)
         else (Map.insert target updated states, queue ++ [target], nextCounts)
    executionContext transition target states source sourceState =
      case transition of
        Assert _ _ _ -> contextAt target states source sourceState
        _ -> pcLevel (secState sourceState)
    setPc level state =
      state { secState = (secState state) { pcLevel = level } }
    contextAt target states source sourceState =
      foldl' joinLevel Low
        [ branchContext branchSource (branchState branchSource)
        | (branchSource, controlled) <- Map.toList regions
        , target `Set.member` controlled
        ]
      where
        branchState branchSource
          | branchSource == source = sourceState
          | otherwise = Map.findWithDefault bottomIF branchSource states
        branchContext branchSource branchState =
          foldl' joinLevel (pcLevel (secState branchState))
            [ branchGuardLevel guard (secState branchState)
            | guard <- Map.findWithDefault [] branchSource guards
            ]
