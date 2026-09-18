module AbsState
  ( AbsState(..)
  , botState
  , topState
  , joinState
  , leqState
  , widenState
  , narrowState
  , readReg
  , writeReg
  , readMem
  , writeMem
  ) where

import Data.Array
import Interval

data AbsState = AbsState
  { regs :: Array Int Itv
  , mem  :: Array Int Itv
  }
  deriving (Eq, Show)

botState :: AbsState
botState =
  AbsState
    (array (0, 10) [(i, Bot) | i <- [0..10]])
    (array (0, 511) [(i, Bot) | i <- [0..511]])

topState :: AbsState
topState =
  AbsState
    (array (0, 10) [(i, topItv) | i <- [0..10]])
    (array (0, 511) [(i, topItv) | i <- [0..511]])

readReg :: Int -> AbsState -> Itv
readReg register state = regs state ! register

writeReg :: Int -> Itv -> AbsState -> AbsState
writeReg register value state =
  state { regs = regs state // [(register, value)] }

joinState :: AbsState -> AbsState -> AbsState
joinState left right =
  AbsState
    (array (0, 10)
      [ (i, joinItv (regs left ! i) (regs right ! i))
      | i <- [0..10]
      ])
    (array (0, 511)
      [ (i, joinItv (mem left ! i) (mem right ! i))
      | i <- [0..511]
      ])

leqState :: AbsState -> AbsState -> Bool
leqState left right =
  and
    [ leqItv (regs left ! i) (regs right ! i)
    | i <- [0..10]
    ]
  &&
  and
    [ leqItv (mem left ! i) (mem right ! i)
    | i <- [0..511]
    ]

widenState :: AbsState -> AbsState -> AbsState
widenState left right =
  AbsState
    (array (0, 10)
      [ (i, widenItv (regs left ! i) (regs right ! i))
      | i <- [0..10]
      ])
    (array (0, 511)
      [ (i, widenItv (mem left ! i) (mem right ! i))
      | i <- [0..511]
      ])

narrowState :: AbsState -> AbsState -> AbsState
narrowState left right =
  AbsState
    (array (0, 10)
      [ (i, narrowItv (regs left ! i) (regs right ! i))
      | i <- [0..10]
      ])
    (array (0, 511)
      [ (i, narrowItv (mem left ! i) (mem right ! i))
      | i <- [0..511]
      ])

readMem :: Itv -> AbsState -> Itv
readMem Bot _ = Bot
readMem (Ival _ _) state =
  foldr joinItv Bot
    [ mem state ! i | i <- [0..511] ]

writeMem :: Itv -> Itv -> AbsState -> AbsState
writeMem Bot _ state = state
writeMem _ value state =
  state
    { mem = array (0, 511)
        [ (i, joinItv (mem state ! i) value)
        | i <- [0..511]
        ]
    }