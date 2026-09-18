module Transfer where

import AbsState
import CFG
import Ebpf.Asm
import Interval
import Data.Int (Int64)

step :: Trans -> AbsState -> AbsState
step (NonCF instruction) state =
  transferInstr instruction state
step Unconditional state =
  state
step (Assert comparison register operand) state =
  refineCmp comparison register operand state

refineCmp :: Jcmp -> Reg -> RegImm -> AbsState -> AbsState
refineCmp Jset _ _ state = state
refineCmp comparison register operand state =
  case operand of
    Imm value ->
      refineOne comparison register (constItv (fromIntegral value)) state

    R other ->
      refineTwo comparison register other state

refineOne :: Jcmp -> Reg -> Itv -> AbsState -> AbsState
refineOne comparison register other state =
  let current = readReg (regNumber register) state
      refined =
        case comparison of
          Jeq -> meetItv current other
          Jge -> meetItv current (lowerOnly other)
          Jgt -> meetItv current (lowerOnly (increaseItv other))
          Jle -> meetItv current (upperOnly other)
          Jlt -> meetItv current (upperOnly (decreaseItv other))
          Jsgt -> meetItv current (lowerOnly (increaseItv other))
          Jsge -> meetItv current (lowerOnly other)
          Jslt -> meetItv current (upperOnly (decreaseItv other))
          Jsle -> meetItv current (upperOnly other)
          _ -> current
  in writeReg (regNumber register) refined state

refineTwo :: Jcmp -> Reg -> Reg -> AbsState -> AbsState
refineTwo comparison left right state =
  let leftValue = readReg (regNumber left) state
      rightValue = readReg (regNumber right) state
      state' = refineOne comparison left rightValue state
  in refineOne (reverseCmp comparison) right leftValue state'

reverseCmp :: Jcmp -> Jcmp
reverseCmp Jeq = Jeq
reverseCmp Jgt = Jlt
reverseCmp Jge = Jle
reverseCmp Jlt = Jgt
reverseCmp Jle = Jge
reverseCmp Jsgt = Jslt
reverseCmp Jsge = Jsle
reverseCmp Jslt = Jsgt
reverseCmp Jsle = Jsge
reverseCmp comparison = comparison

lowerOnly :: Itv -> Itv
lowerOnly (Ival lower _) = Ival lower PosInf
lowerOnly Bot = Bot

upperOnly :: Itv -> Itv
upperOnly (Ival _ upper) = Ival NegInf upper
upperOnly Bot = Bot

increaseItv :: Itv -> Itv
increaseItv (Ival lower upper) =
  Ival (increaseBound lower) (increaseBound upper)
increaseItv Bot = Bot

decreaseItv :: Itv -> Itv
decreaseItv (Ival lower upper) =
  Ival (decreaseBound lower) (decreaseBound upper)
decreaseItv Bot = Bot

increaseBound :: Bound -> Bound
increaseBound (Fin value) = Fin (value + 1)
increaseBound bound = bound

decreaseBound :: Bound -> Bound
decreaseBound (Fin value) = Fin (value - 1)
decreaseBound bound = bound

regNumber :: Reg -> Int
regNumber (Reg number) = number

operandItv :: RegImm -> AbsState -> Itv
operandItv operand state =
  case operand of
    R register ->
      readReg (regNumber register) state
    Imm value ->
      constItv (fromIntegral value)

updateBinary
  :: (Itv -> Itv -> Itv)
  -> Reg
  -> RegImm
  -> AbsState
  -> AbsState
updateBinary operation destination source state =
  writeReg destinationNumber result state
  where
    destinationNumber = regNumber destination
    result =
      operation
        (readReg destinationNumber state)
        (operandItv source state)

addressItv :: Reg -> Maybe Int64 -> AbsState -> Itv
addressItv register offset state =
  addItv
    (readReg (regNumber register) state)
    (constItv (maybe 0 fromIntegral offset))

transferInstr :: Instruction -> AbsState -> AbsState
transferInstr instruction state =
  case instruction of
    LoadImm destination value ->
      writeReg
        (regNumber destination)
        (constItv (fromIntegral value))
        state

    Binary _ Mov destination source ->
      writeReg
        (regNumber destination)
        (operandItv source state)
        state

    Binary _ Add destination source ->
      updateBinary addItv destination source state

    Binary _ Sub destination source ->
      updateBinary subItv destination source state

    Binary _ Mul destination source ->
      updateBinary mulItv destination source state

    Binary _ And destination source ->
      updateBinary bitwiseTop destination source state

    Binary _ Or destination source ->
      updateBinary bitwiseTop destination source state

    Binary _ Xor destination source ->
      updateBinary bitwiseTop destination source state

    Binary _ Lsh destination source ->
      updateBinary shiftTop destination source state

    Binary _ Rsh destination source ->
      updateBinary shiftTop destination source state

    Binary _ Arsh destination source ->
      updateBinary shiftTop destination source state

    Binary _ _ destination _ ->
      writeReg (regNumber destination) topItv state

    Load _ destination source offset ->
      writeReg
        (regNumber destination)
        (readMem (addressItv source offset state) state)
        state

    Store _ destination offset source ->
      writeMem
        (addressItv destination offset state)
        (operandItv source state)
        state

    Unary _ _ destination ->
      writeReg (regNumber destination) topItv state

    LoadMapFd destination _ ->
      writeReg (regNumber destination) topItv state

    _ ->
      state