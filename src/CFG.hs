{-# LANGUAGE OverloadedStrings #-}
module CFG
  ( Trans(..)
  , Label
  , CFG
  , cfg
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Ebpf.Asm

data Trans =
    NonCF Instruction -- no jumps, or exit
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type CFG = Set (Label, Trans, Label)

neg :: Jcmp -> Jcmp
neg cmp =
  case cmp of
    Jeq -> Jne ; Jne -> Jeq
    Jgt -> Jle; Jge -> Jlt; Jlt -> Jge; Jle -> Jgt
    Jsgt -> Jsle; Jsge -> Jslt; Jslt -> Jsge; Jsle -> Jsgt
    Jset -> error "Don't know how to negate JSET"

label :: Program -> [(Label, Instruction)]
label = zip [0..]

cfg :: Program -> CFG
cfg program = Set.unions (map transition (label program))
  where
    transition (source, instruction) =
      case instruction of
        JCond Jset register operand offset ->
          Set.fromList
            [ (source, Assert Jset register operand, target)
            , (source, Unconditional, source + 1)
            ]
          where
            target = source + 1 + fromIntegral offset

        JCond comparison register operand offset ->
          Set.fromList
            [ (source, Assert comparison register operand, target)
            , (source, Assert (neg comparison) register operand, source + 1)
            ]
          where
            target = source + 1 + fromIntegral offset

        Jmp offset ->
          Set.singleton
            (source, Unconditional, source + 1 + fromIntegral offset)

        Exit ->
          Set.empty

        _ ->
          Set.singleton (source, NonCF instruction, source + 1)