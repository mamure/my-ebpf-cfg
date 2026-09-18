{-# LANGUAGE OverloadedStrings #-}
module Main where

import AbsState
import Analysis
import CFG
import Ebpf.AsmParser
import System.Environment
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  arguments <- getArgs

  case arguments of
    [inputFile] -> do
      result <- parseFromFile inputFile

      case result of
        Left err ->
          print err

        Right program -> do
          let graph = cfg program
              resultStates = analyze topState graph

          mapM_ printState (Map.toAscList resultStates)

    _ ->
      putStrLn "Usage: ebpf-cfg PROGRAM.asm"

printState :: (Int, AbsState) -> IO ()
printState (label, state) = do
  putStrLn ("Label " ++ show label)
  mapM_ printRegister [0..10]
  where
    printRegister register =
      putStrLn $
        "  r" ++ show register ++ " = " ++ show (readReg register state)
