{-# LANGUAGE OverloadedStrings #-}
module Main where

import AbsState
import Analysis
import CFG
import Ebpf.AsmParser
import Ebpf.Asm
import InformationFlow
import Data.Array ((//))
import System.Environment
import Text.Read (readMaybe)
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

    ["--if", inputFile, secretRegisterText] ->
      case readMaybe secretRegisterText of
        Nothing ->
          putStrLn "Secret register must be an integer from 0 to 10"
        Just secretRegister
          | secretRegister < 0 || secretRegister > 10 ->
              putStrLn "Secret register must be an integer from 0 to 10"
          | otherwise -> do
              result <- parseFromFile inputFile

              case result of
                Left err ->
                  print err

                Right program -> do
                  let initial = lowSecState
                        { regLevels =
                            regLevels lowSecState // [(secretRegister, High)]
                        }
                      resultStates = analyzeIF initial program (cfg program)

                  mapM_ printIFState (Map.toAscList resultStates)
                  mapM_ (printExitSecurity resultStates)
                    [ label
                    | (label, instruction) <- zip [0..] program
                    , instruction == Exit
                    ]

    _ ->
      do
        putStrLn "Usage: ebpf-cfg PROGRAM.asm"
        putStrLn "   or: ebpf-cfg --if PROGRAM.asm SECRET_REGISTER"

printState :: (Int, AbsState) -> IO ()
printState (label, state) = do
  putStrLn ("Label " ++ show label)
  mapM_ printRegister [0..10]
  where
    printRegister register =
      putStrLn $
        "  r" ++ show register ++ " = " ++ show (readReg register state)

printIFState :: (Int, IFState) -> IO ()
printIFState (label, state) = do
  putStrLn ("Label " ++ show label)
  putStrLn ("  pc = " ++ show (pcLevel security))
  mapM_ printRegister [0..10]
  let highMemory =
        [ address
        | address <- [0..511]
        , readMemLevel address security == High
        ]
  putStrLn ("  high memory = " ++ show highMemory)
  where
    security = secState state
    printRegister register =
      putStrLn $
        "  r" ++ show register ++ " = " ++
        show (readRegLevel register security)

printExitSecurity :: Map.Map Int IFState -> Int -> IO ()
printExitSecurity states label =
  case Map.lookup label states of
    Nothing ->
      putStrLn ("Exit at label " ++ show label ++ ": unreachable")
    Just state ->
      putStrLn $
        "Exit at label " ++ show label ++ ": " ++
        if exitSecure state then "secure" else "SECURITY VIOLATION"
