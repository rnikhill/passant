{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Chess
  ( BoardRep,
    GameState (_board),
    getNextStates,
    mkGameState,
    printBoard,
    startGameState,
  )
import GameTree

{-testStr :: BoardRep
testStr =
  [ ["  ", "bR", "bR", "bR", "bK", "bR", "bR", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["wR", "  ", "  ", "  ", "wK", "  ", "  ", "wR"]
  ]
-}

testStr2 :: BoardRep
testStr2 =
  [ ["  ", "bR", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["wK", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "bR", "  ", "bK", "  ", "  ", "  "]
  ]

main :: IO ()
main = do
  let gameState = if True then startGameState else mkGameState testStr2
  (putStr . printBoard) gameState._board
  putStrLn ""
  let nextStates = getNextStates gameState
  print (length nextStates)
  print $ evalGameState 3 gameState
  mapM_
    ( \x -> do
        (putStr . printBoard) x._board
        putStr "\n=============================================\n"
    )
    nextStates
