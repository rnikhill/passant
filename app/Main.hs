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

testStr :: BoardRep
testStr =
  [ ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "bP", "  ", "  ", "  "],
    ["  ", "  ", "  ", "wR", "wP", "  ", "  ", "  "],
    ["  ", "  ", "  ", "bP", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "]
  ]

main :: IO ()
main = do
  let gameState = if False then startGameState else mkGameState testStr
  (putStr . printBoard) gameState._board
  putStrLn ""
  let nextStates = getNextStates [] gameState
  print (length nextStates)
  mapM_
    ( \x -> do
        (putStr . printBoard) x._board
        putStr "\n=============================================\n"
    )
    nextStates
