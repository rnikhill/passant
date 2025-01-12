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

bishopTestStr :: BoardRep
bishopTestStr =
  [ ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "bP", "  ", "  ", "  "],
    ["  ", "  ", "  ", "wB", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "]
  ]

main :: IO ()
main = do
  let gameState = if True then startGameState else mkGameState bishopTestStr
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
