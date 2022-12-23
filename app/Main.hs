module Main (main) where

import Chess

main :: IO ()
main = do
    print startGameState
    let nextStates = getNextStates [] startGameState
    mapM_ print nextStates
