-------------------------------------------------------------------------------
-- This module defines the main entrypoint into the application.
-------------------------------------------------------------------------------
module Main where

import PlayScrabble
-- import SetupScrabble


-- Starts the application from the initial Scrabble State
-- Prints the results once the game terminates
main :: IO ()
main = do
  -- playerCount <- setupScrabble
  -- putStrLn $ "Player Count: " ++ show playerCount
  let playerCount = 1
  score <- playScrabble playerCount
  putStrLn $ "Your final score: " ++ show score
