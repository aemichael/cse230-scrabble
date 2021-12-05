-------------------------------------------------------------------------------
-- This module defines the main entrypoint into the application.
-------------------------------------------------------------------------------
module Main where

import PlayScrabble
import SetupScrabble


-- Starts the application from the initial Scrabble State
-- Prints the results once the game terminates
main :: IO ()
main = do
  playerCount <- setupScrabble
  scores <- playScrabble playerCount
  putStrLn $ scores
