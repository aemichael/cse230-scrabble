-------------------------------------------------------------------------------
-- This module defines the main entrypoint into the application.
-------------------------------------------------------------------------------
module Main where

import           Brick
import           Brick.BChan (newBChan, writeBChan)

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad (forever)
import           Controller
import qualified Graphics.Vty as V

import           Graphics.Vty.Attributes
import           Model
import           View

-- Starts the application from the initial Scrabble State
-- Prints the results once the game terminates
main :: IO ()
main = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  -- This line is the entrypoint for the application
  res <- customMain initialVty buildVty (Just chan) app (Model.initScrabble)
  -- Print the results once the game terminates
  print (scrabbleResult res) 

-- Constant that defines the application
app :: App Scrabble Tick String
app = App
  { 
    -- Converts the current application state to the widgets to display
    appDraw         = View.drawUI 
    -- Not Important
    , appChooseCursor = const . const Nothing
    -- The function to call once an event occurs and needs to be handled
    , appHandleEvent  = Controller.control 
    -- The function to run when the application is started
    , appStartEvent   = Controller.startup
    -- Attribute map
    , appAttrMap      = const (attrMap defAttr [])
  }
