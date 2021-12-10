-------------------------------------------------------------------------------
-- This module defines the Scrabble entrypoint into the application.
-------------------------------------------------------------------------------
module PlayScrabble (
    playScrabble
)

where
import           Brick
import           Brick.BChan (newBChan, writeBChan)

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad (forever)
import           Controller

import qualified Graphics.Vty as V
import           Graphics.Vty.Attributes
import           Model
import           Model.Player
import           ScrabbleColors
import           View

-- | Runs the main Scrabble game.
playScrabble :: Int -> IO String
playScrabble playerCount = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  -- This line is the entrypoint for the application
  res <- customMain initialVty buildVty (Just chan) (app playerCount) (Model.initScrabble)
  -- Print the results once the game terminates
  let outputString = printPlayerScores (scrabblePlayersMap res)
  return outputString

-- | Constant that defines the application
app :: Int -> App Scrabble Tick String
app playerCount = App
  { 
    -- Converts the current application state to the widgets to display
    appDraw         = View.drawUI 
    -- Not Important
  , appChooseCursor = const . const Nothing
    -- The function to call once an event occurs and needs to be handled
  , appHandleEvent  = Controller.control 
    -- The function to run when the application is started
  , appStartEvent   = Controller.startup playerCount
    -- Attribute map
  , appAttrMap      = const (attrMap appAttr [])
  }
  where
    appAttr = Attr
                { attrStyle     = Default
                , attrForeColor = SetTo black
                , attrBackColor = SetTo defbg
                , attrURL       = Default
                }
