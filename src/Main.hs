module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View 
import Control 

-------------------------------------------------------------------------------
main :: IO ()
main = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <- customMain initialVty buildVty (Just chan) app (Model.init)
  print (psResult res) 

app :: App PlayState Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const (attrMap defAttr [])
  }
