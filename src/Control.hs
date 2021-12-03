-------------------------------------------------------------------------------
-- This module defines the control which is used to handle events once they occur.
-------------------------------------------------------------------------------
module Control where

import           Brick hiding (Result)
import qualified Brick.Types as T
import           Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Graphics.Vty as V
import           Model
import           Model.Board
import           Model.Tile

-- Main control function that is used to handle events once they occur
control :: GameState -> BrickEvent n Tick -> EventM n (Next GameState)
control s ev = case ev of 
  T.VtyEvent (V.EvKey (V.KChar lett) _) -> nextS s =<< liftIO (playLetter (Letter lett) s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s

-- Moves the cursor in a direction
move :: (BoardPos -> BoardPos) -> GameState -> GameState
move f s = s { psPos = f (psPos s) }

-- Updates a board position to be up by 1
up :: BoardPos -> BoardPos 
up p = p { pRow = max 1 (pRow p - 1) } 

-- Updates a board position to be down by 1
down :: BoardPos -> BoardPos
down p = p { pRow = min Model.Board.boardDim (pRow p + 1) } 

-- Updates a board position to be left by 1
left :: BoardPos -> BoardPos 
left p = p { pCol   = max 1 (pCol p - 1) } 

-- Updates a board position to be right by 1
right :: BoardPos -> BoardPos 
right p = p { pCol = min Model.Board.boardDim (pCol p + 1) } 

-- Places a letter on the board
playLetter :: Tile -> GameState -> IO (Result BoardState)
playLetter lett s = put (psBoard s) lett <$> getPos s

-- Gets the current position of the cursor on the board
getPos :: GameState -> IO BoardPos
getPos s = do {return (psPos s)}

-- TODO?
nextS :: GameState -> Result BoardState -> EventM n (Next GameState)
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 
