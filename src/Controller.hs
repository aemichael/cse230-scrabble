-------------------------------------------------------------------------------
-- This module defines the control which is used to handle events once they occur.
-------------------------------------------------------------------------------
module Controller where

import           Brick hiding (Result)
import qualified Brick.Types as T
import           Control.Monad.IO.Class (MonadIO(liftIO))

import           Data.Char (toUpper)
import qualified Graphics.Vty as V
import           Model
import           Model.Board
import           Model.Player
import           Model.Rack
import           Model.Tile

-- Main control function that is used to handle events once they occur
control :: Scrabble -> BrickEvent n Tick -> EventM n (Next Scrabble)
control s ev = case ev of 
  T.VtyEvent (V.EvKey (V.KChar 'a') _) -> nextS s =<< liftIO (playLetter (Letter 'a') s)
  T.VtyEvent (V.EvKey (V.KChar 'b') _) -> nextS s =<< liftIO (playLetter (Letter 'b') s)
  T.VtyEvent (V.EvKey (V.KChar 'c') _) -> nextS s =<< liftIO (playLetter (Letter 'c') s)
  T.VtyEvent (V.EvKey (V.KChar 'd') _) -> nextS s =<< liftIO (playLetter (Letter 'd') s)
  T.VtyEvent (V.EvKey (V.KChar 'e') _) -> nextS s =<< liftIO (playLetter (Letter 'e') s)
  T.VtyEvent (V.EvKey (V.KChar 'f') _) -> nextS s =<< liftIO (playLetter (Letter 'f') s)
  T.VtyEvent (V.EvKey (V.KChar 'g') _) -> nextS s =<< liftIO (playLetter (Letter 'g') s)
  T.VtyEvent (V.EvKey (V.KChar 'h') _) -> nextS s =<< liftIO (playLetter (Letter 'h') s)
  T.VtyEvent (V.EvKey (V.KChar 'i') _) -> nextS s =<< liftIO (playLetter (Letter 'i') s)
  T.VtyEvent (V.EvKey (V.KChar 'j') _) -> nextS s =<< liftIO (playLetter (Letter 'j') s)
  T.VtyEvent (V.EvKey (V.KChar 'k') _) -> nextS s =<< liftIO (playLetter (Letter 'k') s)
  T.VtyEvent (V.EvKey (V.KChar 'l') _) -> nextS s =<< liftIO (playLetter (Letter 'l') s)
  T.VtyEvent (V.EvKey (V.KChar 'm') _) -> nextS s =<< liftIO (playLetter (Letter 'm') s)
  T.VtyEvent (V.EvKey (V.KChar 'n') _) -> nextS s =<< liftIO (playLetter (Letter 'n') s)
  T.VtyEvent (V.EvKey (V.KChar 'o') _) -> nextS s =<< liftIO (playLetter (Letter 'o') s)
  T.VtyEvent (V.EvKey (V.KChar 'p') _) -> nextS s =<< liftIO (playLetter (Letter 'p') s)
  T.VtyEvent (V.EvKey (V.KChar 'q') _) -> nextS s =<< liftIO (playLetter (Letter 'q') s)
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> nextS s =<< liftIO (playLetter (Letter 'r') s)
  T.VtyEvent (V.EvKey (V.KChar 's') _) -> nextS s =<< liftIO (playLetter (Letter 's') s)
  T.VtyEvent (V.EvKey (V.KChar 't') _) -> nextS s =<< liftIO (playLetter (Letter 't') s)
  T.VtyEvent (V.EvKey (V.KChar 'u') _) -> nextS s =<< liftIO (playLetter (Letter 'u') s)
  T.VtyEvent (V.EvKey (V.KChar 'v') _) -> nextS s =<< liftIO (playLetter (Letter 'v') s)
  T.VtyEvent (V.EvKey (V.KChar 'w') _) -> nextS s =<< liftIO (playLetter (Letter 'w') s)
  T.VtyEvent (V.EvKey (V.KChar 'x') _) -> nextS s =<< liftIO (playLetter (Letter 'x') s)
  T.VtyEvent (V.EvKey (V.KChar 'y') _) -> nextS s =<< liftIO (playLetter (Letter 'y') s)
  T.VtyEvent (V.EvKey (V.KChar 'z') _) -> nextS s =<< liftIO (playLetter (Letter 'z') s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s

-- Moves the cursor in a direction
move :: (BoardPos -> BoardPos) -> Scrabble -> Scrabble
move f s = s { scrabblePos = f (scrabblePos s) }

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
playLetter :: Tile -> Scrabble -> IO (Result Board)
playLetter (Letter char) s = do {
  -- Check if this letter is in the player's rack
  if (isTileInRack (Letter (toUpper char)) (plRack (scrabblePlayer s)))
    -- If yes, then insert it and remove it from the rack
  then do {
    res <- putTile (scrabbleBoard s) (Letter (toUpper char)) <$> getPos s;
    
    return (res);
  }
  -- If no, then retry
  else do {
    return (Retry);
  }
}

-- Gets the current position of the cursor on the board
getPos :: Scrabble -> IO BoardPos
getPos s = do {return (scrabblePos s)}

-- Updates the result of the Scrabble
nextS :: Scrabble -> Result Board -> EventM n (Next Scrabble)
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { scrabbleResult = res }) 
