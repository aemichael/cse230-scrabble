-------------------------------------------------------------------------------
-- This module defines the control which is used to handle events once they occur.
-------------------------------------------------------------------------------
module Controller where

import           Brick hiding (Result)
import qualified Brick.Types as T
import           Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Graphics.Vty as V
import           Model
import           Model.Bag
import           Model.Board
import           Model.PlayedRack
import           Model.Player
import           Model.Rack
import           Model.Score
import           Model.Tile

-- Startup function that runs prior to any events in the game.
startup :: Int -> Scrabble -> EventM String Scrabble
startup playerCount s = do
  let player = scrabblePlayer s
  let rack = plRack player
  let bag = scrabbleBag s
  (bag', rack') <- liftIO (fillRack rack bag)
  let player' = player { plRack = rack' }
  return (s { scrabblePlayer = player', scrabbleBag = bag'});

-- Main control function that is used to handle events once they occur
control :: Scrabble -> BrickEvent n Tick -> EventM n (Next Scrabble)
control s ev = case ev of 
  T.VtyEvent (V.EvKey (V.KChar 'a') _) -> nextS s =<< liftIO (playLetter (Letter 'A') s)
  T.VtyEvent (V.EvKey (V.KChar 'b') _) -> nextS s =<< liftIO (playLetter (Letter 'B') s)
  T.VtyEvent (V.EvKey (V.KChar 'c') _) -> nextS s =<< liftIO (playLetter (Letter 'C') s)
  T.VtyEvent (V.EvKey (V.KChar 'd') _) -> nextS s =<< liftIO (playLetter (Letter 'D') s)
  T.VtyEvent (V.EvKey (V.KChar 'e') _) -> nextS s =<< liftIO (playLetter (Letter 'E') s)
  T.VtyEvent (V.EvKey (V.KChar 'f') _) -> nextS s =<< liftIO (playLetter (Letter 'F') s)
  T.VtyEvent (V.EvKey (V.KChar 'g') _) -> nextS s =<< liftIO (playLetter (Letter 'G') s)
  T.VtyEvent (V.EvKey (V.KChar 'h') _) -> nextS s =<< liftIO (playLetter (Letter 'H') s)
  T.VtyEvent (V.EvKey (V.KChar 'i') _) -> nextS s =<< liftIO (playLetter (Letter 'I') s)
  T.VtyEvent (V.EvKey (V.KChar 'j') _) -> nextS s =<< liftIO (playLetter (Letter 'J') s)
  T.VtyEvent (V.EvKey (V.KChar 'k') _) -> nextS s =<< liftIO (playLetter (Letter 'K') s)
  T.VtyEvent (V.EvKey (V.KChar 'l') _) -> nextS s =<< liftIO (playLetter (Letter 'L') s)
  T.VtyEvent (V.EvKey (V.KChar 'm') _) -> nextS s =<< liftIO (playLetter (Letter 'M') s)
  T.VtyEvent (V.EvKey (V.KChar 'n') _) -> nextS s =<< liftIO (playLetter (Letter 'N') s)
  T.VtyEvent (V.EvKey (V.KChar 'o') _) -> nextS s =<< liftIO (playLetter (Letter 'O') s)
  T.VtyEvent (V.EvKey (V.KChar 'p') _) -> nextS s =<< liftIO (playLetter (Letter 'P') s)
  T.VtyEvent (V.EvKey (V.KChar 'q') _) -> nextS s =<< liftIO (playLetter (Letter 'Q') s)
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> nextS s =<< liftIO (playLetter (Letter 'R') s)
  T.VtyEvent (V.EvKey (V.KChar 's') _) -> nextS s =<< liftIO (playLetter (Letter 'S') s)
  T.VtyEvent (V.EvKey (V.KChar 't') _) -> nextS s =<< liftIO (playLetter (Letter 'T') s)
  T.VtyEvent (V.EvKey (V.KChar 'u') _) -> nextS s =<< liftIO (playLetter (Letter 'U') s)
  T.VtyEvent (V.EvKey (V.KChar 'v') _) -> nextS s =<< liftIO (playLetter (Letter 'V') s)
  T.VtyEvent (V.EvKey (V.KChar 'w') _) -> nextS s =<< liftIO (playLetter (Letter 'W') s)
  T.VtyEvent (V.EvKey (V.KChar 'x') _) -> nextS s =<< liftIO (playLetter (Letter 'X') s)
  T.VtyEvent (V.EvKey (V.KChar 'y') _) -> nextS s =<< liftIO (playLetter (Letter 'Y') s)
  T.VtyEvent (V.EvKey (V.KChar 'z') _) -> nextS s =<< liftIO (playLetter (Letter 'Z') s)
  T.VtyEvent (V.EvKey (V.KChar '*') _) -> nextS s =<< liftIO (playLetter (Letter '*') s)
  T.VtyEvent (V.EvKey V.KDel _) -> nextS s =<< liftIO (deleteLetter s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (endTurn s)
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

-- Delete a letter from the board
deleteLetter :: Scrabble -> IO (Result Board, Player, Bag)
deleteLetter s = do
  let bag = (scrabbleBag s)
  let player = (scrabblePlayer s)
  let rack = (plRack player)
  let playedRack = (plPlayedRack player)
  let board = (scrabbleBoard s)
  let Just tile = getTile board (scrabblePos s)
  -- Check if this tile has been played this turn
  if (isTileInPlayedRack (tile, (scrabblePos s)) playedRack)
  then do
    let board' = deleteTile board (scrabblePos s)
    let rack' = insertTileIntoRack tile rack
    let playedRack' = removeTileFromPlayedRack (tile, (scrabblePos s)) playedRack
    let player' = player { plRack = rack', plPlayedRack = playedRack' }
    return (board', player', bag)
  else do
    return (Retry, player, bag)
  
-- Places a letter on the board
playLetter :: Tile -> Scrabble -> IO (Result Board, Player, Bag)
playLetter tile s = do
  let bag = (scrabbleBag s)
  let player = (scrabblePlayer s)
  let rack = (plRack player)
  let playedRack = (plPlayedRack player)
  -- Check if this letter is in the player's rack
  if (isTileInRack tile rack)
    -- If yes, then insert it and remove it from the rack and insert into played rack
  then do
    let res = putTile (scrabbleBoard s) tile (scrabblePos s)
    let rack' = removeTileFromRack tile rack
    let playedRack' = insertTileIntoPlayedRack (tile, (scrabblePos s)) playedRack
    let player' = player { plRack = rack', plPlayedRack = playedRack' }
    return (res, player', bag)
  -- If no, then retry
  else do
    return (Retry, player, bag)

endTurn :: Scrabble -> IO (Result Board, Player, Bag)
endTurn s = do
  let board = (scrabbleBoard s)
  let player = (scrabblePlayer s)
  let rack = (plRack player)
  let playedRack = (plPlayedRack player)
  let currScore = (plScore player)
  let bag = (scrabbleBag s)
  -- Update the player's score
  let newScore = updateScore (extractTiles playedRack) currScore
  -- Refill Rack
  (bag', rack') <- fillRack rack bag
  -- Clear playedRack
  let player' = player { plRack = rack', plPlayedRack = initPlayedRack, plScore = newScore }
  -- If the bag is empty, end the game
  if (isBagEmpty bag)
  then do
    return (End board, player', bag')
  else do
    return (Cont board, player', bag')

-- Updates the result of the Scrabble
nextS :: Scrabble -> (Result Board, Player, Bag) -> EventM n (Next Scrabble)
nextS s (board, player, bag) = do
  case next s board player bag of
    Right s' -> continue s'
    Left s' -> halt s'
