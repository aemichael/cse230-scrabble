-------------------------------------------------------------------------------
-- This module defines the control which is used to handle events once they occur.
-------------------------------------------------------------------------------
module Controller where

import           Brick hiding (Result)
import qualified Brick.Types as T
import           Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Data.Map as M
import qualified Graphics.Vty as V
import           Model
import           Model.Bag
import           Model.Board
import           Model.PlayedRack
import           Model.Player
import           Model.Rack
import           Model.Score
import           Model.Tile

createPlayer :: Int -> Bag -> IO (Player, Bag)
createPlayer playerNum bag = do
  -- Create a new rack for this player
  (bag', rack') <- fillRack initRack bag
  -- Create the player
  let player = initPlayer playerNum rack'
  -- Return the newly created player and new bag
  return (player, bag')

createPlayers :: Bag -> [Int] -> IO (PlayerMap, Bag)
createPlayers bag [] = do
  return (M.empty, bag)
createPlayers bag (x:xs) = do
  -- Create the new player and get the new bag
  (player', bag') <- createPlayer x bag
  -- Make the recursive call with the rest of the player indicies and the new bag
  (playerMap, bag'') <- createPlayers bag' xs
  -- Update the map with the player we just created
  let playerMap' = M.insert x player' playerMap
  -- Return the map of players and new bag
  return (playerMap', bag'')

-- Startup function that runs prior to any events in the game.
startup :: Int -> Scrabble -> EventM String Scrabble
startup playerCount s = do
  -- Get the bag for the game
  let bag = scrabbleBag s
  -- Create the map of players
  (playerMap', bag') <- liftIO (createPlayers bag (take playerCount [0..]) )
  -- Return the new game state with the player map and the new bag
  return (s { scrabbleNumPlayers = playerCount, scrabblePlayersMap = playerMap', scrabbleBag = bag'});

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
deleteLetter :: Scrabble -> IO (Result Board, PlayerMap, Int, Bag)
deleteLetter s = do
  let bag = (scrabbleBag s)
  let currPlayerKey = (scrabbleCurrPlayerKey s)
  let playerMap = (scrabblePlayersMap s)
  let player = getPlayer playerMap (scrabbleCurrPlayerKey s)
  let rack = (plRack player)
  let playedRack = (plPlayedRack player)
  let board = (scrabbleBoard s)
  case getTile board (scrabblePos s) of
    Just tile ->   
      -- Check if this tile has been played this turn
      if (isTileInPlayedRack (tile, (scrabblePos s)) playedRack)
      then do
        let board' = deleteTile board (scrabblePos s)
        let rack' = insertTileIntoRack tile rack
        let playedRack' = removeTileFromPlayedRack (tile, (scrabblePos s)) playedRack
        let player' = player { plRack = rack', plPlayedRack = playedRack' }
        let playerMap' = updatePlayer (scrabbleCurrPlayerKey s) player' playerMap
        return (board', playerMap', currPlayerKey, bag)
      else do
        return (Retry, playerMap, currPlayerKey, bag)
    Nothing -> return (Retry, playerMap, currPlayerKey, bag)

  
-- Places a letter on the board
playLetter :: Tile -> Scrabble -> IO (Result Board, PlayerMap, Int, Bag)
playLetter tile s = do
  let bag = (scrabbleBag s)
  let currPlayerKey = (scrabbleCurrPlayerKey s)
  let playerMap = (scrabblePlayersMap s)
  let player = getPlayer playerMap (scrabbleCurrPlayerKey s)
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
    let playerMap' = updatePlayer (scrabbleCurrPlayerKey s) player' playerMap
    return (res, playerMap', currPlayerKey, bag)
  -- If no, then retry
  else do
    return (Retry, playerMap, currPlayerKey, bag)

-- When a player ends their turn, the player's score will be updated, their
-- rack will be refilled, their playedRack will be cleared, the nextPlayer is
-- chosen, and we check if the bag is empty (which terminates the game).
endTurn :: Scrabble -> IO (Result Board, PlayerMap, Int, Bag)
endTurn s = do
  let board = (scrabbleBoard s)
  let playerMap = (scrabblePlayersMap s)
  let player = getPlayer playerMap (scrabbleCurrPlayerKey s) 
  let rack = (plRack player)
  let playedRack = (plPlayedRack player)
  let currScore = (plScore player)
  let bag = (scrabbleBag s)
  -- Update the player's score
  -- NOTE: This is where the new score is actually computed
  -- PlayedRack is a [(Tile, BoardPos)] so the board pos info can be used
  -- to compute which tiles are on bonus board positions and also search fromEnum
  -- a board position the four directions for the tiles it is connected to.
  let newScore = updateScore (extractTiles playedRack) currScore
  -- Refill Rack
  (bag', rack') <- fillRack rack bag
  -- Clear playedRack
  -- NOTE: This is where the new score is actually updated for the player
  let player' = player { plRack = rack', plPlayedRack = initPlayedRack, plScore = newScore }
  let playerMap' = updatePlayer (scrabbleCurrPlayerKey s) player' playerMap
  -- Get the next player's key
  let nextPlayerKey = getNextPlayerKey s
  -- If the bag is empty, end the game
  if (isBagEmpty bag)
  then do
    return (End board, playerMap', nextPlayerKey, bag')
  else do
    return (Cont board, playerMap', nextPlayerKey, bag')

-- Updates the result of the Scrabble
nextS :: Scrabble -> (Result Board, PlayerMap, Int, Bag) -> EventM n (Next Scrabble)
nextS s (board, playerMap, nextPlayerKey, bag) = do
  case next s board playerMap nextPlayerKey bag of
    Right s' -> continue s'
    Left s' -> halt s'
