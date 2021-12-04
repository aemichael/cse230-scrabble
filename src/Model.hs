-------------------------------------------------------------------------------
-- This module defines the Scrabble game.
-- A Scrabble game has a player, board, current cursor, bag, and result.
-- You can initialize a Scrabble game.
-------------------------------------------------------------------------------

module Model where 

import qualified Model.Bag  as Bag
import qualified Model.Board  as Board
import qualified Model.Player as Player
import           Prelude

-------------------------------------------------------------------------------
-- Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

--------------------------------------------------------------------------------
-- Scrabble -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- A Scrabble has a player, board, current cursor, bag, and result.
data Scrabble = MkScrabble
  { scrabblePlayer :: Player.Player    -- ^ current player
  , scrabbleBoard  :: Board.Board      -- ^ current board
  , scrabblePos    :: Board.BoardPos   -- ^ current cursor
  , scrabbleBag    :: Bag.Bag          -- ^ current bag
  , scrabbleResult :: Board.Result ()  -- ^ current result      
  }

-- Initialize the Scrabble game state
initScrabble :: Scrabble
initScrabble = MkScrabble 
  { scrabblePlayer = Player.initPlayer
  , scrabbleBoard  = Board.initBoard
  , scrabblePos    = head Board.boardPositions
  , scrabbleBag    = Bag.initBag
  , scrabbleResult = Board.Cont ()
  }

-- Checks if the coordinate (r,c) the same as the current position of the board
isCurr :: Scrabble -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = scrabblePos s 

-- Determines what the next GameState should be based on the current boardState
next :: Scrabble -> Board.Result Board.Board -> Player.Player -> Bag.Bag -> Either (Board.Result ()) Scrabble
next s Board.Retry     _ _  = Right s
next s (Board.Cont b') p' bag'  = Right (s { scrabbleBoard = b', scrabblePlayer = p', scrabbleBag = bag'})
