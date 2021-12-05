<<<<<<< HEAD
{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
  } 

init :: Int -> PlayState
init n = PS 
  { psX      = Player.human
  , psO      = Player.rando
  , psScore  = Score.init n
  , psBoard  = Board.init
  , psTurn   = Board.X
  , psPos    = head Board.positions 
  , psResult = Board.Cont ()
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } 

=======
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
  }

-- Initialize the Scrabble game state
initScrabble :: Scrabble
initScrabble = MkScrabble 
  { scrabblePlayer = Player.initPlayer
  , scrabbleBoard  = Board.initBoard
  , scrabblePos    = head Board.boardPositions
  , scrabbleBag    = Bag.initBag
  }

-- Checks if the coordinate (r,c) the same as the current position of the board
isCurr :: Scrabble -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = scrabblePos s 

-- Determines what the next GameState should be based on the current boardState
next :: Scrabble -> Board.Result Board.Board -> Player.Player -> Bag.Bag -> Either Scrabble Scrabble
next s Board.Retry     _ _  = Right s
next s (Board.Cont b') p' bag'  = Right (s { scrabbleBoard = b', scrabblePlayer = p', scrabbleBag = bag'})
next s (Board.End b') p' bag'  = Left (s { scrabbleBoard = b', scrabblePlayer = p', scrabbleBag = bag'})
>>>>>>> 4acd91be8b92353772ab11d14e4f8bd5ff4fe7af
