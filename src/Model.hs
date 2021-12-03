-------------------------------------------------------------------------------
-- This module defines the main entrypoint into the application.
-------------------------------------------------------------------------------
-- TODO: What is this for?
{-# LANGUAGE RecordWildCards #-}

module Model where 

import qualified Model.Board  as Board
import qualified Model.Player as Player
import           Prelude hiding ((!!))

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
-- TODO: What is this for?
data Tick = Tick

--------------------------------------------------------------------------------
-- | Top-level Game State ------------------------------------------------------
--------------------------------------------------------------------------------
-- A GameState defines the player in the game, the board state, the current cursor
-- position, and the result of the game.
data GameState = MkGameState
  { gsPlayer      :: Player.Player      -- ^ current player
  , gsBoard  :: Board.BoardState      -- ^ current board
  , gsPos    :: Board.BoardPos        -- ^ current cursor
  , gsResult :: Board.Result ()       -- ^ result      
  } 

-- Constant that defines the initial game state
initialGameState :: GameState
initialGameState = MkGameState 
  { gsPlayer      = Player.player1
  , gsBoard  = Board.initialBoardState
  , gsPos    = head Board.boardPositions 
  , gsResult = Board.Cont ()
  }

-- Checks if the coordinate (r,c) the same as the current position of the board
isCurr :: GameState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = gsPos s 

-- Determines what the next GameState should be based on the current boardState
next :: GameState -> Board.Result Board.BoardState -> Either (Board.Result ()) GameState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { gsBoard = b'})
