{-# LANGUAGE RecordWildCards #-}
module Model where 

import qualified Model.Board  as Board
import qualified Model.Player as Player
import           Prelude hiding ((!!))

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play GameState 
  | Outro 
  
data GameState = PS
  { p1      :: Player.Player   -- ^ player X info
  , psBoard  :: Board.BoardState     -- ^ current board
  , psPos    :: Board.BoardPos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
  } 

init :: GameState
init = PS 
  { p1      = Player.human
  , psBoard  = Board.initialBoardState
  , psPos    = head Board.boardPositions 
  , psResult = Board.Cont ()
  }

isCurr :: GameState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: GameState -> Board.Result Board.BoardState -> Either (Board.Result ()) GameState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'})
