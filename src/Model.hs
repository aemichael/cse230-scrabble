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
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { p1      :: Player.Player   -- ^ player X info
  , psBoard  :: Board.ScrabbleBoard     -- ^ current board
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
  } 

init :: PlayState
init = PS 
  { p1      = Player.human
  , psBoard  = Board.initialBoardState
  , psPos    = head Board.boardPositions 
  , psResult = Board.Cont ()
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.ScrabbleBoard -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'})
