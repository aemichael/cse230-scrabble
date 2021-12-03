-------------------------------------------------------------------------------
-- This module defines the Scrabble Board.
-------------------------------------------------------------------------------
-- TODO: What is this for?
{-# LANGUAGE DeriveFunctor #-}

module Model.Board 
(
  -- Types
  BoardState
  , BoardPos (..)
  , Result (..)

  -- Constants
  , boardDim
  , boardPositions
  , initialBoardState

  -- Board API
  , get
  , put
)
  where

import qualified Data.Map as M
import           Model.Tile 
import           Prelude hiding (init)

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Constant defining the dimension of the board
boardDim :: Int
boardDim = 15

-- Constant defining a list of all positions on the board
boardPositions :: [BoardPos]
boardPositions = [ BoardPos r c | r <- [1..boardDim], c <- [1..boardDim] ] 

-- Constant defining the initial board state
initialBoardState :: BoardState
initialBoardState = M.empty

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- A BoardState is a map of positions to Tile instances
type BoardState = M.Map BoardPos Tile

-- A BoardPos is a row (1 <= pRow <= dim) and column (1 <= pCol <= dim) pair
data BoardPos = BoardPos 
  { pRow :: Int
  , pCol :: Int
  }
  deriving (Eq, Ord)

-- A Result is a TODO?
data Result a 
  = Retry 
  | Cont a
  deriving (Eq, Functor, Show)

-- Gets a Tile on the board at the position
get :: BoardState -> BoardPos -> Maybe Tile 
get board pos = M.lookup pos board

-- Puts a Tile on the board at the position
put :: BoardState -> Tile -> BoardPos -> Result BoardState
put board lett pos = case M.lookup pos board of 
  -- If there is a tile placed at this position, then retry
  Just _  -> Retry
  -- If there is no tile placed at this position, then insert this Tile
  Nothing -> Cont (M.insert pos lett board) 
