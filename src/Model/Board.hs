-------------------------------------------------------------------------------
-- This module defines the Board class.
-------------------------------------------------------------------------------

-- TODO: What is this for?
{-# LANGUAGE DeriveFunctor #-}

module Model.Board 
(
  -- Types
  ScrabbleBoard
  , Pos (..)
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

import Prelude hiding (init)
import qualified Data.Map as M 
import Model.Tile

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Constant defining the dimension of the board
boardDim :: Int
boardDim = 15

-- Constant defining a list of all positions on the board
boardPositions :: [Pos]
boardPositions = [ Pos r c | r <- [1..boardDim], c <- [1..boardDim] ] 

-- Constant defining the initial board state
initialBoardState :: ScrabbleBoard
initialBoardState = M.empty

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- A ScrabbleBoard is a map of positions to TileLetter instances
type ScrabbleBoard = M.Map Pos TileLetter

-- A Pos is a row (1 <= pRow <= dim) and column (1 <= pCol <= dim) pair
data Pos = Pos 
  { pRow :: Int
  , pCol :: Int
  }
  deriving (Eq, Ord)

-- A Result is a TODO?
data Result a 
  = Retry 
  | Cont a
  deriving (Eq, Functor, Show)

-- Gets a TileLetter on the board at the position
get :: ScrabbleBoard -> Pos -> Maybe TileLetter 
get board pos = M.lookup pos board

-- Puts a TileLetter on the board at the position
put :: ScrabbleBoard -> TileLetter -> Pos -> Result ScrabbleBoard
put board lett pos = case M.lookup pos board of 
  -- If there is a tile placed at this position, then retry
  Just _  -> Retry
  -- If there is no tile placed at this position, then insert this TileLetter
  Nothing -> Cont (M.insert pos lett board) 
