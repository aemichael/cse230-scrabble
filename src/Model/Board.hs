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
  , dim
  , positions
  , init

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
dim :: Int
dim = 15

-- Constant defining a list of all positions on the board
positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

-- Constant defining the initial board state
init :: ScrabbleBoard
init = M.empty

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- The ScrabbleBoard data type
type ScrabbleBoard = M.Map Pos TileLetter

-- The Pos data type
data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

-- The Result data type
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
