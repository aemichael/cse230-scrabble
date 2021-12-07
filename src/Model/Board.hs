-------------------------------------------------------------------------------
-- This module defines the Scrabble Board.
-- A Board has a map of board positions to tiles.
-- You can initialize a board.
-- You can get the tile at a board position.
-- You can put a tile at a board position.
-- You can delete a tile at a board position.
-------------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}

module Model.Board 
(
  -- Types
  Board
  , BoardPos (..)
  , Result (..)

  -- Constants
  , boardDim
  , boardPositions

  -- Board API
  , initBoard
  , isOccupied
  , getTile
  , getTileUnsafe
  , putTile
  , deleteTile
  , getAllNeighbors
)
  where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import           Model.Tile 
import           Prelude hiding (init)

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Constant defining the dimension of the board
boardDim :: Int
boardDim = 15

-- | Constant defining a list of all positions on the board
boardPositions :: [BoardPos]
boardPositions = [ BoardPos r c | r <- [1..boardDim], c <- [1..boardDim] ] 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | A Board is a map of positions to Tile instances
type Board = M.Map BoardPos Tile

-- | A BoardPos is a row (1 <= pRow <= dim) and column (1 <= pCol <= dim) pair
data BoardPos = BoardPos 
  { pRow :: Int
  , pCol :: Int
  }
  deriving (Eq, Ord, Show)

-- | A Result is the definition of if a put succeed or fails on the board
data Result a 
  = Retry 
  | End a
  | Cont a
  deriving (Eq, Functor, Show)

-- | Initialize a board
initBoard :: Board
initBoard = M.empty

-- | Determine whether there's a tile at the given position
isOccupied :: Board -> BoardPos -> Bool
isOccupied b pos = Maybe.isJust $ getTile b pos

-- | Gets a Tile on the board at the position
getTile :: Board -> BoardPos -> Maybe Tile 
getTile board pos = M.lookup pos board

-- | Get the tile on the board at this position. Throws an error if the position
-- is unoccupied.
getTileUnsafe :: Board -> BoardPos -> Tile
getTileUnsafe board pos = board M.! pos

-- | Puts a Tile on the board at the position
putTile :: Board -> Tile -> BoardPos -> Result Board
putTile board lett pos = case M.lookup pos board of 
  -- If there is a tile placed at this position, then retry
  Just _  -> Retry
  -- If there is no tile placed at this position, then insert this Tile
  Nothing -> Cont (M.insert pos lett board) 

-- | Delete a tile at a board position
deleteTile :: Board -> BoardPos -> Result Board
deleteTile board boardpos = Cont (M.delete boardpos board)


-- These functions used to calculate complex scores, acccounting for contiguous tiles
-- in cardinal directions from the current play

-- | Get all contiguous occupied positions (\"neighbors\") in cardinal directions
-- from this pos.
-- 
-- By default, we exclude the current position. However, to support double-counting of
-- tiles when they are used to play in both directions (up-down and left-right),
-- we count the current position as its own neighbor whenever it has neighbors in
-- *both* the up-down and left-right directions.
getAllNeighbors :: Board -> BoardPos -> S.Set BoardPos
getAllNeighbors b pos@(BoardPos r c)
  | hasUDNeighbors && hasLRNeighbors
              = S.unions [S.singleton pos, upN, downN, leftN, rightN]
  | otherwise = S.unions [upN, downN, leftN, rightN]
  where
    upN = getUpNeighbors b $ BoardPos (r-1) c
    downN = getDownNeighbors b $ BoardPos (r+1) c
    leftN = getLeftNeighbors b $ BoardPos r (c-1)
    rightN = getRightNeighbors b $ BoardPos r (c+1)

    hasUDNeighbors = (S.size upN + S.size downN) > 0
    hasLRNeighbors = (S.size leftN + S.size rightN) > 0

-- | Get contiguous occupied positions directly up from this pos, including
-- the current position
getUpNeighbors :: Board -> BoardPos -> S.Set BoardPos
getUpNeighbors b pos@(BoardPos r c)
  | r < 0 || not (isOccupied b pos) = S.empty
  | otherwise = S.union sPos (getUpNeighbors b upPos)
  where
    sPos = S.singleton pos
    upPos = BoardPos (r - 1) c

-- | Get contiguous occupied positions directly down from this pos, including
-- the current position
getDownNeighbors :: Board -> BoardPos -> S.Set BoardPos
getDownNeighbors b pos@(BoardPos r c)
  | r == boardDim || not (isOccupied b pos) = S.empty
  | otherwise = S.union sPos (getDownNeighbors b downPos)
  where
    sPos = S.singleton pos
    downPos = BoardPos (r + 1) c

-- | Get contiguous occupied positions directly left of this pos, including
-- the current position
getLeftNeighbors :: Board -> BoardPos -> S.Set BoardPos
getLeftNeighbors b pos@(BoardPos r c)
  | c < 0 || not (isOccupied b pos) = S.empty
  | otherwise = S.union sPos (getLeftNeighbors b leftPos)
  where
    sPos = S.singleton pos
    leftPos = BoardPos r (c - 1)

-- | Get contiguous occupied positions directly right of this pos, including
-- the current position
getRightNeighbors :: Board -> BoardPos -> S.Set BoardPos
getRightNeighbors b pos@(BoardPos r c)
  | c == boardDim || not (isOccupied b pos) = S.empty
  | otherwise = S.union sPos (getRightNeighbors b rightPos)
  where
    sPos = S.singleton pos
    rightPos = BoardPos r (c + 1)
