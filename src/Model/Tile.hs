-------------------------------------------------------------------------------
-- This module defines a Tile.
-------------------------------------------------------------------------------
module Model.Tile
(
  -- Types
  Tile (..)

  -- Constants
  , scores
  , counts

  -- Tile API
  , getTileScore
  , getTileCount
)
where

import qualified Data.Map.Strict as M
import           Prelude 

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Constant map of Tile to Int containing the score of each Tile in the game
scores :: M.Map Tile Int
scores = M.fromList
  [ (Blank     , 0 )
  , (Letter 'A', 1 )
  , (Letter 'B', 3 )
  , (Letter 'C', 3 )
  , (Letter 'D', 2 )
  , (Letter 'E', 1 )
  , (Letter 'F', 4 )
  , (Letter 'G', 2 )
  , (Letter 'H', 4 )
  , (Letter 'I', 1 )
  , (Letter 'J', 8 )
  , (Letter 'K', 5 )
  , (Letter 'L', 1 )
  , (Letter 'M', 3 )
  , (Letter 'N', 1 )
  , (Letter 'O', 1 )
  , (Letter 'P', 3 )
  , (Letter 'Q', 10)
  , (Letter 'R', 1 )
  , (Letter 'S', 1 )
  , (Letter 'T', 1 )
  , (Letter 'U', 1 )
  , (Letter 'V', 4 )
  , (Letter 'W', 4 )
  , (Letter 'X', 8 )
  , (Letter 'Y', 4 )
  , (Letter 'Z', 10)
  ]

-- Constant map of Tile to Int containing the counts of that Tile in the game
counts :: M.Map Tile Int
counts = M.fromList
  [ (Blank     , 2 )
  , (Letter 'A', 9 )
  , (Letter 'B', 2 )
  , (Letter 'C', 2 )
  , (Letter 'D', 4 )
  , (Letter 'E', 12)
  , (Letter 'F', 2 )
  , (Letter 'G', 3 )
  , (Letter 'H', 2 )
  , (Letter 'I', 9 )
  , (Letter 'J', 1 )
  , (Letter 'K', 1 )
  , (Letter 'L', 4 )
  , (Letter 'M', 2 )
  , (Letter 'N', 6 )
  , (Letter 'O', 8 )
  , (Letter 'P', 2 )
  , (Letter 'Q', 1 )
  , (Letter 'R', 6 )
  , (Letter 'S', 4 )
  , (Letter 'T', 6 )
  , (Letter 'U', 4 )
  , (Letter 'V', 2 )
  , (Letter 'W', 2 )
  , (Letter 'X', 1 )
  , (Letter 'Y', 2 )
  , (Letter 'Z', 1 )
  ]

-------------------------------------------------------------------------------
-- | Tile --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- A Tile is either Blank or has a Letter
data Tile = Blank
            | Letter Char
            deriving (Eq)

-- Define how to show an instance of a Tile
instance Show Tile where
  show Blank      = " "
  show (Letter x) = [x]

-- Define how to order an instance of a Tile
instance Ord Tile where
  (<=) Blank      _          = True
  (<=) _          Blank      = False
  (<=) (Letter x) (Letter y) = (x <= y)

-- Look up the score of a tile letter
getTileScore :: Tile -> Int
getTileScore Blank = error "This should not occur"
getTileScore tile = case M.lookup tile scores of 
  -- If Found, return the score of this tile
  Just count  -> count
  -- If Not Found, this should never happen
  Nothing -> error "This should not occur"

-- Look up the count of a tile letter in the game
getTileCount :: Tile -> Int
getTileCount Blank = error "This should not occur"
getTileCount tile = case M.lookup tile counts of 
  -- If Found, return the count of this tile
  Just count  -> count
  -- If Not Found, this should never happen
  Nothing -> error "This should not occur"
