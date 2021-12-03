-------------------------------------------------------------------------------
-- This module defines a Tile.
-------------------------------------------------------------------------------
module Model.Tile
(
  -- Types
  Tile (..)
)
where

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
