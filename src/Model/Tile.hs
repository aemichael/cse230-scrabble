-------------------------------------------------------------------------------
-- This module defines the Tile class.
-------------------------------------------------------------------------------
module Model.Tile
(
  -- Types
  TileLetter (..)
)
where

-- A TileLetter is either Blank or has a Letter
data TileLetter = Blank
                | Letter Char
                deriving (Eq)

-- Define how to show an instance of a TileLetter
instance Show TileLetter where
  show Blank      = " "
  show (Letter x) = [x]

-- Define how to order an instance of a TileLetter
instance Ord TileLetter where
  (<=) Blank      _          = True
  (<=) _          Blank      = False
  (<=) (Letter x) (Letter y) = (x <= y)
