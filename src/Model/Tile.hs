module Model.Tile
  (
    TileLetter (..)
  )
  where

import Prelude

-------------------------------------------------------------------------------
-- | Scrabble Tiles -----------------------------------------------------------
------------------------------------B-------------------------------------------

data TileLetter = Blank
                | Letter Char
  deriving (Eq)

instance Show TileLetter where
  show Blank      = " "
  show (Letter x) = [x]

instance Ord TileLetter where
  (<=) Blank      _          = True
  (<=) _          Blank      = False
  (<=) (Letter x) (Letter y) = (x <= y)
