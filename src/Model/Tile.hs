module Model.Tile
  ( -- * Types
    TileLetter (..)

    -- * Tile properties
  , tileScore
  , tileCount
  )
  where

import Prelude
import qualified Data.Map.Strict as M 

-------------------------------------------------------------------------------
-- | Scrabble Tiles -----------------------------------------------------------
-------------------------------------------------------------------------------

data TileLetter = Blank
                | Letter Char
  deriving Eq

instance Show TileLetter where
  show Blank      = " "
  show (Letter x) = [x]

instance Ord TileLetter where
  (<=) Blank      _          = True
  (<=) _          Blank      = False
  (<=) (Letter x) (Letter y) = (x <= y)

-- | Look up the score for a tile letter
tileScore :: TileLetter -> Int
tileScore = (scores M.!)

scores :: M.Map TileLetter Int
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

-- | Look up the total number of a tile letter in the game
tileCount :: TileLetter -> Int
tileCount = (counts M.!)

counts :: M.Map TileLetter Int
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
