-------------------------------------------------------------------------------
-- This module defines a Tile.
-- A tile has a letter or is blank.
-- You can get the score of a tile.
-- You can get the count of a tile.
-------------------------------------------------------------------------------
module Model.Tile
(
  -- Types
  Tile (..)

  -- Constants
  , capitals

  -- Tile API
  , getTileScore
  , getTileCount

  -- Testing
  , genTile
  , genTileAtFrequency
)
where

import qualified Data.Map.Strict as M
import Prelude 
import Test.QuickCheck 

-------------------------------------------------------------------------------
-- | Tile --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- A Tile is either Blank or has a Letter
data Tile = Letter Char deriving (Eq)

-- Define how to show an instance of a Tile
instance Show Tile where
  show (Letter x) = [x]

-- Define how to order an instance of a Tile
instance Ord Tile where
  (<=) (Letter '*')      _          = True
  (<=) _          (Letter '*')      = False
  (<=) (Letter x) (Letter y) = (x <= y)

-- Look up the score of a tile letter
getTileScore :: Tile -> Int
getTileScore = (scores M.!)

-- Look up the count of a tile letter in the game
getTileCount :: Tile -> Int
getTileCount = (counts M.!)

-------------------------------------------------------------------------------
-- | Tile Tests --------------------------------------------------------------------
-------------------------------------------------------------------------------

genTile :: Gen Tile
genTile = do
  t <- elements $ Letter '*' : map Letter capitals
  return t

-- Generate a tile at the expected frequency in normal Scrabble play
genTileAtFrequency :: Gen Tile
genTileAtFrequency = do
  t <- elements $ concat
    [replicate (getTileCount x) x | x <- Letter '*' : map Letter capitals]
  return t


-- Simple sanity checks on getTileScore and getTileCount

prop_tile_score :: Property
prop_tile_score = forAll genTile $
  \tile -> (getTileScore tile == scores M.! tile)

-- >>> quickCheck prop_tile_score
-- +++ OK, passed 100 tests.
--

prop_tile_count :: Property
prop_tile_count = forAll genTile $
  \tile -> (getTileCount tile == counts M.! tile)

-- >>> quickCheck prop_tile_count
-- +++ OK, passed 100 tests.
--


-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Constant list of characters representing all capital letters
capitals :: [Char]
capitals = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q',
           'R','S','T','U','V','W','X','Y','Z']

-- Constant map of Tile to Int containing the score of each Tile in the game
scores :: M.Map Tile Int
scores = M.fromList
  [ (Letter '*', 0 )
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
  [ (Letter '*', 2 )
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
