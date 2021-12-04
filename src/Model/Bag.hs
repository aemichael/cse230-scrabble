-------------------------------------------------------------------------------
-- This module defines the Scrabble Bag.
-- A Bag has a list of tiles.
-- You can initialize a bag.
-- You can draw 1 tile from the bag.
-- You can draw N tiles from the bag.
-- You can check if the bag is empty.
-------------------------------------------------------------------------------
module Model.Bag
( 
    -- Types
    Bag

    -- Constants
    , capitals
    
    -- Bag API
    , initBag
    , drawN
    , draw1
    , isBagEmpty
)
where

import Model.Tile
import Prelude
import System.Random

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Constant list of characters representing all capital letters
capitals :: [Char]
capitals = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q',
           'R','S','T','U','V','W','X','Y','Z']

-------------------------------------------------------------------------------
-- | Bag --------------------------------------------------
-------------------------------------------------------------------------------

-- A Bag is a list of Tiles
type Bag = [Tile]

-- Initialize the Scrabble bag with all 100 tiles
initBag :: Bag
initBag = concat [ replicate (getTileCount x) x | x <- Letter '*' : map Letter capitals ]

-- Draw N random tiles from the bag
drawN :: Int -> Bag -> IO (Bag, [Tile])
drawN 0 bag = return (bag, [])
drawN n bag = do
  (bag' , x)  <- draw1 bag
  (bag'', xs) <- drawN (n-1) bag'
  return (bag'', x:xs)

-- Draw 1 random tile from the bag
draw1 :: Bag -> IO (Bag, Tile)
draw1 bag = do
  i <- randomRIO (0, length bag - 1)
  let bag' = take (i - 1) bag ++ drop i bag
  return (bag', bag !! i)

-- Check if the bag is empty
isBagEmpty :: Bag -> Bool
isBagEmpty bag = (length bag) == 0
