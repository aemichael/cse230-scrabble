-------------------------------------------------------------------------------
-- This module defines a Rack.
-- A Rack has a list of tiles.
-- You can initialize a rack.
-- You can fill a rack with tiles from the bag.
-- You can check if a tile is in a rack.
-- You can remove a tile from a rack.
-- You can insert a tile in a rack.
-------------------------------------------------------------------------------
module Model.Rack
( 
    -- Types
    Rack

    -- Rack API
    , initRack
    , fillRack
    , isTileInRack
    , removeTileFromRack
    , insertTileIntoRack
)
where

import Model.Bag
import Model.Tile
import Prelude
import Test.QuickCheck

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Rack --------------------------------------------------
-------------------------------------------------------------------------------

-- A rack is an array of seven tiles. A player's rack should only have less
-- seven tiles before the game begins, or when the bag is empty.
type Rack = [Tile]

-- The initialize a Rack
initRack :: Rack
initRack = []

-- Fill a rack with random tiles drawn from the bag.
fillRack :: Rack -> Bag -> IO (Bag, Rack)
fillRack rack bag = do
  let n = min (7 - length rack) (length bag)
  (bag', rack') <- drawN n bag
  return (bag', rack ++ rack')

-- Check if a tile is in a rack
isTileInRack :: Tile -> Rack -> Bool
isTileInRack tile rack = elem tile rack

-- Removes the first occurrence of a tile from a rack.
removeTileFromRack :: Tile -> Rack -> Rack
removeTileFromRack _ [] = []
removeTileFromRack tile (x:xs)
  | x == tile = xs
  | otherwise = x : removeTileFromRack tile xs

-- Insert a tile into a rack
insertTileIntoRack :: Tile -> Rack -> Rack
insertTileIntoRack tile rack = rack ++ [tile]

-------------------------------------------------------------------------------
-- | Tests --------------------------------------------------------------------
-------------------------------------------------------------------------------

genRack :: Gen Rack
genRack = resize 7 $ listOf genTile

-- A refilled rack should always have 7 tiles
prop_rack_refill_size :: Property
prop_rack_refill_size = forAll genRack $ \rack -> ioProperty (checkRefillSize rack)
  where
    checkRefillSize :: Rack -> IO Bool
    checkRefillSize rack = do
      (_, rack') <- fillRack rack initBag
      return $ length rack' == 7

-- >>> quickCheck prop_rack_refill_size
-- +++ OK, passed 100 tests.
--
