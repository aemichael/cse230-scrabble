-------------------------------------------------------------------------------
-- This module defines a Rack.
-- A Rack has a list of tiles.
-- You can initialize a rack.
-- You can fill a rack with tiles from the bag.
-------------------------------------------------------------------------------
module Model.Rack
( 
    -- Types
    Rack

    -- Rack API
    , initRack
    , fillRack
    , isTileInRack
)
where

import Model.Bag
import Model.Tile
import Prelude

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
fillRack :: Rack -> Bag -> IO (Rack, Bag)
fillRack rack bag = do
  let n = min (7 - length rack) (length bag)
  (rack', bag') <- drawN n bag
  return (rack', bag')

-- Check if a tile is in a rack
isTileInRack :: Tile -> Rack -> Bool
isTileInRack tile rack = elem tile rack
