{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- This module defines a PlayedRack
-- A Rack has a list of tile and board pos pairs.
-- You can initialize a rack.
-- You can check if a tile is in a rack.
-- You can remove a tile from a rack.
-- You can insert a tile in a rack.
-------------------------------------------------------------------------------
module Model.PlayedRack
( 
    -- Types
    PlayedRack

    -- PlayedRack API
    , initPlayedRack
    , isTileInPlayedRack
    , removeTileFromPlayedRack
    , insertTileIntoPlayedRack
    , extractTiles
)
where

import Model.Board
import Model.Tile
import Prelude

-------------------------------------------------------------------------------
-- Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- PlayedRack --------------------------------------------------
-------------------------------------------------------------------------------

-- A played rack is a list of tile an board 
type PlayedRack = [(Tile, BoardPos)]

extractTiles :: PlayedRack -> [Tile]
extractTiles xs = [x | (x,_) <- xs]

-- The initialize a PlayedRack
initPlayedRack :: PlayedRack
initPlayedRack = []

-- Check if a tile is in a rack
isTileInPlayedRack :: (Tile, BoardPos) -> PlayedRack -> Bool
isTileInPlayedRack tile rack = elem tile rack

-- Removes the first occurrence of a tile from a rack.
removeTileFromPlayedRack :: (Tile, BoardPos) -> PlayedRack -> PlayedRack
removeTileFromPlayedRack _ [] = []
removeTileFromPlayedRack pair (x:xs)
  | x == pair = xs
  | otherwise = x : removeTileFromPlayedRack pair xs

-- Insert a tile into a rack
insertTileIntoPlayedRack :: (Tile, BoardPos) -> PlayedRack -> PlayedRack
insertTileIntoPlayedRack tile rack = rack ++ [tile]
