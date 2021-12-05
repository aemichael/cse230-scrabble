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

-- | A played rack is a list of tiles placed at board positions
type PlayedRack = [(Tile, BoardPos)]

-- | Extracts the list of played tiles.
extractTiles :: PlayedRack -> [Tile]
extractTiles xs = [x | (x,_) <- xs]
-- More verbose implementation blow:
-- extractTiles :: PlayedRack -> [String]
-- extractTiles xs = [ show tile ++ ";" ++ "(" ++ (show rowidx) ++ "," ++ (show colidx) ++ ")"  
--                   | (tile,pos) <- xs, let rowidx = pRow pos, let colidx = pCol pos
--                   ]

-- | Initialize an empty PlayedRack
initPlayedRack :: PlayedRack
initPlayedRack = []

-- | Check if a (tile, pos) is in the played rack
isTileInPlayedRack :: (Tile, BoardPos) -> PlayedRack -> Bool
isTileInPlayedRack tile rack = elem tile rack

-- | Removes the first occurrence of a (tile, pos) from a played rack.
removeTileFromPlayedRack :: (Tile, BoardPos) -> PlayedRack -> PlayedRack
removeTileFromPlayedRack _ [] = []
removeTileFromPlayedRack pair (x:xs)
  | x == pair = xs
  | otherwise = x : removeTileFromPlayedRack pair xs

-- | Insert a (tile, pos) into a played rack
insertTileIntoPlayedRack :: (Tile, BoardPos) -> PlayedRack -> PlayedRack
insertTileIntoPlayedRack tile rack = rack ++ [tile]
