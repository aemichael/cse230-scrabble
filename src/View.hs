-------------------------------------------------------------------------------
-- This module defines how the view for the application is rendered.
-------------------------------------------------------------------------------
module View (view) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.Char (toUpper)
import Graphics.Vty hiding (dim)

import Model
import Model.Board
import Model.Tile
import Text.Printf (printf)

-- Wrapper function for returning a list of widgets
view :: GameState -> [Widget String]
view s = [view' s]

-- Creates a Widget that represents the game board
view' :: GameState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..Model.Board.boardDim] ]

-- Prints the header of the game board
header :: GameState -> String
header s = printf "Scrabble row = %d, col = %d" (pRow p) (pCol p)
  where 
    p    = gsPos s

-- Creates a row in the board
mkRow :: GameState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..Model.Board.boardDim] ]

-- Creates vertical tiles
vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

-- Creates horizontal tiles
hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget

-- Creates a cell for each position in the board. For the current cell that the
-- cursor is in, render it specially.
mkCell :: GameState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

-- Render the cell with the cursor in it in a special ways
withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

-- Returns a cell with a widget corresponding to the tile at this position on
-- the board
mkCell' :: GameState -> Int -> Int -> Widget n
mkCell' s r c = center (mkTile xoMb)
  where 
    xoMb      = get (gsBoard s) (BoardPos r c)

-- Converts a Tile to a widget
mkTile :: Maybe Tile -> Widget n
mkTile (Just (Letter char)) = blockLetter char
mkTile (Just (Blank))  = blockBlank
mkTile Nothing  = blockBlank

-- Widget for blank tiles
blockBlank :: Widget n
blockBlank = vBox [ str " " ]

-- Widget for a tile with a letter
blockLetter :: Char -> Widget n
blockLetter char = vBox [ str [(toUpper char)] ]
