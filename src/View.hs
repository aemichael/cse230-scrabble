-------------------------------------------------------------------------------
-- This module defines how the view for the application is rendered.
-------------------------------------------------------------------------------
module View
(
  -- View API
  drawUI
) 
where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center
import Data.Char (toUpper)
import Graphics.Vty hiding (dim)

import Model
import Model.Board as Board
import Model.Player as Player
import Model.Tile as Tile

-------------------------------------------------------------------------------
-- Draw UI entrypoint
-------------------------------------------------------------------------------
drawUI :: Model.Scrabble -> [Widget String]
drawUI scrabble = [ (drawBoard scrabble) <+> ((drawPlayer scrabble) <=> (drawBag scrabble)) ]

-------------------------------------------------------------------------------
-- Draw UI for Board
-------------------------------------------------------------------------------
drawBoard :: Model.Scrabble -> Widget String
drawBoard scrabble = 
  withBorderStyle unicode
  $ borderWithLabel (str "Scrabble")
  $ vTile [ mkRow scrabble row | row <- [1..Board.boardDim] ]

-- Creates a row in the board
mkRow :: Model.Scrabble -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..Board.boardDim] ]

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
mkCell :: Model.Scrabble -> Int -> Int -> Widget n
mkCell s r c 
  | Model.isCurr s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

-- Render the cell with the cursor in it in a special ways
withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

-- Returns a cell with a widget corresponding to the tile at this position on
-- the board
mkCell' :: Model.Scrabble -> Int -> Int -> Widget n
mkCell' s r c = center (mkTile xoMb)
  where 
    xoMb      = Board.getTile (scrabbleBoard s) (BoardPos r c)

-- Converts a Tile to a widget
mkTile :: Maybe Tile.Tile -> Widget n
mkTile (Just (Tile.Letter char)) = blockLetter char
mkTile Nothing  = blockBlank

-- Widget for blank tiles
blockBlank :: Widget n
blockBlank = vBox [ str " " ]

-- Widget for a tile with a letter
blockLetter :: Char -> Widget n
blockLetter char = vBox [ str [(toUpper char)] ]

-------------------------------------------------------------------------------
-- Draw UI for Player
-------------------------------------------------------------------------------
drawPlayer :: Model.Scrabble -> Widget String
drawPlayer scrabble = 
  withBorderStyle unicode
  $ borderWithLabel (str playerName)
  $ padTopBottom 1
  $ vBox
  $ map (uncurry drawInfo)[ ("Current Score", playerScore), ("Rack", playerRack) ]
  where
    playerName = plName $ scrabblePlayer scrabble
    playerScore = show $ plScore $ (scrabblePlayer scrabble)
    playerRack = show $ plRack $ (scrabblePlayer scrabble)

drawInfo :: String -> String -> Widget String
drawInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

-------------------------------------------------------------------------------
-- Draw UI for Bag
-------------------------------------------------------------------------------
drawBag :: Model.Scrabble -> Widget String
drawBag scrabble = 
  withBorderStyle unicode
  $ borderWithLabel (str ("Bag"))
  $ padTopBottom 1
  $ padRight Max (padLeft (Pad 1) $ str (bag))
  where
    bag = show $ scrabbleBag scrabble
