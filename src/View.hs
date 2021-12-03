module View (view) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Graphics.Vty hiding (dim)

import Model
import Model.Board
import Model.Tile
import Text.Printf (printf)

-------------------------------------------------------------------------------
view :: GameState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: GameState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..Model.Board.boardDim] ]

header :: GameState -> String
header s = printf "Scrabble row = %d, col = %d" (pRow p) (pCol p)
  where 
    p    = psPos s

mkRow :: GameState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..Model.Board.boardDim] ]

mkCell :: GameState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: GameState -> Int -> Int -> Widget n
mkCell' s r c = center (mkTileLetter xoMb)
  where 
    xoMb      = get (psBoard s) (BoardPos r c)

mkTileLetter :: Maybe Tile -> Widget n
mkTileLetter (Just (Letter _)) = blockA
mkTileLetter (Just (Blank))  = blockBlank
mkTileLetter Nothing  = blockBlank

blockBlank, blockA :: Widget n
blockBlank = vBox [ str " " ]
blockA = vBox [ str "A" ]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget
