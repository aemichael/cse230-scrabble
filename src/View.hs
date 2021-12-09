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
import Graphics.Vty hiding (dim)

import Model
import Model.Board as Board
import Model.Bonus as Bonus
import Model.PlayedRack as PlayedRack
import Model.Player as Player
import Model.Tile as Tile

-------------------------------------------------------------------------------
-- Draw UI entrypoint
-------------------------------------------------------------------------------
drawUI :: Model.Scrabble -> [Widget String]
drawUI scrabble = [ (drawBoard scrabble) <+> 
                    ( (drawBag scrabble) <=>
                      (drawCurrentPlayer scrabble) <=>
                      (drawPlayers scrabble)
                    )
                  ]

-------------------------------------------------------------------------------
-- | Draw UI for Board
-------------------------------------------------------------------------------
drawBoard :: Model.Scrabble -> Widget String
drawBoard scrabble = 
  withBorderStyle unicode
  $ borderWithLabel (str "Scrabble")
  $ vTile [ mkRow scrabble row | row <- [1..Board.boardDim] ]

-- | Creates a row in the board
mkRow :: Model.Scrabble -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..Board.boardDim] ]

-- | Creates vertical tiles
vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

-- | Creates horizontal tiles
hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget

-- | Creates a cell for each position in the board. For the current cell that
-- the cursor is in, render it specially.
mkCell :: Model.Scrabble -> Int -> Int -> Widget n
mkCell s r c 
  | Model.isCurrentPos s (r,c) = withCursor raw 
  | otherwise                  = raw 
  where
    raw = mkCell' s r c

-- | Render the cell with the cursor in it in a special ways
withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

-- | Returns a cell with a widget corresponding to the tile at this position on
-- the board
mkCell' :: Model.Scrabble -> Int -> Int -> Widget n
mkCell' s r c = center (mkTile tile bonus)
  where 
    tile      = Board.getTile  (scrabbleBoard s) (BoardPos r c)
    bonus     = Bonus.getBonus (bonusBoard s)    (BoardPos r c)


-- | Converts a Tile to a widget
mkTile :: Maybe Tile.Tile -> Maybe Bonus.Bonus -> Widget n
mkTile (Just tile) _            = blockLetter tile
mkTile _           (Just bonus) = blockBonus bonus
mkTile _           _            = blockBlank

-- | Widget for blank tiles
blockBlank :: Widget n
blockBlank = vBox [ str "   " ]

-- | Widget for unoccupied bonus tiles
blockBonus :: Bonus.Bonus -> Widget n
blockBonus bonus = vBox [ str (show bonus) ]

-- | Widget for a tile with a letter
blockLetter :: Tile.Tile -> Widget n
blockLetter tile = vBox [ str "┌―――┐"
                        , str ("| " ++ show tile ++ " |")
                        , str ("└――" ++ tileScore)
                        ]
  where
    tileScore | getTileScore tile > 9 = show $ getTileScore tile
              | otherwise             = " " ++ (show $ getTileScore tile)

-------------------------------------------------------------------------------
-- | Draw UI for Player
-------------------------------------------------------------------------------

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

drawPlayers :: Model.Scrabble -> Widget String
drawPlayers scrabble = vBox (map (drawPlayer scrabble) playerKeysList)
  where
    playerCount = scrabbleNumPlayers scrabble
    playerKeysList = removeItem (scrabbleCurrPlayerKey scrabble) (take playerCount [0..])

drawPlayer :: Model.Scrabble -> Int -> Widget String
drawPlayer scrabble playerKey = 
  withBorderStyle unicode
  $ borderWithLabel (str playerName)
  $ padTopBottom 1
  $ vBox
  $ map (uncurry drawInfo)
    [ ("Current Score", playerScore)
    ]
  where
    player = getPlayer (scrabblePlayersMap scrabble) playerKey
    playerName = plName $ player
    playerScore = show $ plScore $ player

drawInfo :: String -> String -> Widget String
drawInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

-------------------------------------------------------------------------------
-- | Draw UI for Current Player
-------------------------------------------------------------------------------
drawCurrentPlayer :: Model.Scrabble -> Widget String
drawCurrentPlayer scrabble = 
  withBorderStyle unicode
  $ borderWithLabel (str ("Current Player: " ++ playerName))
  $ padTopBottom 1
  $ vBox
  $ map (uncurry drawInfo)
    [ ("Current Player", playerName)
    , ("Current Score", playerScore)
    , ("Rack", playerRack)
    , ("Played Rack", playerPlayedRack)
    ]
  where
    player = getPlayer (scrabblePlayersMap scrabble) (scrabbleCurrPlayerKey scrabble)
    playerName = plName $ player
    playerScore = show $ plScore $ player
    playerRack = show $ plRack $ player
    playerPlayedRack = show $ PlayedRack.extractTiles $ plPlayedRack $ player

-------------------------------------------------------------------------------
-- | Draw UI for Bag
-------------------------------------------------------------------------------
drawBag :: Model.Scrabble -> Widget String
drawBag scrabble = 
  withBorderStyle unicode
  $ borderWithLabel (str "Bag")
  $ padTopBottom 1
  $ vBox
  $ map (uncurry drawInfo)[ ("Num Tiles Left", numTilesLeft) ]
  where
    numTilesLeft = show $ length $ scrabbleBag scrabble
