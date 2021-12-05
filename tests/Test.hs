module Main where 

import System.Exit
import qualified Model.Tile as Tile
import qualified Model.Rack as Rack
import Test.QuickCheck

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  mapM quickCheck allTests
  putStrLn "\nDone Testing"
  exitWith ExitSuccess


allTests :: [Property]
allTests = concat
  [ propTile
  , propRack
  ]

propTile :: [Property]
propTile =
  [ Tile.prop_tile_count 
  , Tile.prop_tile_score
  ]

propRack :: [Property]
propRack =
  [ Rack.prop_rack_refill_size
  ]
