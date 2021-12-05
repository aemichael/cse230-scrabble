module Main where 

import System.Exit
import qualified Model.Tile as Tile
import qualified Model.Rack as Rack
import Test.QuickCheck

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  mapM runTest allTests
  putStrLn "\nDone Testing"
  exitWith ExitSuccess

runTest :: (String, Property) -> IO ()
runTest (s, prop) = do
  putStr "Running "
  putStr s
  putStrLn " ..."
  quickCheck prop

allTests :: [(String, Property)]
allTests = concat
  [ propTile
  , propRack
  ]

propTile :: [(String, Property)]
propTile =
  [ ("prop_tile_count", Tile.prop_tile_count)
  , ("prop_tile_score", Tile.prop_tile_score)
  ]

propRack :: [(String, Property)]
propRack =
  [ ("prop_rack_refill_size", Rack.prop_rack_refill_size)
  ]
