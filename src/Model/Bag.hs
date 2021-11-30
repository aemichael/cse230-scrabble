module Model.Bag
  ( -- * Types
    Bag

    -- * Bag operations
  , initBag
  , drawN
  , draw1
  )
  where

import Prelude
import Model.Tile
import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Bag and Tile Withdrawal --------------------------------------------------
-------------------------------------------------------------------------------

type Bag = [TileLetter]

-- | Initialize Scrabble bag with all 100 tiles
initBag :: Bag
initBag = concat
  [ replicate (tileCount x) x
  | x <- Blank : map Letter capitals
  ]

-- | Draw N random tiles from the bag
drawN :: Int -> Bag -> IO (Bag, [TileLetter])
drawN 0 bag = return (bag, [])
drawN n bag = do
  (bag' , x)  <- draw1 bag
  (bag'', xs) <- drawN (n-1) bag'
  return (bag'', x:xs)

draw1 :: Bag -> IO (Bag, TileLetter)
draw1 bag = do
  i <- randomRIO (0, length bag - 1)
  let bag' = take (i - 1) bag ++ drop i bag
  return (bag', bag !! i)



capitals :: [Char]
capitals = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q',
           'R','S','T','U','V','W','X','Y','Z']